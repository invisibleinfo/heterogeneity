# heterogeneity

Explore whether the effect of an independent variable (W) on a dependent variable (Y) is larger for some people than for others. One R call fits an honest causal forest (via [`grf`](https://grf-labs.github.io/grf/)); printing the result shows the average treatment effect, how much the estimated treatment effects vary across people with different characteristics, which characteristics the forest used most, and how people with smaller and larger estimated treatment effects differ. `effect_by()` then estimates the average treatment effect within groups of interest, in the units of Y. A point-and-click app follows the same steps.

The package is meant for researchers new to causal forests who want **exploratory, descriptive signals of heterogeneity**. You need:

- a **dependent variable (Y)**: binary, with any two values (such as voted or did not vote), or continuous (such as a test score). For a binary Y, effects are differences in the share coded 1;
- an **independent variable (W)**: binary, with any two values (such as treatment and control), or continuous (such as hours of use). A binary W gives treatment effects as **differences** between its two groups, a continuous W gives them as **slopes**, the change in Y per one-unit increase in W;
- plausible **characteristics (X)** that might moderate the effect of W on Y (covariates).

The forest estimates a treatment effect for each combination of characteristics (a conditional average treatment effect, CATE); their average is the average treatment effect (for a numeric W, the average partial effect). Each is an adjusted difference between people with similar characteristics. Whether it is a causal effect depends on the research design, not on the software: it is when W was randomly assigned, or when every common cause of W and Y is among the characteristics; otherwise it is an adjusted association.

## Installation

```r
install.packages("remotes")
remotes::install_github("invisibleinfo/heterogeneity")
```

To install from a local copy of the repository instead, run `install.packages(c("grf", "ggplot2"))` and then `install.packages(".", repos = NULL, type = "source")` in its root folder. The app also needs `shiny`; Excel uploads need `readxl` and SPSS or Stata uploads `haven`. The GSS example is downloaded with [`gssr`](https://kjhealy.github.io/gssr/), which is installed from its own repository:

```r
install.packages(c("shiny", "readxl", "haven"))
install.packages("gssr", repos = c("https://kjhealy.r-universe.dev", "https://cloud.r-project.org"))
```

## Four steps

### 1. Choose variables

Use one row per observation. Choose a dependent variable (Y), an independent variable (W), and characteristics (X) that might make the effect of W larger or smaller. A simulated experiment:

```r
library(heterogeneity)
set.seed(1)
d <- data.frame(
  age = runif(1000, 18, 80),
  educ = sample(8:20, 1000, replace = TRUE),
  programme = rbinom(1000, 1, 0.5)
)
d$score <- 0.1 * d$educ + d$programme * (1 + (d$age > 50)) + rnorm(1000)
d$age[1:50] <- NA
```

### 2. Run the forest

```r
report <- explore_heterogeneity(
  data = d, y = "score", w = "programme", x = c("age", "educ"),
  W_hat = 0.5, effect_label = "Treatment effect on score (points)"
)
report
```

Printing the report shows a summary (the average treatment effect, also in standard deviations for a continuous Y; the smallest and largest estimated treatment effects; the candidate moderators most used in tree splits; and notes on the data preparation) and three plots:

1. **Distribution of estimated treatment effects, conditional on covariates.** A histogram of each observation's out-of-bag estimate of its conditional average treatment effect, with the number of bins set by the Freedman–Diaconis rule. As in the heatmaps, bars are increasingly red further below 0 and increasingly teal further above it. The spread includes estimation noise, so it is not the true distribution of individual effects; the smallest and largest estimates carry the most noise.
2. **Candidate moderators of the treatment effect, ranked by importance.** All characteristics, ranked by how often and how early the forest split on each. Importance makes a characteristic a candidate moderator, not a proven one, and gives neither the size nor the direction of its link with the treatment effect.
3. **Characteristics across treatment effect quintiles.** For every characteristic, in order of importance (a text characteristic gets one row per category), its average in each treatment effect quintile, from the bottom 20% of estimated treatment effects to the top 20% (labelled on top), with its range on the axis. Colours compare each average with the characteristic's overall average, teal above and red below, measured in standard deviations of that characteristic, so characteristics in different units share one colour scale.

`print(report, plots = FALSE)` shows the summary alone, and `plot(report, "distribution")`, `plot(report, "importance")` or `plot(report, "profiles")` draws one plot. With many characteristics, `top_n` limits the last two, for example `plot(report, "importance", top_n = 10)`.

`W_hat` is the known expected value of W: for two groups, the probability of being in the group coded 1. Supply it when it is known from an experiment; otherwise leave it out and the forest estimates it. By default, a two-valued W or Y has its larger (or alphabetically later) value coded 1, so differences are that value of W minus the other; `treated` (for W) and `y_one` (for Y) choose the value coded 1. The default seed (21042020) makes the fit reproducible.

**In standard deviations.** For a continuous Y, the summary also gives the average as a standardized effect size, which can be compared across outcomes measured in different units: the average treatment effect divided by the pooled within-group standard deviation of Y before adjustment, or, for a numeric W, the change in Y (in standard deviations) per standard deviation of W. With weights, the standard deviations are weighted. It is left out for a 0/1 Y, whose differences in proportions are already on a natural scale. Everything else stays in the units of Y: `report$ate$standardized` holds the value and `report$ate$sd_y` the standard deviation used, so dividing by it converts any other result.

### 3. Compare groups

```r
age <- effect_by(report, "age")         # average treatment effect in each age group, with counts
age
plot(age)
plot(effect_by(report, "age", "educ"))  # two characteristics at once: a heatmap
```

This is the focused check: the average treatment effect within groups of a characteristic your research question names, or one the forest used often. Numeric characteristics are cut into quantile bins (five for one characteristic, three each for two). The table includes a "(Missing)" group; the plots leave missing values out.

The overall and group averages are adjusted estimates built from grf's doubly robust scores. The plots in step 2 describe the forest's **estimated conditional treatment effects**. The two can differ: one summarizes groups of people, the other the fitted model. Groups also differ in other ways, so a pattern does not show what would happen if someone's age changed.

### 4. Export results

```r
write.csv(effect_by(report, "age"), "age_groups.csv", row.names = FALSE)
write.csv(report$moderators, "importance.csv", row.names = FALSE)
write.csv(report$cates, "treatment_effects.csv", row.names = FALSE)
```

Report Y and W, the analysis sample, the average difference (in the units of Y and, for a continuous Y, in standard deviations), and the group estimates and counts relevant to your question, as exploratory results.

## A numeric independent variable

When W has more than two numeric values, the same steps give slopes:

```r
d$hours <- runif(1000, 0, 10)
d$wellbeing <- d$hours * (0.2 + 0.3 * (d$educ > 14)) + rnorm(1000)
slopes <- explore_heterogeneity(data = d, y = "wellbeing", w = "hours", x = c("age", "educ"))
slopes                                 # average treatment effect per hour, range of estimated effects, ...
plot(effect_by(slopes, "educ"))        # change in wellbeing per extra hour, by years of education
```

grf estimates a local linear slope for each profile of characteristics, the change in Y per one-unit increase in W, and averages them into the average partial effect. A group in `effect_by()` is estimated when W varies within it.

## The GSS example

```r
gss <- load_gss2024()
report <- explore_heterogeneity(
  data = gss$data, y = "rating_trump", w = "conservative", x = gss$covariates, ids = "id",
  labels = gss$labels, value_labels = gss$value_labels, effect_label = gss$effect_label
)
```

`load_gss2024()` downloads the 2024 General Social Survey with `gssr` (nothing is stored in the package) and prepares the example: the rating of Donald Trump as the dependent variable (Y), conservative self-identification as the independent variable (W), and demographic, socioeconomic, religious, attitudinal and wellbeing characteristics. Each recode is defined by the meaning of the GSS category labels and checked against them, so a changed category in a later release stops the loading with a message naming the item, rather than miscoding it. `gss$codebook` describes every column, generated from the GSS labels. Because NORC revises its releases, rerun the analysis rather than copying numbers from earlier output.

## The app

```r
heterogeneity::run_app()
```

Upload a CSV, TSV, Excel, SPSS (.sav) or Stata (.dta) file, or load the GSS example, which downloads the data and lists the tutorial's choices without filling them in. A small expander lists what the file needs. The app finds the delimiter and text encoding of text files, recognises a missing header row, removes empty rows and columns, names unnamed columns, and uses the variable and value labels of SPSS and Stata files as readable names. Nothing is saved: the file is used only during your session (Shiny deletes its temporary copy when the session ends), and refreshing the page clears everything.

The page names the four steps at the outset, and each step shows all of its output at once. In step 1, once a file is loaded, every field appears together: the dependent variable (Y) and the independent variable (W), each binary (any two values) or continuous, the value coded 1 for a binary Y or W (when its two values are not 0 and 1), the characteristics (X), and optional settings for an ID column, weights, clusters, the number of trees and the random seed. A column chosen for one field disappears from the others. **Continue to step 2: Run the forest** fits the forest and shows the average treatment effect with three plots of the estimated treatment effects; each later Continue button opens the next step: average treatment effects within groups (step 3, starting with the top candidate moderator), then a plain-language summary and each row's estimated treatment effect as a CSV file (step 4). Everything is explained with the numbers from your data. Technical terms and field labels are underlined; hovering over one shows a short explanation. To keep the page uncluttered, longer explanations (what the file needs, notes on the data, how the results were computed, how to read each plot) sit in small expanders. A switch at the top changes between light and dark mode. Changing any input clears the results until you run the forest again. A link at the top of the page leads to this repository.

## Missing values and untidy data

**Missing characteristics are always kept, never filled in.** `grf` splits on missingness directly. This handles the computation; it does not remove bias from data that are missing for systematic reasons. See the [grf guide](https://grf-labs.github.io/grf/articles/grf_guide.html).

- Blank cells, infinite numbers and common missing-value text ("NA", "N/A", "NaN", "NULL", ".", "-") count as missing; text columns that hold only numbers are read as numbers; labels and other attributes (for example from SPSS or Stata files) are dropped, with codes kept as numbers.
- Rows missing Y, W, or a supplied weight or cluster are removed. Original row numbers, or supplied IDs, identify the rows that remain.
- Entirely missing and constant characteristics are left out.
- Text and factor characteristics (up to 20 categories) become 0/1 indicators internally: one per category, or a single one for two categories. Trees need no reference category, and with its own indicator any category can be separated in a single split. Plots use the original categories, and importance is added back up to the original variable.
- Numeric codes are treated as ordered numbers. Convert nominal categories, such as region codes, to text or factors first.
- `report$notes` records every change and `report$missingness` counts the missing values of each characteristic.

`explore_heterogeneity(Y, W, X)` also accepts numeric vectors and a covariate matrix directly.

## Weights, clusters and counts

With the data-frame interface, `ids`, `weights` and `clusters` name columns; with the vector interface they are vectors. Weights apply to the forest and to every displayed average. Clusters identify observations sampled together, such as students in a school, and the forest samples them as groups. Without weights, the results describe the analysis sample.

Treatment effect quintiles contain about equal numbers of rows, with tied estimates kept together. For each group, `effect_by()` returns the average treatment effect `estimate`, the row count `n`, the positive-weight count `n_positive`, for a binary W the counts in each of its groups (`n_control`, `n_treated`), and a `status`. A group is estimated only when it has at least 30 positive-weight observations and includes both groups of W (for a numeric W: more than one value); this is a practical display rule, not a guarantee of a stable estimate. Fitting has no minimum sample size, but the report notes when a group of W (or, for a numeric W, the whole sample) has fewer than 30 rows, because estimates are then very noisy; a forest cannot be grown from only a handful of rows.

## Tutorial

The [tutorial](tutorial/quickstart.md) follows the four steps with the GSS example. It describes adjusted associations and makes no causal claim. For more background see the [grf guide](https://grf-labs.github.io/grf/articles/grf_guide.html) and [Stanford's tutorial on heterogeneous treatment effects](https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/hte-i-binary-treatment.html).

## Development

The package itself needs only `grf` and `ggplot2` (plus base R); the app, the GSS example and the documents use the suggested packages.

| Folder | Contents |
| --- | --- |
| `R/` | Package code, including the GSS example's recodes (`R/gss.R`) |
| `man/` | Help pages, generated from `R/` by roxygen2 |
| `inst/app/` | The Shiny app |
| `tests/` | Package, example and app tests |
| `tutorial/` | Tutorial source, rendered Markdown and figures |
| `tools/` | Script that re-renders the tutorial |

Run the tests with `devtools::test()`. After editing the code or the tutorial, rebuild the rendered tutorial (this needs `knitr`, `rmarkdown`, `gssr`, an internet connection and Pandoc):

```sh
Rscript tools/render_resources.R
```

The script installs the current source into a temporary library, so the tutorial always matches the code and the current GSS release.

## Support

For questions, bug reports or feature requests, please [open an issue](https://github.com/invisibleinfo/heterogeneity/issues).

## License

GNU Affero General Public License, version 3 or later (see [LICENSE](LICENSE)).
