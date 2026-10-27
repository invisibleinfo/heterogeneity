Quickstart: explore differences beyond the average
================

This tutorial shows how to explore whether the effect of one variable on
another is larger for some people than for others. You need three
things:

- a **dependent variable (Y)**, the variable you want to explain:
  binary, with any two values (such as voted or did not vote), or
  continuous (such as a test score);
- an **independent variable (W)**, whose effect on Y you want to study:
  binary, with any two values (such as treated or untreated), or
  continuous (such as hours of use) (see [5. Other
  analyses](#5-other-analyses));
- **characteristics (X)** that might moderate the effect, that is, make
  it larger or smaller.

A causal forest estimates the effect of W for people with different
characteristics and points to patterns worth investigating.

The example uses the 2024 General Social Survey (GSS). Y is a rating of
Donald Trump from 0 to 100. W compares respondents who identify as
conservative with those who identify as liberal or moderate. Political
identification was not randomly assigned, so we read the results as
**adjusted associations**, not causal effects.

The package can be used in two ways: with its R functions, as in this
tutorial, or as a local app that does the same on one page without code
(see [6. The app](#6-the-app)). The tutorial has four steps: choose
variables, run the forest, look at the treatment effect by candidate
moderator, and export the results. Sections 5 and 6 cover other analyses
and the app.

## 1. Choose variables

Install the package as described in the
[README](../README.md#installation). Then load the package and the
example, which ships with it:

``` r
library(heterogeneity)
gss <- load_gss2024()
```

`load_gss2024()` reads an extract of NORC’s complete 2024 release,
prepared with every recode checked against the GSS category labels.
`gss$data` has one row for each of the 3,721 respondents with a value
for both Y and W. `gss$covariates` names 39 demographic, socioeconomic,
religious, attitudinal and wellbeing characteristics, and `gss$codebook`
describes every column, with its role, GSS source item and notes. The
file also holds further dependent and independent variables for other
analyses, such as ratings of Kamala Harris and of the two parties, and a
college degree:

``` r
gss$codebook[
  gss$codebook$variable %in%
    c("rating_trump", "conservative", "age", "lkelyvot"),
  c("variable", "label", "coding")
]
```

| variable | label | coding |
|:---|:---|:---|
| rating_trump | Rating of Donald Trump | Numeric, 0 to 100 |
| conservative | Conservative | 1 = slightly conservative / conservative / extremely conservative; 0 = extremely liberal / liberal / slightly liberal / moderate, middle of the road |
| age | Age | Numeric, 18 to 89 |
| lkelyvot | Likelihood of voting | 1 = Not at all likely, 2 = Not very likely, 3 = Somewhat likely, 4 = Very likely, 5 = Extremely likely |

The notes matter for some characteristics: for example, the likelihood
of voting was not asked of respondents who were not U.S. citizens, so
they are missing on it.

Choose characteristics that make sense for your question. If you want to
read the results causally, use only characteristics that W could not
have changed.

**How the data are prepared.** `explore_heterogeneity()` tidies the data
before fitting, and the report’s notes list every change:

- blank cells, infinite numbers and common missing-value text (“NA”,
  “N/A”, “NaN”, “NULL”, “.”, “-”) count as missing, and text columns
  that hold only numbers are read as numbers;
- rows missing Y or W (or a supplied weight or cluster) are left out of
  the analysis;
- characteristics that are entirely missing or constant are left out;
- text characteristics (up to 20 categories) become 0/1 indicators, one
  per category, so any category can be split off; their importance is
  added back up afterwards;
- numbers are treated as ordered values, so turn unordered codes (such
  as region codes) into text first;
- the notes also flag characteristics with more than half of their
  values missing, pairs of characteristics correlated at \|r\| ≥ 0.9
  (which share importance), and groups of W with fewer than 30 rows
  (whose estimates are very noisy).

**Keep missing characteristics.** The forest handles missing values
directly, so nothing needs to be filled in. Here the characteristic
missing most often is “Father’s education”, for 20.1% of respondents.
This avoids inventing values, but it does not remove bias when values
are missing for systematic reasons. Rows missing Y or W (or a supplied
weight or cluster) are left out, and the report lists every such change.

## 2. Run the forest

One call fits the forest, and printing the report shows a summary and
draws two plots. The labels are optional; here they come from the
example.

``` r
report <- explore_heterogeneity(
  data = gss$data,
  y = "rating_trump",
  w = "conservative",
  x = gss$covariates,
  ids = "id",
  labels = gss$labels,
  value_labels = gss$value_labels,
  effect_label = gss$effect_label
)
report
```

    ## Exploratory heterogeneity report
    ##   3,721 observations; 39 characteristics; 2,000 trees
    ##   Effect of conservative: Conservative minus Liberal/moderate
    ##   Average treatment effect: 30.28
    ##   In standard deviations: 0.95 (SD of rating_trump within the two groups: 32.04)
    ##   Estimated treatment effects range from 6.68 to 51.26
    ##   Results describe the analysis sample without weighting.
    ## 
    ## Candidate moderators (most used in tree splits):
    ##        characteristic importance rank
    ##  Likelihood of voting     0.4350    1
    ##         Family income     0.1200    2
    ##                 White     0.0878    3
    ##    Years of education     0.0575    4
    ##                   Age     0.0454    5
    ## 
    ## Data notes:
    ##   - Text characteristics became 0/1 indicators, one per category (a single one for
    ##     two categories), with missing entries kept missing: region.
    ## 
    ## See how the treatment effect varies with a candidate moderator:
    ## plot(effect_by(report, "lkelyvot")). Estimated treatment effects describe people
    ## with similar characteristics and include noise; a causal interpretation depends on
    ## the research design.

**The summary.** The average treatment effect is **30.3 rating points**:
on average, conservative respondents rate Trump 30.3 points higher than
liberal/moderate respondents with similar characteristics. “Treatment
effect” is the standard name for this quantity; because W was not
randomly assigned here, it is an adjusted association rather than a
causal effect. It also differs from simply subtracting the two group
means, because it compares people with similar characteristics.

In standard deviations the average is **0.95**: the difference divided
by how much ratings vary within the two groups, before adjustment (one
standard deviation is 32.0 points). This standardized effect size can be
compared across dependent variables measured in different units.
Everything else stays in rating points; dividing by `report$ate$sd_y`
converts any of it.

The forest also estimates a treatment effect for each combination of
characteristics (a conditional average treatment effect, CATE); these
range from 6.7 to 51.3 points. They come from 2,000 honest trees: each
tree uses one part of the data to choose its splits and another part to
estimate the effects, and each respondent’s estimate comes only from
trees grown without them (“out-of-bag”). Every average in this tutorial,
overall, in a group or along a characteristic, uses doubly robust
scores, which combine the estimates with the observed ratings; grf
recommends this over averaging the estimates themselves. The candidate
moderators are the characteristics the forest used most in its splits,
and the data notes list everything that was changed or left out.

Below, the two plots are shown one at a time, each with its explanation.
`plot(report)` and `plot(report, "importance")` draw them on their own,
for example to save one.

``` r
plot(report)
```

![](quickstart_files/figure-gfm/plot-1-1.png)<!-- -->

**Plot 1: distribution of estimated treatment effects, conditional on
covariates.** The histogram shows each respondent’s estimated treatment
effect, with the number of bins chosen from the data (the
Freedman–Diaconis rule). The colours deepen with distance from 0, which
the dashed line marks, and the solid line marks the average treatment
effect, with its value. Teal bars, above 0, are estimates in which
conservative respondents rate Trump higher than liberal/moderate
respondents with similar characteristics, and the further above 0, the
deeper the teal; red bars, below 0, would show the reverse. Here every
estimated treatment effect is above 0. These are model estimates: part
of their spread is estimation noise, so the plot is not the true
distribution of individual effects, and the lowest and highest estimates
are the least reliable.

``` r
plot(report, "importance")
```

![](quickstart_files/figure-gfm/plot-2-1.png)<!-- -->

**Plot 2: candidate moderators of the treatment effect, ranked by
importance.** The chart ranks all 39 characteristics. The top-ranked one
is **Likelihood of voting**. Importance measures how often, and how
early, the forest split on a characteristic. That makes it a candidate
moderator, not a proven one: importance does not say whether the
treatment effect rises or falls with the characteristic, and related
characteristics share importance. For a text characteristic, the
importance of its indicators is added together.

A few options are worth knowing:

- `W_hat`: in a randomized experiment, the known probability of being in
  the group coded 1, such as `W_hat = 0.5`; otherwise, as here, the
  forest estimates it.
- `treated` and `y_one`: for a binary W or Y, the value coded 1; by
  default the larger (or alphabetically later) value, so effects are
  that value minus the other.
- `weights` and `clusters`: survey weights apply to the forest and to
  every average; clusters, such as students in the same school, are
  sampled together, while each observation still counts once in
  averages. Here `weights = "svy_weight"` would describe U.S. adults.
  This illustration is unweighted, so it describes these respondents,
  who include an oversample of Black, Hispanic, and Asian American,
  Native Hawaiian and Pacific Islander respondents.
- `controls`: characteristics to adjust for but not to study as
  moderators, such as common causes of W and Y. The forest uses them
  like every other characteristic, so each estimated treatment effect
  accounts for them, but the importance ranking and the plots leave them
  out.
- `seed`: the default (21042020) makes the results reproducible.

`print(report, plots = FALSE)` shows the summary without the plots, and
with many characteristics `top_n` limits the importance plot, for
example `plot(report, "importance", top_n = 10)`.

## 3. Treatment effect by candidate moderator

`effect_by()` shows how the treatment effect varies with one
characteristic: it estimates the average treatment effect within the
characteristic’s groups, and `plot()` draws the result. The app shows
this plot for every candidate moderator, in order of importance; in R,
`for (v in report$moderators$variable) print(plot(effect_by(report, v)))`
does the same. We start with likelihood of voting, which ranks 1 of 39
in importance.

``` r
voting <- effect_by(report, "lkelyvot")
voting
```

    ##    lkelyvot    n n_positive n_control n_treated   estimate status
    ## 1         1  283        283       211        72  0.9677679     ok
    ## 2         2  144        144       109        35  1.8373110     ok
    ## 3         3  333        333       248        85 22.6342198     ok
    ## 4         4  443        443       310       133 20.1101005     ok
    ## 5         5 2054       2054      1318       736 41.9930771     ok
    ## 6 (Missing)  464        464       338       126 20.3667390     ok

Each row is a group of respondents, labelled by its code (the codebook
above gives the meaning). `n_control` counts the liberal/moderate
respondents in the group and `n_treated` the conservative ones. A group
with fewer than 30 respondents, or with only one group of W, gets no
estimate, and its `status` says why. The average treatment effect is 1.0
points among respondents who are not at all likely to vote and 42.0
among those who are extremely likely. The table includes respondents
with a missing answer as a group of their own; the plot leaves them out.
The plot shows each respondent’s estimated treatment effect, with a line
for the average treatment effect across the five categories. With so few
values the line is smoothed widely, so it shows the trend rather than
each category’s own average, which the table gives. Its y axis is the
treatment effect, a dashed line marks 0, and the caption gives the
overall average treatment effect:

``` r
plot(voting)
```

![](quickstart_files/figure-gfm/voting-plot-1.png)<!-- -->

These groups also differ in other ways, so the plot does not show what
would happen if someone’s likelihood of voting changed. Check
characteristics your theory names as well as the top-ranked ones:

``` r
age <- effect_by(report, "age")
plot(age)
```

![](quickstart_files/figure-gfm/age-plot-1.png)<!-- -->

For a numeric characteristic such as age, each point is one respondent’s
estimated treatment effect, and the line is the average treatment effect
along age: a loess smooth (the line `geom_smooth(method = "loess")`
would draw) of the same doubly robust scores behind every average. The
line is lowest near age 29 (about 24.9 points) and highest near age 79
(about 39.7 points). The table `age` gives the average treatment effect
in up to five age groups of about equal size: 25.9 points for ages 18 -
32 and 41.1 for ages 68 - 89. The line rises and falls more than the
points do, because each estimated treatment effect is pulled toward the
overall average. Both describe people of different ages, not the effect
of growing older, and the group boundaries are not thresholds.

A text characteristic, such as region, has no order, so its plot shows
the average treatment effect in each group as a point instead of a line.

**How the averages are computed.** Every average in these plots and
tables, like the overall average treatment effect, comes from grf’s
doubly robust scores rather than from the estimated treatment effects
themselves, which are pulled toward the overall average; a group’s
average equals grf’s own estimate for that group. The package does not
form groups from the estimated treatment effects (such as their top and
bottom fifths), because grf warns that such groups give misleading
averages unless a separate sample is used to evaluate them.

Two characteristics give a heatmap (in the app, choose a second
characteristic above any plot):

``` r
two <- effect_by(report, "lkelyvot", "income16")
plot(two)
```

![](quickstart_files/figure-gfm/two-way-1.png)<!-- -->

With two characteristics, numeric ones with more than eight values are
cut into three groups of about equal size, each characteristic on its
own, at its own thirds. Tied values stay in one group, so the groups can
be unequal: income, recorded in 26 ordered categories rather than dollar
amounts, splits into 1 - 18 (1,309 respondents), 19 - 22 (1,157
respondents), 23 - 26 (908 respondents). Each cell shows an average
treatment effect and its number of respondents, coloured around the
overall average treatment effect, as the legend’s “Treatment effect”
scale shows. Grey cells have too few respondents, or only one group of
W, to be estimated. The heatmap leaves out respondents missing either
characteristic; the table from
`effect_by(report, "lkelyvot", "income16")` includes them.

## 4. Export results

Save each respondent’s estimated treatment effect, the group estimates
and the importance ranking as ordinary CSV files:

``` r
write.csv(report$cates, "treatment_effects.csv", row.names = FALSE)
write.csv(effect_by(report, "age"), "age_groups.csv", row.names = FALSE)
write.csv(report$moderators, "importance.csv", row.names = FALSE)
```

`report$cates` has one row per row of `gss$data`, in the same order: the
row number, the respondent’s `id` and the estimated treatment effect,
which is empty for any row that was not analysed (here, every respondent
was analysed). It can therefore be put next to the data directly.

A short report can say:

> Among 3,721 GSS respondents, those identifying as conservative rated
> Trump 30.3 points (0.95 standard deviations) higher, on average, than
> liberal or moderate respondents with similar characteristics. The
> characteristics the forest used most were “Likelihood of voting”,
> “Family income” and “White”. Across voting-likelihood groups, the
> average treatment effect ranged from 1.0 to 42.0 points. The analysis
> was unweighted, and missing characteristics were kept for the forest
> rather than filled in. These patterns are exploratory: estimated
> treatment effects describe people with similar characteristics and
> include noise.

Add the group estimates and counts relevant to your question. A causal
reading needs a defensible research design, comparable groups and
suitable characteristics; the GSS example makes no causal claim.

## 5. Other analyses

The example file also holds further dependent variables (ratings of
Kamala Harris, Joe Biden, the two parties and MAGA Republicans, the
rating of the economy and the preferred immigration level) and a further
independent variable (a college degree); `gss$codebook` lists each
column’s role. The same functions handle controls, a binary Y and a
continuous W:

``` r
# Controls: adjust for sex and age without studying them as moderators.
adjusted <- explore_heterogeneity(
  data = gss$data,
  y = "rating_trump",
  w = "conservative",
  x = setdiff(gss$covariates, c("female", "age")),
  controls = c("female", "age")
)
# A binary Y: effects are differences in the share coded 1
# (here, being married).
married <- explore_heterogeneity(
  data = gss$data,
  y = "married",
  w = "college",
  x = setdiff(gss$covariates, c("married", "degree", "educ"))
)
# A continuous W: effects are slopes, the change in Y per one-unit
# increase in W.
slopes <- explore_heterogeneity(
  data = gss$data,
  y = "rating_trump",
  w = "educ",
  x = setdiff(gss$covariates, c("educ", "degree"))
)
```

With controls, the printed summary counts them separately (“37 candidate
moderators and 2 controls”), and they appear in neither the importance
ranking nor `effect_by()`, just like the app’s Controls field. Leave out
of X any characteristic that measures the same thing as Y or W, as
degree and years of education do for a college degree. For a continuous
W, grf estimates a local slope for each combination of characteristics
and averages them into the average partial effect; a group in
`effect_by()` gets an estimate only when W varies within it.

## 6. The app

`heterogeneity::run_app()` opens the same analysis as a local app in
your browser, without code. Upload a CSV, TSV, Excel, SPSS (.sav) or
Stata (.dta) file, or load the GSS example. The app finds the delimiter
and text encoding of text files, recognises a missing header row and
uses SPSS and Stata labels as readable names. It shows the first 10
rows, then every field at once: Y, W, candidate moderators, optional
controls, and default parameters (an ID column, weights, clusters, the
number of trees and the seed). **Select all** chooses the remaining
columns as candidate moderators.

**Run the causal forest** shows all results on one page once they are
ready: the average treatment effect, the two plots of step 2, a plot for
each candidate moderator in order of importance (each with an optional
second characteristic for a heatmap), and the export, a file with one
row per row of your data in the same order. Hovering over an underlined
term explains it, and changing any input clears the results until you
run the forest again. The app runs in your own R session; uploaded files
are not stored and are deleted when the session ends.

## More background

The [grf guide](https://grf-labs.github.io/grf/articles/grf_guide.html)
explains the underlying method and its handling of missing values.

    ##       Component Version
    ## 1             R   4.5.3
    ## 2 heterogeneity   0.2.0
    ## 3           grf   2.5.0
