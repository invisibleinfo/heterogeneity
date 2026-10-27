# heterogeneity

Explore whether the effect of an independent variable (W) on a dependent variable (Y) is larger for some observations than for others. The package fits a causal forest (via [`grf`](https://grf-labs.github.io/grf/)) and shows the average treatment effect, how much the estimated treatment effects vary, which characteristics the forest used most, and how the treatment effect changes with each of them. It is meant for social science researchers aiming to explore heterogeneity in treatment effects.

You need:

- a **dependent variable (Y)**: binary (any two values) or continuous;
- an **independent variable (W)**: binary (any two values) or continuous;
- **characteristics (X)** that might moderate the effect of W on Y, called candidate moderators, and optionally **controls** to adjust for (`controls =` in R, the Controls field in the app).

## Installation

Install [R](https://cran.r-project.org) (version 4.1 or later), then run in R:

```r
install.packages("pak")
pak::pak(c("invisibleinfo/heterogeneity", "shiny", "readxl", "haven"))
```

The last three packages are only for the app: shiny runs it, and readxl and haven open Excel, SPSS and Stata files.

## Two ways to use it

### 1. With R functions

```r
library(heterogeneity)

# An included example from the 2024 General Social Survey
gss <- load_gss2024()

# Fit the causal forest
report <- explore_heterogeneity(
  data = gss$data,
  y = "rating_trump",
  w = "conservative",
  x = gss$covariates
)

# The average treatment effect, with two plots
report

# How the treatment effect changes with age
plot(effect_by(report, "age"))

# Each row's estimated treatment effect, one row per row of the data
write.csv(report$cates, "treatment_effects.csv", row.names = FALSE)
```

### 2. As a local app

```r
heterogeneity::run_app()
```

Upload a CSV, Excel, SPSS or Stata file, or load the GSS example; choose Y, W and X; and run the causal forest. Every result appears on one page, with short explanations on hover, and you can download each row's estimated treatment effect. The app runs in your own R session, and uploaded files are not stored.

## Learn more

The [quickstart tutorial](tutorial/quickstart.md) walks through the GSS example step by step: how to choose variables, how to read each result, how the averages are computed, and the options for weights, clusters, controls and a continuous W. Each function also has a help page, such as `?explore_heterogeneity`.

## Support

For questions, bug reports or feature requests, please [open an issue](https://github.com/invisibleinfo/heterogeneity/issues).

## License

GNU Affero General Public License, version 3 or later (see [LICENSE](LICENSE)).
