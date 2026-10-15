# heterogeneity

`heterogeneity` provides modern R tools for understanding heterogeneous media effects.

## Overview

The `heterogeneity` package provides contemporary machine learning methods for identifying and characterizing heterogeneous treatment effects in media and communication research. Built on causal forest methodology ([Wager & Athey, 2018](https://doi.org/10.1080/01621459.2017.1319839)), this package enables researchers to move beyond average treatment effects and discover how media effects vary across individuals.

### Why heterogeneity?

Traditional approaches to studying media effects focus on **average treatment effects (ATEs)** - a single number summarizing the average impact of media exposure. However, this average often conceals meaningful variation: some individuals may be strongly influenced by media content while others show minimal response.

`heterogeneity` addresses three key limitations of traditional moderation analysis:

1. The algorithm discovers which variables drive heterogeneity directly from the data
2. Goes beyond simple linear interactions to model complex functional forms
3. Generates predicted treatment effects for each person, not just group averages

## Installation

Install the development version from GitHub:

```r
# Install remotes if needed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install heterogeneity
remotes::install_github("invisibleinfo/heterogeneity")
```

## Core Workflow

The `heterogeneity` package implements a three-stage analytical workflow:

### 1. **Test** for heterogeneity
Formally assess whether treatment effects vary across individuals using the RATE (Rank-Average Treatment Effect) method.

### 2. **Estimate** individual-level effects  
Calculate Conditional Average Treatment Effects (CATEs) for each person using causal forests.

### 3. **Identify** key moderators
Discover which covariates explain the observed heterogeneity through variable importance scores.

## Quick Start Example

```r
library(heterogeneity)
library(tidyverse)

# Prepare your data
# Y: outcome variable (numeric)
# W: treatment indicator (binary: 0/1)
# X: covariate matrix (data frame or matrix)

# Step 1: Test if heterogeneity exists
het_test <- test_heterogeneity(
  Y = outcome,
  W = treatment,
  X = covariates,
  weights = survey_weights  # optional
)

# View AUTOC estimate (>0 indicates heterogeneity)
print(paste("AUTOC:", round(het_test$autoc, 2), 
            "+/-", round(1.96 * het_test$std_err, 2)))

# Plot the targeting curve
plot(het_test$rate)

# Step 2: Estimate individual treatment effects
cate_results <- estimate_cates(
  Y = outcome,
  W = treatment,
  X = covariates,
  ids = participant_ids,  # optional
  weights = survey_weights,  # optional
  num_trees = 2000
)

# View average treatment effect
print(paste("ATE:", round(cate_results$ate, 2)))

# Access individual-level estimates
cate_df <- cate_results$cates
head(cate_df)

# Step 3: Identify which covariates drive heterogeneity
moderators <- identify_moderators(
  causal_forest = cate_results$causal_forest,
  X = covariates,
  top_n = 10  # show top 10 moderators
)

# View variable importance
print(moderators$variable_importance)
```

## Main Functions

### Core Analysis Functions

#### `test_heterogeneity()`

Tests whether treatment effects vary across individuals using the RATE method.

**Arguments:**
- `Y`: Outcome variable (numeric vector)
- `W`: Treatment variable (binary: 0/1)
- `X`: Covariate matrix (data frame or matrix)
- `weights`: Optional survey weights (numeric vector)
- `seed`: Random seed for reproducibility (default: 27011990)

**Returns:**
- `rate`: RATE object from grf package
- `autoc`: AUTOC estimate (Area Under the Targeting Operator Characteristic)
- `std_err`: Standard error of AUTOC
- `ci_lower`, `ci_upper`: 95% confidence interval bounds
- `train_forest`, `eval_forest`: Fitted causal forest objects

**Interpretation:**
- AUTOC > 0 with statistical significance indicates meaningful heterogeneity
- The targeting curve lying above the horizontal axis suggests the model successfully ranks individuals by treatment effect magnitude

---

#### `estimate_cates()`

Estimates Conditional Average Treatment Effects (CATEs) for each individual using a causal forest.

**Arguments:**
- `Y`: Outcome variable (numeric vector)
- `W`: Treatment variable (binary: 0/1)
- `X`: Covariate matrix (data frame or matrix)
- `ids`: Optional ID vector for observations
- `weights`: Optional survey weights (numeric vector)
- `num_trees`: Number of trees in the causal forest (default: 2000)
- `seed`: Random seed for reproducibility (default: 27011990)

**Returns:**
- `causal_forest`: Fitted causal forest object
- `cates`: Data frame with columns:
  - `treatment_effect`: Individual-level treatment effect estimate
  - `standard_error`: Standard error of the estimate
  - `ci_lower`, `ci_upper`: 95% confidence interval bounds
  - `quintile`: Effect quintile (1 = lowest, 5 = highest)
- `ate`: Average treatment effect
- `ate_se`: Standard error of ATE

---

#### `identify_moderators()`

Calculates variable importance scores to identify which covariates drive heterogeneity.

**Arguments:**
- `causal_forest`: Fitted causal forest object from `estimate_cates()`
- `X`: Covariate matrix used in fitting the forest
- `top_n`: Optional number of top variables to return (NULL = all)

**Returns:**
- `variable_importance`: Data frame with columns:
  - `variable`: Variable name
  - `importance`: Importance score (higher = more important)
  - `rank`: Importance rank
- `ranked_vars`: Numeric vector of ranked variable indices
- `top_variables`: Character vector of top variable names

---

### Visualization Functions

#### `plot_treatment_effect_histogram()`

Creates a histogram showing the distribution of individual treatment effects, colored by quintile.

**Arguments:**
- `df_cate`: Data frame with `treatment_effect` and `quintile` columns
- `ate`: Optional ATE value to show as reference line

**Returns:** ggplot2 object

---

#### `plot_treatment_effect_violin()`

Creates violin plots showing the distribution of treatment effects within each quintile.

**Arguments:**
- `df_cate`: Data frame with `treatment_effect` and `quintile` columns
- `ate`: Optional ATE value to show as reference line

**Returns:** ggplot2 object

---

#### `plot_moderator_heatmap()`

Creates a heatmap showing standardized mean values of top moderators across treatment effect quintiles.

**Arguments:**
- `df_cate`: Data frame with `quintile` column and moderator variables
- `top_variables`: Character vector of variable names to include

**Returns:** ggplot2 object

**Interpretation:** 
- Green cells indicate above-average values of a covariate in that quintile
- Red cells indicate below-average values
- Helps visualize which characteristics are associated with larger/smaller treatment effects

---

### Utility Functions

#### `highly_correlated()`

Identifies pairs of variables with correlations above a threshold.

**Arguments:**
- `data`: Data frame containing numeric variables
- `threshold`: Minimum absolute correlation to report (default: 0.7)

**Returns:** Tibble with columns:
- `x`, `y`: Variable names
- `r`: Pearson correlation coefficient

**Use case:** Pre-analysis check to identify potential collinearity issues before fitting models.

---

## Extended Example: Analyzing Media Effects

Here's a complete example analyzing how political conservatism affects support for a candidate:

```r
library(heterogeneity)
library(tidyverse)

# Assume we have survey data with:
# - trump_rating: 0-100 rating of Donald Trump
# - conservative: binary indicator (1 = conservative, 0 = not)
# - Multiple demographic and attitudinal covariates

# Pre-analysis: Check for multicollinearity
highly_correlated(survey_data, threshold = 0.7)

# Define variables
Y <- survey_data$trump_rating
W <- survey_data$conservative
X <- survey_data %>% 
  select(age, education, income, political_engagement, 
         media_use, trust_media, # ... other covariates
  )

# STEP 1: Test for heterogeneity
het_test <- test_heterogeneity(Y, W, X, 
                                weights = survey_data$weights)

# Check AUTOC
autoc_estimate <- het_test$autoc
autoc_ci <- 1.96 * het_test$std_err
cat(sprintf("AUTOC: %.3f (95%% CI: %.3f to %.3f)\n",
            autoc_estimate, 
            autoc_estimate - autoc_ci,
            autoc_estimate + autoc_ci))

# Plot targeting curve
plot(het_test$rate, 
     main = "Targeting Operator Characteristic Curve")

# If AUTOC > 0 and significant, proceed to estimation

# STEP 2: Estimate individual-level effects
cate_results <- estimate_cates(
  Y, W, X,
  ids = survey_data$id,
  weights = survey_data$weights,
  num_trees = 2000
)

# View ATE
cat(sprintf("Average Treatment Effect: %.2f +/- %.2f\n",
            cate_results$ate,
            1.96 * cate_results$ate_se))

# Merge CATEs back into main data
survey_data <- survey_data %>%
  left_join(cate_results$cates, by = c("id" = "id"))

# Visualize distribution of effects
plot_treatment_effect_histogram(survey_data, 
                                 ate = cate_results$ate) +
  labs(title = "Heterogeneity in Conservative Effect on Trump Support")

plot_treatment_effect_violin(survey_data,
                              ate = cate_results$ate) +
  labs(title = "Treatment Effects by Quintile")

# STEP 3: Identify key moderators
moderators <- identify_moderators(
  cate_results$causal_forest,
  X,
  top_n = 10
)

# Display top moderators
print(moderators$variable_importance)

# Visualize how moderators vary across quintiles
plot_moderator_heatmap(survey_data, 
                       moderators$top_variables) +
  labs(title = "Characteristics of High vs. Low Effect Individuals")

# VERIFICATION: Compare with traditional regression
# Test top moderator using interaction term
lm(trump_rating ~ conservative * political_engagement + 
     age + education + income,
   data = survey_data,
   weights = weights) %>%
  summary()
```

## Methodological Details

### The Honesty Principle

Causal forests implement the **honesty principle** to ensure valid statistical inference:

1. Each tree randomly splits data into two subsamples
2. **Splitting sample**: Used to determine tree structure (which variables to split on)
3. **Estimation sample**: Used to calculate treatment effects within each leaf

This separation prevents overfitting and ensures that confidence intervals and hypothesis tests are valid.

### What is AUTOC?

The **Area Under the Targeting Operator Characteristic (AUTOC)** quantifies the degree of treatment effect heterogeneity:

- **AUTOC = 0**: No heterogeneity (all individuals have the same treatment effect)
- **AUTOC > 0**: Positive heterogeneity (some individuals benefit more than others)
- **AUTOC < 0**: Negative heterogeneity (some individuals are harmed while others benefit)

A statistically significant AUTOC indicates that the causal forest successfully identifies subgroups with different treatment effects.

### Variable Importance

Variable importance scores indicate how much each covariate contributes to explaining treatment effect heterogeneity:

- **Higher scores** = More important for distinguishing high-effect from low-effect individuals
- **Lower scores** = Less relevant for explaining effect variation

These scores guide researchers toward the most important moderators without requiring prior specification.

## Interpretation Guidelines

### When to use this package

✅ **Use `heterogeneity` when:**
- You want to explore which factors moderate treatment effects
- You have many potential moderators and don't want to pre-specify all interactions
- You suspect nonlinear or complex moderation patterns
- You want individual-level treatment effect estimates
- You need a formal test for whether heterogeneity exists

❌ **Traditional methods may suffice when:**
- You have strong theory about specific moderators
- You only need to test a few pre-specified interactions
- Your sample size is too small (< 100 observations)
- Interpretability of exact functional form is critical

### Verification and Triangulation

We recommend using `heterogeneity` **alongside** traditional methods:

1. Use causal forests to discover important moderators
2. Verify findings using traditional regression with interaction terms
3. Create subgroup analyses to validate patterns
4. Report both exploratory (causal forest) and confirmatory (regression) results

This triangulation strengthens the credibility of findings.

## Dependencies

The package builds on these R packages:

- `grf` - Generalized Random Forests for causal inference
- `tidyverse` - Data manipulation and visualization


## Support

For questions, bug reports, or feature requests, please [open an issue](https://github.com/invisibleinfo/heterogeneity/issues) on GitHub.

---

**Note:** This package is currently under development.
