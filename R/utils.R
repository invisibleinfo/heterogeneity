# Internal helpers shared by the report, the plots and the app.

# Column names used inside ggplot2 aesthetics.
utils::globalVariables(c(
  "weight",
  "importance",
  "label",
  "g1",
  "g2",
  "estimate",
  "x_label",
  "x",
  "value",
  "effect"
))

check_integer <- function(x, name, lower = 1L, upper = .Machine$integer.max) {
  if (
    !is.numeric(x) ||
      length(x) != 1L ||
      !is.finite(x) ||
      x != floor(x) ||
      x < lower ||
      x > upper
  ) {
    msg <- sprintf(
      "%s must be a whole number between %s and %s.",
      name,
      lower,
      upper
    )
    stop(msg, call. = FALSE)
  }
}

# TRUE for a single, nonmissing string.
is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

# "a", "a or b", "a, b or c" (or joined with "and").
and_or <- function(x, word = "or") {
  if (length(x) <= 1L) {
    return(paste(x))
  }
  paste(paste(x[-length(x)], collapse = ", "), word, x[length(x)])
}

# Weighted mean over positive weights; NA if any of those values is not finite.
point_average <- function(values, weights) {
  keep <- is.finite(weights) & weights > 0
  if (!any(keep) || any(!is.finite(values[keep]))) {
    return(NA_real_)
  }
  stats::weighted.mean(values[keep], weights[keep])
}

# Weighted standard deviation over positive weights, around the mean of each
# group (one group by default). The factor n / (n - groups) makes equal
# weights give the usual formulas: sd() for one group, the pooled
# within-group SD for two.
spread <- function(values, weights, groups = rep(1L, length(values))) {
  keep <- is.finite(values) & weights > 0
  values <- values[keep]
  weights <- weights[keep]
  groups <- groups[keep]
  n <- length(values)
  k <- length(unique(groups))
  if (n <= k) {
    return(NA_real_)
  }
  means <- stats::ave(values * weights, groups, FUN = sum) /
    stats::ave(weights, groups, FUN = sum)
  sqrt(sum(weights * (values - means)^2) / sum(weights) * n / (n - k))
}

# The average effect in standard deviations of Y, using its spread before
# adjustment: pooled within the two groups of a binary W, or, for a numeric W,
# the change in Y (in SDs) per SD of W. Left out for a two-valued Y, whose
# differences in proportions are already on a natural scale.
standardize <- function(estimate, Y, W, weights, binary) {
  none <- list(standardized = NA_real_, sd_y = NA_real_, sd_w = NA_real_)
  if (!is.finite(estimate) || length(unique(Y)) <= 2L) {
    return(none)
  }
  sd_y <- spread(Y, weights, if (binary) W else rep(1L, length(Y)))
  sd_w <- if (binary) NA_real_ else spread(W, weights)
  value <- if (binary) estimate / sd_y else estimate * sd_w / sd_y
  if (!is.finite(value)) {
    return(none)
  }
  list(standardized = value, sd_y = sd_y, sd_w = sd_w)
}

# IDs as supplied, for the exported table: without labels or classes, with
# factors and dates as text, so that they match the original file.
id_values <- function(v) {
  if (is.factor(v) || inherits(v, c("Date", "POSIXt"))) {
    return(as.character(v))
  }
  if (is.atomic(v)) {
    v <- unclass(v)
    attributes(v) <- NULL
  }
  v
}

# Pairs of numeric columns with |r| at or above the threshold (observed pairs).
highly_correlated <- function(data, threshold = 0.9) {
  d <- as.data.frame(data)
  d <- d[vapply(d, is.numeric, logical(1))]
  d <- d[vapply(
    d,
    function(v) length(unique(v[is.finite(v)])) > 1L,
    logical(1)
  )]
  empty <- data.frame(x = character(), y = character(), r = numeric())
  if (ncol(d) < 2L) {
    return(empty)
  }
  d[] <- lapply(d, function(v) {
    v[!is.finite(v)] <- NA_real_
    v
  })
  correlation <- suppressWarnings(stats::cor(d, use = "pairwise.complete.obs"))
  positions <- which(
    upper.tri(correlation) &
      is.finite(correlation) &
      abs(correlation) >= threshold,
    arr.ind = TRUE
  )
  if (!nrow(positions)) {
    return(empty)
  }
  out <- data.frame(
    x = colnames(correlation)[positions[, 1]],
    y = colnames(correlation)[positions[, 2]],
    r = correlation[positions]
  )
  out[order(-abs(out$r)), , drop = FALSE]
}

# Readable names for covariates and for their category codes.
var_label <- function(report, v) unname(report$labels[v])
level_labels <- function(report, v, levels) {
  labels <- report$settings$value_labels[[v]]
  result <- as.character(levels)
  matched <- match(result, names(labels))
  found <- !is.na(matched)
  result[found] <- unname(labels[matched[found]])
  make.unique(result)
}

# Words for the estimates, which are treatment effects: for a binary W the
# difference between its two groups, for a numeric W the change in Y per unit
# of W (grf's partial effect, a slope).
effect_words <- function(report) {
  s <- report$settings
  if (identical(s$w_type, "continuous")) {
    per <- paste("per unit of", s$comparison)
    list(
      plural = paste("treatment effects", per),
      numeric = TRUE,
      Average = paste(
        "Average treatment effect",
        per,
        "(average partial effect)"
      ),
      effect = paste("change per one-unit increase in", s$comparison)
    )
  } else {
    list(
      plural = "treatment effects",
      numeric = FALSE,
      Average = "Average treatment effect",
      effect = paste(s$w_labels[2], "minus", s$w_labels[1])
    )
  }
}
