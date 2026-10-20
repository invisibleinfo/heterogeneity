#' Adjusted differences (or slopes) within groups
#'
#' Groups are formed from one or two of the report's covariates. Numeric
#' covariates with many values are cut into quantile bins (ties can reduce
#' the number of bins); missing values form their own "(Missing)" group in the
#' table. Each estimate averages grf's doubly robust scores within the group,
#' using any supplied weights, without refitting the forest: a difference
#' between the two groups of `W`, or, for a numeric `W`, a slope (change in
#' `Y` per one-unit increase in `W`). Groups
#' contain different people, so a pattern does not show what would happen if
#' the covariate changed.
#'
#' Pass the result to [plot()][plot.heterogeneity_groups]: one covariate gives
#' a dot plot, two give a heatmap (without the missing-value groups).
#'
#' @param report A report from [explore_heterogeneity()].
#' @param x1,x2 Names of one or two covariates; `x2` is optional.
#' @param bins Number of quantile bins for numeric covariates (default 5 for
#'   one covariate, 3 for two).
#' @param max_levels Numeric covariates with at most this many observed values
#'   are grouped by value (default 8). Text categories keep their levels.
#' @param min_n Minimum number of observations (with positive weight) for a
#'   group to be estimated (default 30). Both groups of `W` must be present
#'   (for a numeric `W`: more than one value).
#' @return A data frame (class `heterogeneity_groups`) with one row per group:
#'   the group columns, `n` (rows), `n_positive` (rows with positive weight),
#'   for a binary `W` `n_control` and `n_treated` (positive-weight
#'   rows in each), `estimate` and `status` (why an estimate is missing, or
#'   `"ok"`).
#' @examples
#' set.seed(1)
#' d <- data.frame(age = runif(300, 18, 80), group = rbinom(300, 1, .5))
#' d$score <- d$group * (1 + (d$age > 50)) + rnorm(300)
#' report <- explore_heterogeneity(data = d, y = "score", w = "group",
#'                                 x = "age", W_hat = .5, num_trees = 200)
#' groups <- effect_by(report, "age")
#' groups
#' plot(groups)
#' @export
effect_by <- function(report, x1, x2 = NULL, bins = if (is.null(x2)) 5 else 3, max_levels = 8, min_n = 30) {
  if (!inherits(report, "heterogeneity_report")) stop("report must come from explore_heterogeneity().")
  if (missing(x1) || is.null(x1)) stop("Choose a covariate for x1.")
  for (x in list(x1, x2)) if (!is.null(x) &&
      (!is.character(x) || length(x) != 1L || is.na(x) || !x %in% names(report$covariates))) stop("Choose covariates present in the report.")
  if (identical(x1, x2)) stop("Choose two different covariates.")
  check_integer(bins, "bins"); check_integer(max_levels, "max_levels"); check_integer(min_n, "min_n", 2L)
  covariates <- c(x1, x2)
  grouping <- lapply(covariates, function(v) bin_covariate(report$covariates[[v]], bins, max_levels))
  # One row per combination that occurs, in level order.
  keys <- unique(as.data.frame(stats::setNames(grouping, paste0("g", seq_along(grouping)))))
  keys <- keys[do.call(order, unname(as.list(keys))), , drop = FALSE]
  code <- function(groups) do.call(paste, c(lapply(groups, as.integer), sep = "-"))
  rows_in <- split(seq_along(grouping[[1]]), code(grouping))[code(as.list(keys))]
  weighted <- isTRUE(report$settings$weighted)
  binary <- !identical(report$settings$w_type, "continuous")
  summary <- lapply(rows_in, function(idx) {
    positive <- idx[report$weights[idx] > 0]
    n0 <- sum(report$W[positive] == 0); n1 <- sum(report$W[positive] == 1)
    varies <- if (binary) n0 > 0L && n1 > 0L else length(unique(report$W[positive])) > 1L
    status <- if (length(positive) < min_n) paste0("fewer than ", min_n, if (weighted) " positive-weight", " observations") else
      if (!varies && binary) paste("only one group of W", if (weighted) "has positive weight" else "is present") else
      if (!varies) "W does not vary in this group" else "ok"
    estimate <- if (status == "ok") point_average(report$scores[idx], report$weights[idx]) else NA_real_
    if (status == "ok" && !is.finite(estimate)) status <- "adjusted mean unavailable; check group overlap"
    row <- list(n = length(idx), n_positive = length(positive))
    if (binary) row <- c(row, list(n_control = n0, n_treated = n1))
    as.data.frame(c(row, list(estimate = estimate, status = status)))
  })
  out <- cbind(keys, do.call(rbind, summary))
  # Name group columns after the covariates without clashing with result columns.
  group_names <- make.unique(c(names(out)[-seq_along(covariates)], covariates))[ncol(out) - length(covariates) + seq_along(covariates)]
  names(out)[seq_along(covariates)] <- group_names
  rownames(out) <- NULL
  attr(out, "plot") <- list(
    covariates = covariates, columns = group_names, titles = var_label(report, covariates),
    levels = lapply(seq_along(covariates), function(k) {
      g <- keys[[k]]; factor(g, levels = levels(g), labels = level_labels(report, covariates[k], levels(g)))
    }),
    missing = Reduce(`|`, lapply(seq_along(covariates), function(k) {
      level <- attr(grouping[[k]], "missing_level")
      if (is.null(level)) rep(FALSE, nrow(keys)) else as.character(keys[[k]]) == level
    })),
    effect_label = report$settings$effect_label, average = report$ate$estimate, words = effect_words(report))
  class(out) <- c("heterogeneity_groups", "data.frame")
  out
}

# Groups for one covariate: observed values, or quantile bins labelled by the
# range of values in each, plus a "(Missing)" level when values are missing.
bin_covariate <- function(v, bins, max_levels) {
  observed <- v[!is.na(v)]
  if (is.numeric(v) && length(unique(observed)) > max_levels) {
    breaks <- unique(as.numeric(stats::quantile(observed, seq(0, 1, length.out = bins + 1))))
    if (length(breaks) < 2L) {
      group <- rep(1L, length(v)); group[is.na(v)] <- NA_integer_
      labels <- format(breaks[1L], trim = TRUE)
    } else {
      group <- as.integer(cut(v, breaks, include.lowest = TRUE, labels = FALSE))
      labels <- vapply(seq_len(length(breaks) - 1L), function(k) {
        values <- v[which(group == k)]
        if (!length(values)) return(paste0("Bin ", k))
        paste(format(signif(range(values), 4), trim = TRUE), collapse = " - ")
      }, character(1))
      if (anyDuplicated(labels)) labels <- paste0(labels, " [group ", seq_along(labels), "]")
    }
    values <- labels[group]
  } else {
    labels <- as.character(sort(unique(observed)))
    values <- as.character(v)
  }
  missing_level <- NULL
  if (anyNA(v)) {
    missing_level <- make.unique(c(labels, "(Missing)"))[length(labels) + 1L]
    values[is.na(v)] <- missing_level
    labels <- c(labels, missing_level)
  }
  structure(factor(values, levels = labels), missing_level = missing_level)
}
