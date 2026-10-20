#' Explore heterogeneity with one causal forest
#'
#' Supply a data frame and column names, or numeric vectors `Y`, `W` and
#' covariates `X`. The dependent variable `Y` (the outcome) can be continuous
#' or 0/1 (estimates are then differences in proportions). The independent
#' variable `W` (the treatment or comparison) can have two values, giving
#' adjusted **differences** between the two groups, or be a numeric scale,
#' giving adjusted **slopes**: the change in `Y` per one-unit increase in `W`.
#' Their average is the average treatment effect (for a numeric `W`, grf's
#' average partial effect). For a continuous `Y` it is also given in standard
#' deviations: divided by the pooled within-group standard deviation of `Y`
#' before adjustment, or, for a numeric `W`, as the change in `Y` (in SDs) per
#' SD of `W`; weighted when weights are supplied. Every other result stays in
#' the units of `Y`. The characteristics `X` are the covariates that
#' might moderate the effect. Missing covariates are always handled by grf
#' without imputation. There is no minimum sample size, but small samples are
#' noted, because their estimates are very noisy. Notes also record data
#' preparation, highly correlated covariates (|r| >= 0.9, which share
#' importance) and, for two groups, profiles with few comparable observations
#' in the other group.
#'
#' Printing the report (type `report`) shows a summary and three plots: the
#' spread of predictions, candidate moderators ranked by importance, and
#' profiles (see [plot.heterogeneity_report()]). [effect_by()] then checks
#' groups of a chosen covariate.
#'
#' Overall and group averages use grf's doubly robust scores; the spread and
#' profiles describe out-of-bag predictions, which include estimation noise.
#' All results are exploratory. A causal interpretation requires a suitable
#' research design, overlap and covariates unaffected by `W`; otherwise
#' describe adjusted associations.
#'
#' @param Y,W Numeric dependent variable (continuous or 0/1) and independent
#'   variable (two values, or a numeric scale); no missing values. Omit these
#'   when supplying `data`.
#' @param X Numeric data frame or matrix of covariates; NA is allowed.
#'   Omit when supplying `data`. Linear dependence is allowed.
#' @param ids,weights,clusters Optional identifiers, nonnegative weights and
#'   cluster labels. With `data`, supply column names; otherwise supply vectors.
#'   Clusters affect forest sampling. Weights also apply to displayed means.
#' @param W_hat Known expected value of `W` given the covariates, as one number
#'   or one value per original input row; if `NULL`, grf estimates it. For two
#'   groups this is the assignment probability, strictly between 0 and 1 (0.5
#'   in an experiment with equal allocation).
#' @param num_trees Number of trees (default 2000; at least 100).
#' @param tune Whether grf should tune its forest settings (slower).
#' @param top_k Number of covariates in the printed summary.
#' @param seed Random seed (default 21042020); the caller's random state is
#'   preserved.
#' @param labels Named character vector of readable covariate names.
#' @param value_labels Named list of named character vectors mapping category
#'   codes to readable labels.
#' @param effect_label Axis label with the units of `y`. By default "Difference
#'   in <y>" for a binary `w` and "Change in <y> per unit of <w>" for a numeric
#'   `w`.
#' @param data A data frame. The easiest interface uses `data`, `y`, `w`, `x`.
#' @param y,w Names of the dependent and independent variable columns in
#'   `data`.
#' @param x Character vector of covariate names in `data`.
#' @param treated For a two-valued `w`, the value coded 1 and compared against
#'   the other; by default the larger or alphabetically later.
#' @return A `heterogeneity_report`: a list with `ate` (the average treatment
#'   effect or average partial effect: `estimate` in the units of `Y`,
#'   `status`, and, for a continuous `Y`, `standardized`, the estimate in
#'   standard deviations, with the SDs used, `sd_y` and `sd_w`; these are `NA`
#'   otherwise),
#'   `cates` (one row per observation: id, cate_estimate, quintile),
#'   `moderators` (variable, label, importance, rank), `quintiles` (n and
#'   mean_prediction per prediction group), `prediction_range` (smallest and
#'   largest prediction), `missingness`, preparation `notes`, covariate
#'   `labels`, and the data, scores, `forest` and `settings` (including
#'   `w_type`, `"binary"` or `"continuous"`) used by the plots. Prediction
#'   groups have about equal row counts; tied predictions stay together, so
#'   fewer than five groups are possible. Weights apply to all means.
#' @references [grf guide](https://grf-labs.github.io/grf/articles/grf_guide.html).
#' @seealso [plot.heterogeneity_report()], [effect_by()], [run_app()]
#' @examples
#' set.seed(1)
#' d <- data.frame(age = runif(300, 18, 80), group = rbinom(300, 1, .5),
#'                 hours = runif(300, 0, 10))
#' d$score <- d$group * (1 + (d$age > 50)) + rnorm(300)
#' report <- explore_heterogeneity(data = d, y = "score", w = "group",
#'                                 x = "age", W_hat = .5, num_trees = 200)
#' report                 # summary and three plots
#' effect_by(report, "age")
#'
#' # A numeric W gives slopes: change in score per extra hour
#' d$score2 <- d$hours * (0.2 + 0.3 * (d$age > 50)) + rnorm(300)
#' slopes <- explore_heterogeneity(data = d, y = "score2", w = "hours",
#'                                 x = "age", num_trees = 200)
#' print(slopes, plots = FALSE)
#' @export
explore_heterogeneity <- function(Y = NULL, W = NULL, X = NULL, ids = NULL,
                                  W_hat = NULL, weights = NULL, clusters = NULL,
                                  num_trees = 2000, tune = FALSE, top_k = 5, seed = 21042020,
                                  labels = NULL, value_labels = NULL, effect_label = NULL,
                                  data = NULL, y = NULL, w = NULL, x = NULL, treated = NULL) {
  check_integer(num_trees, "num_trees", 100L)
  check_integer(top_k, "top_k")
  check_integer(seed, "seed", 0L)
  if (!is.logical(tune) || length(tune) != 1L || is.na(tune)) stop("tune must be TRUE or FALSE.")
  if (!is.null(effect_label) && (!is.character(effect_label) || length(effect_label) != 1L || is.na(effect_label))) {
    stop("effect_label must be a single string.")
  }
  if (!is.null(labels) && (!is.character(labels) || is.null(names(labels)) ||
      anyNA(labels) || anyNA(names(labels)) || any(!nzchar(names(labels))) || anyDuplicated(names(labels)))) {
    stop("labels must be a named character vector with unique, nonmissing names and values.")
  }
  if (!is.null(value_labels) && (!is.list(value_labels) || is.null(names(value_labels)) ||
      anyNA(names(value_labels)) || any(!nzchar(names(value_labels))) || anyDuplicated(names(value_labels)) ||
      !all(vapply(value_labels, function(v) is.character(v) && !anyNA(v) && !is.null(names(v)) &&
        !anyNA(names(v)) && !anyDuplicated(names(v)), logical(1))))) {
    stop("value_labels must be a named list of named character vectors.")
  }
  if (!is.null(data)) {
    if (!is.null(Y) || !is.null(W) || !is.null(X)) stop("Use either data with column names, or Y, W and X.")
    n_original <- nrow(data)
    inp <- prepare_inputs(data, y, w, x, ids, weights, clusters, treated)
    outcome_name <- y; comparison_name <- w
  } else {
    if (!is.null(y) || !is.null(w) || !is.null(x)) stop("Column names y, w and x require data.")
    if (!is.numeric(Y) || !is.null(dim(Y)) || any(!is.finite(Y))) stop("Y must be a finite numeric vector.")
    n_original <- length(Y)
    if ((!is.numeric(W) && !is.logical(W)) || !is.null(dim(W)) || length(W) != n_original ||
        any(!is.finite(as.numeric(W)))) stop("W must be a numeric vector with one finite value per outcome.")
    X <- as.data.frame(X)
    if (nrow(X) != n_original || !ncol(X) ||
        !all(vapply(X, function(v) is.numeric(v) && is.null(dim(v)), logical(1)))) {
      stop("X must have one row per outcome and at least one numeric covariate.")
    }
    if (anyNA(names(X)) || any(!nzchar(names(X))) || anyDuplicated(names(X))) stop("X columns must have unique, nonempty names.")
    if (any(is.infinite(as.matrix(X)))) stop("X cannot contain infinite values; NA is allowed.")
    for (v in list(ids = ids, weights = weights, clusters = clusters)) {
      if (!is.null(v) && (!is.atomic(v) || !is.null(dim(v)) || length(v) != n_original)) {
        stop("ids, weights and clusters must each contain one value per row.")
      }
    }
    if (!is.null(weights) && (!is.numeric(weights) || any(!is.finite(weights)))) stop("weights must be finite numeric values.")
    if (!is.null(clusters) && (anyNA(clusters) || (is.numeric(clusters) && any(!is.finite(clusters))))) stop("clusters must contain nonmissing labels.")
    # Reuse preparation without risking collisions with user column names;
    # messages then refer to Y and W as the user supplied them.
    reserved <- make.unique(c(names(X), "Y", "W", "ids", "weights", "clusters"))[-seq_len(ncol(X))]
    d <- X; d[[reserved[1]]] <- Y; d[[reserved[2]]] <- as.numeric(W)
    if (!is.null(ids)) d[[reserved[3]]] <- ids
    if (!is.null(weights)) d[[reserved[4]]] <- weights
    if (!is.null(clusters)) d[[reserved[5]]] <- clusters
    inp <- prepare_inputs(d, reserved[1], reserved[2], names(X),
      ids = if (!is.null(ids)) reserved[3], weights = if (!is.null(weights)) reserved[4],
      clusters = if (!is.null(clusters)) reserved[5], treated = treated)
    outcome_name <- "Y"; comparison_name <- "W"
  }
  binary <- inp$w_type == "binary"
  if (!is.null(W_hat)) {
    if (!is.numeric(W_hat) || !is.null(dim(W_hat)) || !length(W_hat) %in% c(1L, n_original) || any(!is.finite(W_hat)) ||
        (binary && any(W_hat <= 0 | W_hat >= 1))) {
      stop("W_hat must be finite, one value or one per original input row",
           if (binary) ", and strictly between 0 and 1 for two groups." else ".")
    }
    if (length(W_hat) > 1L) W_hat <- W_hat[inp$row_index]
  }
  if (is.null(effect_label)) effect_label <- if (binary) paste("Difference in", outcome_name) else
    paste("Change in", outcome_name, "per unit of", comparison_name)
  positive <- if (is.null(inp$weights)) rep(TRUE, inp$n) else inp$weights > 0
  if (!is.null(inp$clusters) && length(unique(inp$clusters[positive])) < 2L) stop("At least two clusters with positive weight are needed.")
  w_labels <- inp$w_labels
  if (binary) {
    comparison_labels <- value_labels[[comparison_name]]
    found <- w_labels %in% names(comparison_labels)
    w_labels[found] <- unname(comparison_labels[w_labels[found]])
  }

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv), add = TRUE)
  set.seed(seed)
  forest <- tryCatch(grf::causal_forest(inp$X, inp$Y, inp$W, W.hat = W_hat,
    num.trees = num_trees, honesty = TRUE, seed = seed,
    sample.weights = inp$weights, clusters = inp$clusters,
    tune.parameters = if (tune) "all" else "none"),
    error = function(e) stop("The forest could not be fitted (grf: ", sub("[.]\\s*$", "", conditionMessage(e)),
                             "). This usually means there are too few rows.", call. = FALSE))
  tau <- as.numeric(stats::predict(forest)$predictions)
  if (any(!is.finite(tau))) stop("The forest could not predict every row. Check sample size and group overlap.")
  scores <- as.numeric(grf::get_scores(forest))
  averaging_weights <- if (is.null(inp$weights)) rep(1, inp$n) else inp$weights
  average <- point_average(scores, averaging_weights)
  ate <- c(list(estimate = average, status = if (is.finite(average)) "ok" else
    "Adjusted average unavailable: some positive-weight rows have nonfinite scores; check group overlap."),
    standardize(average, inp$Y, inp$W, averaging_weights, binary))
  quintile <- prediction_groups(tau)
  cates <- data.frame(id = inp$ids, cate_estimate = tau, quintile = quintile)
  quintiles <- do.call(rbind, lapply(sort(unique(quintile)), function(k) {
    idx <- which(quintile == k)
    data.frame(quintile = k, n = length(idx), mean_prediction = point_average(tau[idx], averaging_weights[idx]))
  }))
  importance <- as.numeric(grf::variable_importance(forest))
  importance[!is.finite(importance)] <- 0
  variables <- names(inp$covariates)
  labels_all <- stats::setNames(variables, variables)
  known <- intersect(names(labels), variables)
  labels_all[known] <- labels[known]
  moderators <- data.frame(variable = variables, label = unname(labels_all),
    importance = vapply(variables, function(v) sum(importance[inp$feature_map$variable == v]), numeric(1)),
    row.names = NULL)
  moderators <- moderators[order(-moderators$importance), , drop = FALSE]
  moderators$rank <- seq_len(nrow(moderators)); rownames(moderators) <- NULL
  notes <- inp$notes
  correlated <- highly_correlated(inp$covariates)
  if (nrow(correlated)) notes <- c(notes, paste0("Highly correlated characteristics share importance (|r| >= 0.9): ",
    paste(sprintf("%s & %s (r = %.2f)", correlated$x, correlated$y, correlated$r), collapse = "; "), "."))
  if (binary && any(forest$W.hat[positive] < .05 | forest$W.hat[positive] > .95)) notes <- c(notes,
    "Some profiles have few comparable observations in the other group (estimated group probability below 0.05 or above 0.95).")
  if (ate$status != "ok") notes <- c(notes, ate$status)
  structure(list(ate = ate, cates = cates, moderators = moderators,
    quintiles = quintiles, labels = labels_all,
    prediction_range = range(tau[averaging_weights > 0]),
    missingness = inp$missingness, notes = notes, n_dropped = inp$n_dropped,
    X = inp$X, covariates = inp$covariates, feature_map = inp$feature_map,
    Y = inp$Y, W = inp$W, ids = inp$ids, weights = averaging_weights, scores = scores,
    forest = forest, settings = list(n = inp$n, covariates = variables,
      num_trees = forest[["_num_trees"]], tune = tune, top_k = top_k, seed = seed,
      W_hat = W_hat, weighted = !is.null(inp$weights), clustered = !is.null(inp$clusters),
      labels = labels, value_labels = value_labels, effect_label = effect_label,
      outcome = outcome_name, comparison = comparison_name, w_type = inp$w_type,
      w_labels = w_labels)), class = "heterogeneity_report")
}

#' Print an exploratory report
#'
#' Shows the summary (the average adjusted difference or slope, which is the
#' average treatment or partial effect, also in standard deviations for a
#' continuous outcome, the range of predictions, the candidate
#' moderators most used in tree splits, and notes on the data) and, by default,
#' the three plots of [plot.heterogeneity_report()]: the spread of
#' predictions, importance and profiles.
#' @param x A report from [explore_heterogeneity()].
#' @param digits Decimal places for estimates.
#' @param plots Draw the three plots after the summary (default `TRUE`).
#' @param ... Ignored.
#' @return `x`, invisibly.
#' @export
print.heterogeneity_report <- function(x, digits = 2, plots = TRUE, ...) {
  fmt <- function(v) ifelse(is.finite(v), formatC(v, digits = digits, format = "f", big.mark = ","), "unavailable")
  # Wrap text to the console (or document) width.
  say <- function(text, indent = 2, exdent = indent) {
    cat(strwrap(text, width = max(40, getOption("width") - 2), indent = indent, exdent = exdent), sep = "\n")
  }
  words <- effect_words(x)
  cat("Exploratory heterogeneity report\n")
  count <- function(k, word) paste(format(k, big.mark = ","), if (k == 1) word else paste0(word, "s"))
  say(paste(count(x$settings$n, "observation"), count(length(x$settings$covariates), "characteristic"),
            count(x$settings$num_trees, "tree"), sep = "; "))
  say(paste("Effect of W:", words$effect), exdent = 4)
  say(sprintf("%s: %s", words$Average, fmt(x$ate$estimate)))
  std <- x$ate$standardized %||% NA_real_
  if (is.finite(std)) {
    s <- x$settings
    say(if (identical(s$w_type, "continuous"))
      sprintf("In standard deviations: %s per SD of %s (SD of %s: %s; of %s: %s)", fmt(std), s$comparison,
              s$outcome, fmt(x$ate$sd_y), s$comparison, fmt(x$ate$sd_w))
    else sprintf("In standard deviations: %s (SD of %s within the two groups: %s)", fmt(std), s$outcome, fmt(x$ate$sd_y)),
    exdent = 4)
  }
  say(sprintf("Predicted %s range from %s to %s", words$plural, fmt(x$prediction_range[1]), fmt(x$prediction_range[2])))
  say(if (x$settings$weighted) "Means and prediction summaries use supplied weights." else
    "Results describe the analysis sample without weighting.", exdent = 4)
  cat("\nCandidate moderators (most used in tree splits):\n")
  m <- utils::head(x$moderators, x$settings$top_k)
  m <- data.frame(characteristic = m$label, importance = signif(m$importance, 3), rank = m$rank)
  print(m, row.names = FALSE)
  if (length(x$notes)) {
    cat("\nData notes:\n")
    for (note in x$notes) say(paste("-", note), exdent = 4)
  }
  cat("\n")
  say("Check groups of interest with effect_by(). Predictions describe profiles and include noise; causal interpretation depends on the research design.", indent = 0)
  if (isTRUE(plots)) for (type in c("distribution", "importance", "profiles")) print(plot(x, type))
  invisible(x)
}
