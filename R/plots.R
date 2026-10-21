#' Plot a heterogeneity report
#'
#' Three views of the fitted forest:
#' * `"distribution"` (default): histogram of the estimated treatment
#'   effects conditional on covariates (out-of-bag estimates of the
#'   conditional average treatment effect), coloured like the heatmaps: increasingly red
#'   further below 0 and increasingly teal further above it, with equal
#'   distances from 0 equally deep; 0 is always a bin edge. The number of
#'   bins follows the Freedman-Diaconis rule. The spread includes estimation
#'   noise.
#' * `"importance"`: all covariates, as candidate moderators, ranked by how
#'   much the forest split on them, weighted toward early splits; category
#'   indicators are added back to their covariate. Importance gives neither
#'   the size nor the direction of a treatment effect, and related covariates
#'   share it.
#' * `"profiles"`: for every covariate in order of importance (one row per
#'   category of a text covariate, as a share), its average in each treatment
#'   effect quintile, labelled from the bottom 20% of estimated treatment
#'   effects to the top 20% (with many tied estimates, fewer groups, labelled
#'   by their actual shares). Row labels give each covariate's observed range.
#'   Colours compare each average with the covariate's overall average, teal
#'   above and red below, measured in standard deviations of that covariate,
#'   so covariates in different units share one colour scale. Missing values
#'   are ignored.
#'
#' Supplied weights apply to bar heights and means. For estimates within
#' groups of a chosen covariate, use [effect_by()] and plot its result.
#' @param x A report from [explore_heterogeneity()].
#' @param type `"distribution"`, `"importance"` or `"profiles"`.
#' @param top_n Number of covariates for `"importance"` and `"profiles"`,
#'   taken from the importance ranking (default: all).
#' @param variables Covariates to show in `"profiles"`, instead of `top_n`.
#' @param ... Ignored.
#' @return A ggplot object.
#' @examples
#' set.seed(1)
#' d <- data.frame(age = runif(300, 18, 80), educ = sample(8:20, 300, TRUE),
#'                 group = rbinom(300, 1, .5))
#' d$score <- d$group * (1 + (d$age > 50)) + rnorm(300)
#' report <- explore_heterogeneity(data = d, y = "score", w = "group",
#'                                 x = c("age", "educ"), W_hat = .5, num_trees = 200)
#' plot(report)
#' plot(report, "importance")
#' plot(report, "profiles")
#' @export
plot.heterogeneity_report <- function(x, type = c("distribution", "importance", "profiles"),
                                      top_n = NULL, variables = NULL, ...) {
  type <- match.arg(type)
  if (!is.null(top_n)) check_integer(top_n, "top_n")
  all <- nrow(x$moderators)
  switch(type,
    distribution = plot_distribution(x),
    importance = plot_importance(x, top_n %||% all),
    profiles = plot_profiles(x, top_n %||% all, variables))
}

#' Plot average treatment effects within groups
#'
#' One covariate gives a dot plot of the group estimates; the axis shows each
#' group's row count. Two covariates give a heatmap coloured around the overall
#' average. Both leave out the missing-value groups, which the table keeps.
#' Groups that were not estimated (too few observations, or only one group of
#' a binary `W`, or no spread in a numeric `W`) are marked; the
#' table from [effect_by()] gives the reason.
#' @param x The result of [effect_by()].
#' @param ... Ignored.
#' @return A ggplot object.
#' @export
plot.heterogeneity_groups <- function(x, ...) {
  info <- attr(x, "plot")
  if (is.null(info) || nrow(x) != length(info$missing)) stop("Plot the unmodified result of effect_by().")
  d <- data.frame(n = x$n, estimate = x$estimate, g1 = info$levels[[1]])
  if (length(info$covariates) == 2L) {
    d$g2 <- info$levels[[2]]
    return(plot_heatmap(d[!info$missing, , drop = FALSE], info))
  }
  d <- d[!info$missing, , drop = FALSE]
  if (!nrow(d)) stop("Every group has missing values; there is nothing to plot.")
  # Axis labels: the group, its row count, and a mark if it was not estimated.
  axis <- paste0(wrap_lines(as.character(d$g1), 16), "\nn = ", format(d$n, big.mark = ",", trim = TRUE),
                 ifelse(is.finite(d$estimate), "", "\nnot estimated"))
  d$x_label <- factor(axis, levels = axis)
  shown <- d[is.finite(d$estimate), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x_label)) + ggplot2::geom_blank() +
    ggplot2::geom_point(data = shown, ggplot2::aes(y = estimate), colour = "#286b70", size = 3) +
    ggplot2::geom_text(data = shown, ggplot2::aes(y = estimate, label = sprintf("%.1f", estimate)), vjust = -1.1, size = 3.3) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, .2))) +
    ggplot2::labs(x = info$titles[1], y = info$effect_label, title = "Average treatment effects across groups",
                  caption = paste0("Each point is a different group of people. Missing values are left out. Groups with\ntoo few observations",
                                   if (isTRUE(info$words$numeric)) ", or no spread in W," else ", or only one group of W,",
                                   " are not estimated.")) +
    plot_theme()
  if (!nrow(shown)) p <- p + ggplot2::expand_limits(y = 0)
  p
}

# ---- The three report views ---------------------------------------------------

# Each bar is coloured by its position: deeper red further below 0, deeper
# teal further above it. With 0 as a bin edge, no bar spans both sides; bar
# centres just beyond the estimates take the end colours.
plot_distribution <- function(report) {
  d <- report$cates; d$weight <- report$weights
  ggplot2::ggplot(d, ggplot2::aes(x = cate_estimate, weight = weight)) +
    ggplot2::geom_histogram(ggplot2::aes(fill = ggplot2::after_stat(x)),
                            bins = grDevices::nclass.FD(d$cate_estimate), boundary = 0, colour = "white", linewidth = .2) +
    ggplot2::scale_fill_gradient2(low = colours$low, mid = colours$neutral, high = colours$high, midpoint = 0,
                                  limits = colour_limits(d$cate_estimate), oob = squish, guide = "none") +
    ggplot2::labs(x = report$settings$effect_label, y = if (report$settings$weighted) "Sum of weights" else "Observations",
                  title = "Distribution of estimated treatment effects, conditional on covariates",
                  caption = "Colours deepen with distance from 0: red below, teal above. Each estimate describes people\nwith similar characteristics, not one person, and includes estimation noise.") +
    plot_theme()
}

plot_importance <- function(report, top_n) {
  d <- utils::head(report$moderators, top_n)
  labels <- make.unique(wrap_lines(d$label))
  d$label <- factor(labels, levels = rev(labels))
  ggplot2::ggplot(d, ggplot2::aes(x = importance, y = label)) +
    ggplot2::geom_col(fill = "#286b70", width = .65) +
    ggplot2::labs(x = "Importance in tree splits", y = NULL, title = "Candidate moderators of the treatment effect, ranked by importance",
                  caption = "Importance does not give the size or direction of a treatment effect.\nRelated characteristics can share importance.") +
    plot_theme()
}

plot_profiles <- function(report, top_n, variables) {
  if (is.null(variables)) variables <- utils::head(report$moderators$variable, top_n)
  if (!is.character(variables) || !length(variables) || anyNA(variables) ||
      any(!variables %in% names(report$covariates))) stop("Choose covariates present in the report.")
  d <- profile_summary(report, unique(variables))
  # Each row is labelled with the characteristic's observed range.
  d$range <- vapply(d$feature, function(f) {
    v <- report$X[[f]]
    paste(range_label(min(v, na.rm = TRUE)), "to", range_label(max(v, na.rm = TRUE)))
  }, "")
  d$name <- d$label
  map <- unique(d[c("feature", "label", "range")])
  # Long names wrap; the range stays whole at the end of the last line.
  map$label <- make.unique(vapply(seq_len(nrow(map)), function(k) {
    lines <- strwrap(map$label[k], 32)
    lines[length(lines)] <- paste0(lines[length(lines)], " (", map$range[k], ")")
    paste(lines, collapse = "\n")
  }, ""))
  d$label <- factor(map$label[match(d$feature, map$feature)], levels = rev(map$label))
  columns <- quintile_labels(report$cates$quintile)
  d$group <- factor(columns[as.character(d$quintile)], levels = columns)
  d$value_label <- ifelse(is.finite(d$mean), number_label(d$mean), "")
  # Colours: distance from the characteristic's overall average, in its
  # standard deviations, so rows in different units share one scale.
  limits <- colour_limits(d$z)
  ggplot2::ggplot(d, ggplot2::aes(x = group, y = label, fill = z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) + ggplot2::geom_text(ggplot2::aes(label = value_label), size = 3.5) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_gradient2(low = colours$low, mid = colours$mid, high = colours$high, midpoint = 0,
      limits = limits, breaks = c(limits[1] * .8, 0, limits[2] * .8), labels = c("Below\naverage", "Average", "Above\naverage"),
      na.value = "grey90", name = NULL) +
    ggplot2::labs(x = if (length(columns) == 5L) "Estimated treatment effect quintiles" else "Groups by estimated treatment effect",
                  y = NULL, title = "Characteristics across treatment effect quintiles",
                  caption = paste0("Each cell is the characteristic's average in that quintile (a share for 0/1 rows). Colours compare it\n",
                                   "with the overall average: teal above, red below, deeper the further away. Missing values are\n",
                                   "ignored. Sorting on estimated effects can itself create separation.")) +
    plot_theme() +
    ggplot2::theme(legend.position = "bottom", legend.key.width = ggplot2::unit(1.6, "cm"))
}

plot_heatmap <- function(d, info) {
  d$g1 <- droplevels(d$g1); d$g2 <- droplevels(d$g2)
  midpoint <- if (is.finite(info$average)) info$average else 0
  counts <- format(d$n, big.mark = ",", trim = TRUE)
  d$label <- ifelse(is.finite(d$estimate), sprintf("%.1f\nn = %s", d$estimate, counts), paste0("n = ", counts))
  ggplot2::ggplot(d, ggplot2::aes(x = g1, y = g2, fill = estimate)) +
    ggplot2::geom_tile(colour = "white") + ggplot2::geom_text(ggplot2::aes(label = label), size = 3.3) +
    ggplot2::scale_fill_gradient2(low = colours$low, mid = colours$mid, high = colours$high, midpoint = midpoint,
      limits = colour_limits(d$estimate, midpoint), na.value = "grey85", name = info$effect_label) +
    ggplot2::scale_x_discrete(labels = function(v) wrap_lines(v, 12)) +
    ggplot2::scale_y_discrete(labels = function(v) wrap_lines(v, 22)) +
    ggplot2::labs(x = info$titles[1], y = info$titles[2], title = "Average treatment effects across groups",
                  caption = sprintf("Each cell is a different group of people; colours are centred on the average treatment effect (%.1f).\nGrey cells are not estimated. Missing values are left out.", midpoint)) +
    plot_theme() +
    ggplot2::theme(legend.position = "bottom", legend.key.width = ggplot2::unit(1.6, "cm"))
}

# ---- Shared pieces ---------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
# Shared colours: red below the reference value, teal above it. Histogram
# bars near 0 are light grey, since the heatmaps' near-white would vanish on a
# white page.
colours <- list(low = "#bf7770", mid = "#f7f7f7", high = "#72a9ac", neutral = "#e4e4e7")
squish <- function(x, range) pmin(pmax(x, range[1]), range[2])
# Numbers in cells and ranges: two decimals below 10, one below 100, none above.
number_label <- function(x) {
  digits <- ifelse(abs(x) >= 100, 0, ifelse(abs(x) >= 10, 1, 2))
  vapply(seq_along(x), function(k) formatC(x[k], format = "f", digits = digits[k], big.mark = ","), "")
}
range_label <- function(x) if (x == round(x)) formatC(x, format = "d", big.mark = ",") else number_label(x)
# Column labels for the treatment effect quintiles, from each group's share of
# rows: "Bottom 20%", "20-40%", ..., "Top 20%". Tied estimates can merge
# groups, and the labels then give the actual shares.
quintile_labels <- function(groups) {
  k <- sort(unique(groups))
  if (length(k) == 1L) return(stats::setNames("All", k))
  upper <- round(100 * cumsum(as.numeric(table(factor(groups, levels = k)))) / length(groups))
  lower <- c(0, utils::head(upper, -1))
  labels <- sprintf("%d-%d%%", lower, upper)
  labels[1] <- sprintf("Bottom %d%%", upper[1])
  labels[length(k)] <- sprintf("Top %d%%", 100 - lower[length(k)])
  stats::setNames(labels, k)
}
wrap_lines <- function(x, width = 32) vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), character(1), USE.NAMES = FALSE)
# Titles and captions start at the left edge of the whole plot, so long axis
# labels cannot push a caption past the right edge.
plot_theme <- function() ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.caption = ggplot2::element_text(hjust = 0),
                 plot.title.position = "plot", plot.caption.position = "plot")

# Colour limits symmetric around a midpoint. The tiny pad keeps the most
# extreme value inside the limits despite floating-point rounding; otherwise
# ggplot2 would draw it in the colour reserved for missing values.
colour_limits <- function(values, midpoint = 0) {
  values <- values[is.finite(values)]
  limit <- if (length(values)) max(abs(values - midpoint)) else 0
  if (limit == 0) limit <- 1
  midpoint + c(-1, 1) * limit * (1 + 1e-6)
}
