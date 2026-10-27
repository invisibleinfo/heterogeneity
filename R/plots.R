#' Plot a heterogeneity report
#'
#' Two views of the fitted forest:
#' * `"distribution"` (default): histogram of the estimated treatment
#'   effects conditional on covariates (out-of-bag estimates of the
#'   conditional average treatment effect), increasingly red further below 0
#'   and increasingly teal further above it, with equal distances from 0
#'   equally deep; a dashed line marks 0, which is always a bin edge, and a
#'   solid, labelled line marks the average treatment effect. The number of
#'   bins follows the Freedman-Diaconis rule. The spread includes estimation
#'   noise.
#' * `"importance"`: all covariates, as candidate moderators, ranked by how
#'   much the forest split on them, weighted toward early splits; category
#'   indicators are added back to their covariate. Importance gives neither
#'   the size nor the direction of a treatment effect, and related covariates
#'   share it.
#'
#' Supplied weights apply to bar heights. To see how the treatment effect
#' varies with one or two covariates, plot the result of [effect_by()].
#' @param x A report from [explore_heterogeneity()].
#' @param type `"distribution"` or `"importance"`.
#' @param top_n Number of covariates for `"importance"`, taken from the
#'   importance ranking (default: all).
#' @param ... Ignored.
#' @return A ggplot object.
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   age = runif(300, 18, 80),
#'   educ = sample(8:20, 300, TRUE),
#'   group = rbinom(300, 1, .5)
#' )
#' d$score <- d$group * (1 + (d$age > 50)) + rnorm(300)
#' report <- explore_heterogeneity(
#'   data = d,
#'   y = "score",
#'   w = "group",
#'   x = c("age", "educ"),
#'   W_hat = .5,
#'   num_trees = 200
#' )
#' plot(report)
#' plot(report, "importance")
#' @export
plot.heterogeneity_report <- function(
  x,
  type = c("distribution", "importance"),
  top_n = NULL,
  ...
) {
  type <- match.arg(type)
  if (!is.null(top_n)) {
    check_integer(top_n, "top_n")
  }
  switch(
    type,
    distribution = plot_distribution(x),
    importance = plot_importance(x, top_n %||% nrow(x$moderators))
  )
}

#' Plot average treatment effects within groups
#'
#' One numeric covariate gives a scatter plot: each row's estimated treatment
#' effect against the covariate's value (where values repeat, points are spread
#' slightly sideways), with a line for the average treatment effect along the
#' covariate. The line is a loess smooth of grf's doubly robust scores, the
#' scores behind every average, weighted when weights were supplied: with span
#' 0.75 and degree 2, as in `geom_smooth()`, or, for a covariate with at most
#' `max_levels` values (see [effect_by()]), a wider local-linear smooth (span 2,
#' degree 1), which the default cannot fit reliably on so few values; such
#' covariates are labelled by their categories. The estimates themselves are
#' pulled toward the overall average, so the line can rise and fall more than
#' the points. A text covariate gives a dot plot of the group estimates, since
#' its categories have no order; the axis shows each group's row count. Both
#' have a dashed line at 0. Two covariates give a heatmap coloured around the
#' overall average. Captions give the average treatment effect. All three leave
#' out missing values, which the table keeps as a group. In the dot plot and
#' heatmap, groups that were not estimated (too few observations, or only one
#' group of a binary `W`, or no spread in a numeric `W`) are marked; the table
#' from [effect_by()] gives the reason.
#' @param x The result of [effect_by()].
#' @param ... Ignored.
#' @return A ggplot object.
#' @export
plot.heterogeneity_groups <- function(x, ...) {
  info <- attr(x, "plot")
  if (is.null(info) || nrow(x) != length(info$missing)) {
    stop("Plot the unmodified result of effect_by().")
  }
  if (!is.null(info$rows)) {
    return(plot_scatter(info))
  }
  d <- data.frame(n = x$n, estimate = x$estimate, g1 = info$levels[[1]])
  if (length(info$covariates) == 2L) {
    d$g2 <- info$levels[[2]]
    return(plot_heatmap(d[!info$missing, , drop = FALSE], info))
  }
  d <- d[!info$missing, , drop = FALSE]
  if (!nrow(d)) {
    stop("Every group has missing values; there is nothing to plot.")
  }
  # Axis labels: the group, its row count, and a mark if it was not estimated.
  axis <- paste0(
    wrap_lines(as.character(d$g1), 16),
    "\nn = ",
    format(d$n, big.mark = ",", trim = TRUE),
    ifelse(is.finite(d$estimate), "", "\nnot estimated")
  )
  d$x_label <- factor(axis, levels = axis)
  shown <- d[is.finite(d$estimate), , drop = FALSE]
  p <- ggplot2::ggplot(d, ggplot2::aes(x = x_label)) +
    ggplot2::geom_blank() +
    zero_line() +
    ggplot2::geom_point(
      data = shown,
      ggplot2::aes(y = estimate),
      colour = "#286b70",
      size = 3
    ) +
    ggplot2::geom_text(
      data = shown,
      ggplot2::aes(y = estimate, label = sprintf("%.1f", estimate)),
      vjust = -1.1,
      size = 3.3
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(.1, .2))) +
    ggplot2::labs(
      x = info$titles[1],
      y = "Treatment effect",
      title = "Average treatment effects across groups",
      caption = paste0(
        "Each point is the average treatment effect in a different group of people; the dashed line marks 0.\n",
        average_note(info$average),
        " Missing values are left out. Groups with too few observations",
        if (isTRUE(info$words$numeric)) {
          ", or no spread in W,"
        } else {
          ", or only one group of W,"
        },
        "\nare not estimated."
      )
    ) +
    plot_theme()
  p
}

# ---- The report and group views ----------------------------------------------

# Each bar is coloured by its position: deeper red further below 0, deeper
# teal further above it. With 0 as a bin edge, no bar spans both sides; bar
# centres just beyond the estimates take the end colours.
plot_distribution <- function(report) {
  d <- data.frame(effect = report$estimates, weight = report$weights)
  ate <- report$ate$estimate
  p <- ggplot2::ggplot(d, ggplot2::aes(x = effect, weight = weight)) +
    ggplot2::geom_histogram(
      ggplot2::aes(fill = ggplot2::after_stat(x)),
      bins = grDevices::nclass.FD(d$effect),
      boundary = 0,
      colour = "white",
      linewidth = .2
    ) +
    zero_line("x") +
    ggplot2::scale_fill_gradient2(
      low = colours$low,
      mid = colours$neutral,
      high = colours$high,
      midpoint = 0,
      limits = colour_limits(d$effect),
      oob = squish,
      guide = "none"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .12))) +
    ggplot2::labs(
      x = report$settings$effect_label,
      y = if (report$settings$weighted) "Sum of weights" else "Observations",
      title = "Distribution of estimated treatment effects, conditional on covariates",
      caption = paste0(
        "Colours deepen with distance from 0, which the dashed line marks: red below, teal above. The solid\n",
        "line marks the average treatment effect. Each estimate describes people with similar characteristics,\n",
        "not one person, and includes noise."
      )
    ) +
    plot_theme()
  if (!is.finite(ate)) {
    return(p)
  }
  # The label sits on the side of the line with more room.
  right <- ate > mean(range(c(0, d$effect)))
  p +
    ggplot2::geom_vline(xintercept = ate, colour = "#18181b", linewidth = .6) +
    ggplot2::annotate(
      "text",
      x = ate,
      y = Inf,
      label = paste("Average treatment effect:", average_value(ate)),
      hjust = if (right) 1.04 else -.04,
      vjust = 1.6,
      size = 3.3
    )
}

plot_importance <- function(report, top_n) {
  d <- utils::head(report$moderators, top_n)
  labels <- make.unique(wrap_lines(d$label))
  d$label <- factor(labels, levels = rev(labels))
  ggplot2::ggplot(d, ggplot2::aes(x = importance, y = label)) +
    ggplot2::geom_col(fill = "#286b70", width = .65) +
    ggplot2::labs(
      x = "Importance in tree splits",
      y = NULL,
      title = "Candidate moderators of the treatment effect, ranked by importance",
      caption = "Importance does not give the size or direction of a treatment effect.\nRelated characteristics can share importance."
    ) +
    plot_theme()
}

plot_scatter <- function(info) {
  d <- info$rows
  if (!nrow(d)) {
    stop("Every row has a missing value; there is nothing to plot.")
  }
  values <- sort(unique(d$value))
  spread_out <- if (length(values) > 1L && length(values) <= nrow(d) / 2) {
    .4 * min(diff(values))
  } else {
    0
  }
  p <- ggplot2::ggplot(d, ggplot2::aes(x = value, y = effect)) +
    zero_line() +
    ggplot2::geom_point(
      colour = "#286b70",
      alpha = min(.7, max(.08, 15 / sqrt(nrow(d)))),
      size = 1.6,
      stroke = 0,
      position = ggplot2::position_jitter(
        width = spread_out,
        height = 0,
        seed = 1
      )
    )
  if (!is.null(info$smooth)) {
    p <- p +
      ggplot2::geom_line(data = info$smooth, colour = "#18181b", linewidth = 1)
  }
  if (!is.null(info$breaks)) {
    p <- p +
      ggplot2::scale_x_continuous(
        breaks = info$breaks,
        labels = wrap_lines(info$break_labels, 14)
      )
  }
  p +
    ggplot2::labs(
      x = info$titles[1],
      y = "Treatment effect",
      title = "Estimated treatment effects across values of a characteristic",
      caption = paste0(
        "Each point is one row's estimated treatment effect",
        if (!is.null(info$smooth)) {
          paste0(
            "; the line is the average treatment effect along the\n",
            "characteristic (a loess smooth of grf's doubly robust scores)"
          )
        },
        ". The dashed line marks 0.\n",
        average_note(info$average),
        " Missing values are left out."
      )
    ) +
    plot_theme()
}

# The average treatment effect along a covariate: a loess smooth of the rows'
# doubly robust scores, weighted when weights were supplied. Averages of these
# scores, not of the estimated treatment effects, are what grf uses for every
# average. Many values: span 0.75 and degree 2, as in geom_smooth(). Few values
# (`few`): the default is near-singular there, so a wider local-linear smooth
# (span 2, degree 1), which reproduces the two averages of a 0/1 covariate.
# Returns the line on a grid, with its values at the observed values as
# attribute "at"; NULL if it cannot be fitted.
loess_line <- function(rows, few = FALSE) {
  rows <- rows[is.finite(rows$score), , drop = FALSE]
  values <- sort(unique(rows$value))
  if (length(values) < 2L) {
    return(NULL)
  }
  tryCatch(
    suppressWarnings({
      fit <- stats::loess(
        score ~ value,
        data = rows,
        weights = rows$weight,
        span = if (few) 2 else .75,
        degree = if (few) 1 else 2,
        control = stats::loess.control(
          surface = if (few) "direct" else "interpolate",
          trace.hat = "approximate"
        )
      )
      at <- function(x) as.numeric(stats::predict(fit, data.frame(value = x)))
      grid <- seq(min(values), max(values), length.out = 200)
      line <- data.frame(value = grid, effect = at(grid))
      observed <- data.frame(value = values, effect = at(values))
      if (all(is.finite(line$effect)) && all(is.finite(observed$effect))) {
        structure(line, at = observed)
      }
    }),
    error = function(e) NULL
  )
}

plot_heatmap <- function(d, info) {
  d$g1 <- droplevels(d$g1)
  d$g2 <- droplevels(d$g2)
  midpoint <- if (is.finite(info$average)) info$average else 0
  counts <- format(d$n, big.mark = ",", trim = TRUE)
  d$label <- ifelse(
    is.finite(d$estimate),
    sprintf("%.1f\nn = %s", d$estimate, counts),
    paste0("n = ", counts)
  )
  ggplot2::ggplot(d, ggplot2::aes(x = g1, y = g2, fill = estimate)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3.3) +
    ggplot2::scale_fill_gradient2(
      low = colours$low,
      mid = colours$mid,
      high = colours$high,
      midpoint = midpoint,
      limits = colour_limits(d$estimate, midpoint),
      na.value = "grey85",
      name = "Treatment effect"
    ) +
    ggplot2::scale_x_discrete(labels = function(v) wrap_lines(v, 12)) +
    ggplot2::scale_y_discrete(labels = function(v) wrap_lines(v, 22)) +
    ggplot2::labs(
      x = info$titles[1],
      y = info$titles[2],
      title = "Average treatment effects across groups",
      caption = paste0(
        "Each cell is a different group of people. ",
        average_note(info$average),
        " Colours are centred on it.\nGrey cells are not estimated. Missing values are left out."
      )
    ) +
    plot_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(1.6, "cm")
    )
}

# ---- Shared pieces -----------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a
# Shared colours: red below the reference value, teal above it. Histogram
# bars near 0 are light grey, since the heatmap's near-white would vanish on a
# white page.
colours <- list(
  low = "#bf7770",
  mid = "#f7f7f7",
  high = "#72a9ac",
  neutral = "#e4e4e7"
)
squish <- function(x, range) pmin(pmax(x, range[1]), range[2])
# The average treatment effect, as quoted in labels and captions.
average_value <- function(x) formatC(x, digits = 2, format = "f")
average_note <- function(x) {
  if (is.finite(x)) {
    paste0("The average treatment effect is ", average_value(x), ".")
  } else {
    "The average treatment effect is unavailable."
  }
}
# A dashed reference line at a treatment effect of 0.
zero_line <- function(axis = "y") {
  if (axis == "x") {
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      colour = "#71717a",
      linewidth = .5
    )
  } else {
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "#71717a",
      linewidth = .5
    )
  }
}
wrap_lines <- function(x, width = 32) {
  vapply(
    x,
    function(s) paste(strwrap(s, width), collapse = "\n"),
    character(1),
    USE.NAMES = FALSE
  )
}
# Titles and captions start at the left edge of the whole plot, so long axis
# labels cannot push a caption past the right edge.
plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}

# Colour limits symmetric around a midpoint. The tiny pad keeps the most
# extreme value inside the limits despite floating-point rounding; otherwise
# ggplot2 would draw it in the colour reserved for missing values.
colour_limits <- function(values, midpoint = 0) {
  values <- values[is.finite(values)]
  limit <- if (length(values)) max(abs(values - midpoint)) else 0
  if (limit == 0) {
    limit <- 1
  }
  midpoint + c(-1, 1) * limit * (1 + 1e-6)
}
