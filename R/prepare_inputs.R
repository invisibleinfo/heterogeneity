#' Prepare Y, W and X from a data frame
#'
#' Turns the columns of a data frame into the inputs of
#' [explore_heterogeneity()] and records every change it makes, so that the
#' preparation can be reported. [explore_heterogeneity()] runs it when given
#' `data`; call it directly to inspect the preparation.
#'
#' Missing covariates are always retained for grf's native missing-value
#' splitting; nothing is imputed. Rows missing the dependent variable, the
#' independent variable, or a supplied weight/cluster are dropped. Text/factor
#' covariates become 0/1 indicators: one for two categories, otherwise one per
#' category (no reference category, which trees do not need), so a tree can
#' separate any category in one split. Their missing entries remain missing.
#' Original covariates are retained for grouping and readable plots. Entirely
#' missing and constant covariates are removed. Correlated and linearly
#' dependent covariates are allowed. There is no minimum sample size; small
#' groups are noted, because their estimates are very noisy.
#'
#' Untidy values are cleaned first: attributes such as SPSS/Stata labels are
#' dropped (codes stay numbers), infinite numbers and blank or common
#' missing-value text ("NA", "N/A", "NaN", "NULL", ".", "-") become missing,
#' and text columns whose values are all numbers become numeric. Numeric
#' category codes retain their ordering; convert nominal categories to factors
#' or text before preparation. Native missing-value handling avoids filling in
#' values; it does not remove bias from missing data.
#'
#' @param data A data frame.
#' @param y,w Names of the dependent variable and the independent
#'   variable. Each can be binary, with any two
#'   values, which are coded 0/1, or continuous, a numeric scale with more
#'   values, which is kept as it is.
#' @param x Character vector of covariate column names.
#' @param ids,weights,clusters Optional names of identifier, survey-weight and
#'   cluster columns. Without `ids`, original row numbers are retained after
#'   dropping rows. Cluster labels are encoded as integer group identifiers.
#' @param treated,y_one For a two-valued `w` or `y` (including 0/1), the value
#'   to code as 1; by default the larger (or alphabetically later) value.
#' @param max_categories Text covariates with more categories than this are
#'   rejected (they are usually identifiers or free text).
#' @return A list with `Y`, `W`, numeric `X`, original `covariates`, a
#'   `feature_map` linking indicators to their source variables, `missingness`,
#'   `row_index`, `ids`, `weights`, `clusters`, the number
#'   of observations used (`n`) and dropped (`n_dropped`), `y_type` and
#'   `w_type` (`"binary"` or `"continuous"`), `y_labels` and `w_labels` (for a
#'   binary variable, the original values coded as `0` and `1`; otherwise
#'   `NULL`) and `notes`, a character vector describing each change.
#' @noRd
prepare_inputs <- function(
  data,
  y,
  w,
  x,
  ids = NULL,
  weights = NULL,
  clusters = NULL,
  treated = NULL,
  y_one = NULL,
  max_categories = 20
) {
  data <- as.data.frame(data)
  if (
    anyNA(names(data)) ||
      any(!nzchar(names(data))) ||
      anyDuplicated(names(data))
  ) {
    stop("Data columns must have nonempty, unique names.")
  }
  for (v in list(
    y = y,
    w = w,
    ids = ids,
    weights = weights,
    clusters = clusters
  )) {
    if (!is.null(v) && !(is_string(v) && nzchar(v))) {
      stop("y, w, ids, weights and clusters must each name one column.")
    }
  }
  if (is.null(y) || is.null(w)) {
    stop("Choose a dependent variable (y) and an independent variable (w).")
  }
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    stop("x must contain the names of characteristic columns.")
  }
  if (
    !is.numeric(max_categories) ||
      length(max_categories) != 1L ||
      !is.finite(max_categories) ||
      max_categories < 2 ||
      max_categories != floor(max_categories)
  ) {
    stop("max_categories must be an integer of at least 2.")
  }
  if (length(x) < 1) {
    stop("Choose at least one characteristic (x).")
  }
  if (identical(y, w)) {
    stop(
      "The dependent variable (y) and the independent variable (w) must be different columns."
    )
  }
  if (y %in% x || w %in% x) {
    stop("y and w cannot also be characteristics (x).")
  }
  if (anyDuplicated(c(y, w, ids, weights, clusters))) {
    stop("The y, w, ID, weight and cluster columns must be distinct.")
  }
  excluded <- intersect(x, c(ids, weights, clusters))
  x <- setdiff(x, c(ids, weights, clusters))
  if (length(x) < 1) {
    stop(
      "Choose at least one characteristic that is not the ID, weight or cluster column."
    )
  }
  used <- unique(c(y, w, x, ids, weights, clusters))
  absent <- setdiff(used, names(data))
  if (length(absent)) {
    stop("Columns not found in the data: ", paste(absent, collapse = ", "))
  }
  fmt_n <- function(k) formatC(k, format = "d", big.mark = ",")
  rows_were <- function(k) {
    if (k == 1) "1 row was" else paste(fmt_n(k), "rows were")
  }
  notes <- character(0)
  if (length(excluded)) {
    notes <- c(
      notes,
      paste0(
        "Design/ID columns were excluded from the characteristics: ",
        paste(excluded, collapse = ", "),
        "."
      )
    )
  }

  # Clean values: logical -> 0/1, non-finite -> NA, factors -> text,
  # missing-value text -> NA, and text holding only numbers -> numbers.
  d <- data[, used, drop = FALSE]
  row_ids <- seq_len(nrow(d))
  # Labelled columns (e.g. read with haven) become plain codes.
  d[] <- lapply(d, function(v) {
    if (inherits(v, "haven_labelled")) {
      v <- unclass(v)
      attributes(v) <- NULL
      v
    } else {
      v
    }
  })
  supported <- vapply(
    d,
    function(v) {
      is.null(dim(v)) &&
        (is.numeric(v) || is.logical(v) || is.character(v) || is.factor(v))
    },
    logical(1)
  )
  if (any(!supported)) {
    stop(
      "Unsupported column type: ",
      paste(names(d)[!supported], collapse = ", "),
      ". Use numeric values or text/factor categories."
    )
  }
  was_text <- vapply(d, function(v) is.character(v) || is.factor(v), logical(1))
  d[] <- lapply(d, function(v) {
    if (is.logical(v)) {
      v <- as.numeric(v)
    }
    if (is.factor(v)) {
      v <- as.character(v)
    }
    if (is.character(v)) {
      v <- trimws(v)
      v[
        tolower(v) %in% c("", "na", "n/a", "#n/a", "nan", "null", ".", "-")
      ] <- NA
      number <- suppressWarnings(as.numeric(v))
      if (any(!is.na(v)) && all(is.na(v) | !is.na(number))) v <- number
    }
    if (is.numeric(v)) {
      v[!is.finite(v)] <- NA
    }
    attributes(v) <- NULL # labels, formats and other attributes
    v
  })
  converted <- names(d)[was_text & vapply(d, is.numeric, logical(1))]
  if (length(converted)) {
    notes <- c(
      notes,
      paste0(
        "Text columns holding only numbers were read as numbers: ",
        paste(converted, collapse = ", "),
        "."
      )
    )
  }
  if (!is.null(weights) && !is.numeric(d[[weights]])) {
    stop("'", weights, "' must be numeric.")
  }

  # Dependent variable, coded like the independent one below: two values are
  # coded 0/1, so effects are differences in the share coded 1; numbers with
  # more values are a continuous scale.
  y_vals <- as.character(sort(unique(d[[y]][!is.na(d[[y]])])))
  if (length(y_vals) == 2L) {
    y_type <- "binary"
    y1 <- if (is.null(y_one)) y_vals[2] else as.character(y_one)
    if (length(y1) != 1L || is.na(y1) || !y1 %in% y_vals) {
      stop(sprintf(
        "'y_one' must be one of the two values of '%s': %s",
        y,
        paste(y_vals, collapse = ", ")
      ))
    }
    y0 <- setdiff(y_vals, y1)
    if (!(identical(y_vals, c("0", "1")) && y1 == "1")) {
      notes <- c(
        notes,
        sprintf(
          "%s was recoded so that '%s' = 1 and '%s' = 0; effects are differences in the share of '%s'.",
          y,
          y1,
          y0,
          y1
        )
      )
    }
    d[[y]] <- ifelse(is.na(d[[y]]), NA, as.numeric(as.character(d[[y]]) == y1))
  } else {
    if (!is.numeric(d[[y]])) {
      stop("'", y, "' must be numeric, or have two values.")
    }
    if (!is.null(y_one)) {
      stop("'y_one' applies only to a dependent variable with two values.")
    }
    y_type <- "continuous"
  }

  # Independent variable: two values are coded 0/1; a numeric scale is kept.
  vals <- as.character(sort(unique(d[[w]][!is.na(d[[w]])])))
  if (length(vals) < 2) {
    stop(sprintf(
      "The independent variable must take at least two values ('%s' has %s).",
      w,
      length(vals)
    ))
  }
  if (length(vals) == 2L) {
    w_type <- "binary"
    one <- if (is.null(treated)) vals[2] else as.character(treated)
    if (length(one) != 1L || is.na(one)) {
      stop("'treated' must be one of the two values of '", w, "'.")
    }
    if (!one %in% vals) {
      stop(sprintf(
        "'treated' must be one of the two values of '%s': %s",
        w,
        paste(vals, collapse = ", ")
      ))
    }
    zero <- setdiff(vals, one)
    if (!(identical(vals, c("0", "1")) && one == "1")) {
      notes <- c(
        notes,
        sprintf("%s was recoded so that '%s' = 1 and '%s' = 0.", w, one, zero)
      )
    }
    d[[w]] <- ifelse(is.na(d[[w]]), NA, as.numeric(as.character(d[[w]]) == one))
  } else {
    if (!is.numeric(d[[w]])) {
      stop(sprintf(
        "An independent variable with more than two values must be numeric ('%s' has %s text values).",
        w,
        length(vals)
      ))
    }
    if (!is.null(treated)) {
      stop("'treated' applies only to an independent variable with two values.")
    }
    w_type <- "continuous"
    notes <- c(
      notes,
      sprintf(
        "%s has %s values and is used as a continuous scale: treatment effects are slopes, the change per one-unit increase in %s.",
        w,
        fmt_n(length(vals)),
        w
      )
    )
  }

  # Rows missing y, w, the weight or the cluster are dropped
  n_start <- nrow(d)
  key <- c(y, w, weights, clusters)
  bad <- !stats::complete.cases(d[key])
  if (any(bad)) {
    notes <- c(
      notes,
      sprintf(
        "%s dropped because %s was missing.",
        rows_were(sum(bad)),
        and_or(key[colSums(is.na(d[bad, key, drop = FALSE])) > 0])
      )
    )
    d <- d[!bad, , drop = FALSE]
    row_ids <- row_ids[!bad]
  }
  if (!is.null(weights) && any(d[[weights]] < 0)) {
    stop("Survey weights must be zero or positive.")
  }

  # Missing covariates stay missing for the forest.
  empty <- x[vapply(d[x], function(v) all(is.na(v)), logical(1))]
  if (length(empty)) {
    x <- setdiff(x, empty)
    notes <- c(
      notes,
      paste0(
        "Characteristics with no observed values were left out: ",
        paste(empty, collapse = ", "),
        "."
      )
    )
  }
  if (!length(x)) {
    stop("No usable characteristics are left.")
  }
  # A constant observed value plus NA can carry a missingness pattern.
  const <- x[vapply(d[x], function(v) length(unique(v)) < 2L, logical(1))]
  if (length(const)) {
    x <- setdiff(x, const)
    notes <- c(
      notes,
      paste0(
        "Constant characteristics were left out: ",
        paste(const, collapse = ", "),
        "."
      )
    )
  }
  if (!length(x)) {
    stop("No usable characteristics are left.")
  }
  covariates <- d[x]
  missingness <- data.frame(
    variable = x,
    n_missing = vapply(covariates, function(v) sum(is.na(v)), integer(1)),
    n = nrow(d),
    row.names = NULL
  )
  missingness$proportion <- missingness$n_missing / missingness$n
  many <- missingness$variable[missingness$proportion > 0.5]
  if (length(many)) {
    notes <- c(
      notes,
      paste0(
        "More than 50% of values are missing for: ",
        paste(many, collapse = ", "),
        "."
      )
    )
  }

  # Text covariates become 0/1 indicators: one for two categories (the less
  # common one), otherwise one per category, most common first. Trees need no
  # reference category, and with one indicator each, any category can be
  # separated in a single split. A map links the indicators to their variable,
  # so it can be plotted and ranked as a whole.
  parts <- list()
  maps <- list()
  encoded <- character()
  for (v in x) {
    values <- covariates[[v]]
    if (is.character(values)) {
      lv <- names(sort(table(values), decreasing = TRUE))
      if (length(lv) > max_categories) {
        stop(sprintf(
          "'%s' has %s categories; at most %s are supported. Exclude identifiers and free text.",
          v,
          length(lv),
          max_categories
        ))
      }
      chosen <- if (length(lv) == 2L) lv[2L] else lv
      part <- as.data.frame(lapply(chosen, function(l) as.numeric(values == l)))
      names(part) <- paste0(v, "=", chosen)
      maps[[v]] <- data.frame(
        feature = names(part),
        variable = v,
        level = chosen
      )
      parts[[v]] <- part
      encoded <- c(encoded, v)
    } else {
      parts[[v]] <- covariates[v]
      maps[[v]] <- data.frame(feature = v, variable = v, level = NA_character_)
    }
  }
  if (length(encoded)) {
    notes <- c(
      notes,
      paste0(
        "Text characteristics became 0/1 indicators, one per category ",
        "(a single one for two categories), with missing entries kept missing: ",
        paste(encoded, collapse = ", "),
        "."
      )
    )
  }
  X <- do.call(cbind, unname(parts))
  feature_map <- do.call(rbind, unname(maps))
  names(X) <- make.unique(feature_map$feature)
  feature_map$feature <- names(X)
  rownames(feature_map) <- NULL

  # Something to compare. There is no minimum sample size, but small samples
  # are noted: with few rows, honest trees can make few splits and estimates
  # are very noisy. The threshold matches effect_by()'s default group size.
  if (length(unique(d[[y]])) < 2) {
    stop("'", y, "' has only one value in the usable rows.")
  }
  positive <- if (is.null(weights)) rep(TRUE, nrow(d)) else d[[weights]] > 0
  if (length(unique(d[[w]])) < 2) {
    stop("'", w, "' has only one value in the usable rows.")
  }
  if (length(unique(d[[w]][positive])) < 2) {
    stop(
      "Weights must be positive for rows with ",
      if (w_type == "binary") "both values" else "more than one value",
      " of '",
      w,
      "'."
    )
  }
  small <- 10
  if (w_type == "binary") {
    sizes <- c(sum(d[[w]][positive] == 0), sum(d[[w]][positive] == 1))
    few <- sizes < small
    if (any(few)) {
      notes <- c(
        notes,
        paste0(
          "Only ",
          and_or(
            sprintf(
              "%s %s %s = '%s'",
              fmt_n(sizes[few]),
              ifelse(sizes[few] == 1, "row has", "rows have"),
              w,
              c(zero, one)[few]
            ),
            "and"
          ),
          ". With fewer than ",
          small,
          " rows in a group, estimates are very noisy."
        )
      )
    }
  } else if (sum(positive) < small) {
    notes <- c(
      notes,
      paste0(
        "Only ",
        fmt_n(sum(positive)),
        " rows are used. With fewer than ",
        small,
        ", estimates are very noisy."
      )
    )
  }
  cluster_ids <- if (is.null(clusters)) {
    NULL
  } else {
    as.integer(factor(d[[clusters]]))
  }
  if (!is.null(cluster_ids) && length(unique(cluster_ids)) < 2) {
    stop("At least two clusters are needed.")
  }

  list(
    Y = d[[y]],
    W = d[[w]],
    X = X,
    covariates = covariates,
    feature_map = feature_map,
    missingness = missingness,
    row_index = row_ids,
    ids = if (is.null(ids)) row_ids else d[[ids]],
    weights = if (is.null(weights)) NULL else d[[weights]],
    clusters = cluster_ids,
    n = nrow(d),
    n_dropped = n_start - nrow(d),
    y_type = y_type,
    w_type = w_type,
    y_labels = if (y_type == "binary") c(`0` = y0, `1` = y1),
    w_labels = if (w_type == "binary") c(`0` = zero, `1` = one),
    notes = notes
  )
}
