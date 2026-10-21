#' Load the GSS 2024 example data
#'
#' Downloads the 2024 General Social Survey with the \pkg{gssr} package and
#' prepares the teaching example used in the tutorial and the app:
#' a 0-100 rating of Donald Trump (`rating_trump`), conservative
#' self-identification (`conservative`: 1 = slightly to extremely
#' conservative, 0 = liberal or moderate) and demographic, socioeconomic,
#' religious, attitudinal and wellbeing characteristics. Nothing is stored in
#' the package; the download is kept in memory for the R session.
#'
#' Every recode is defined by the meaning of the GSS category labels, not by
#' assumed codes, and the labels are checked: if a later GSS release changes a
#' category, loading stops and names the item. Codes labelled as missing
#' (don't know, no answer, skipped and so on) become `NA`. Missing
#' characteristics stay missing (the forest handles them); only respondents
#' missing the rating or political views are left out. Rerun the analysis after
#' a new GSS release, as the numbers can change.
#'
#' @param raw Optional: the data frame returned by `gssr::gss_get_yr(2024)`,
#'   to avoid downloading again.
#' @param refresh Download again even if the data were loaded in this session.
#' @return A list with `data` (one row per respondent: `id`, the dependent
#'   variable `rating_trump`, the independent variable `conservative`, the
#'   characteristics, and the optional `svy_weight` and `psu_cluster`), `covariates` (names of the characteristics), `labels`
#'   (short readable names), `value_labels` (category names for coded items),
#'   `codebook` (variable, label, coding and source GSS item) and
#'   `effect_label` (an axis label with units).
#' @source NORC, [General Social Survey](https://gss.norc.org/get-the-data.html),
#'   via [gssr](https://kjhealy.github.io/gssr/). Install gssr with
#'   `install.packages("gssr", repos = c("https://kjhealy.r-universe.dev", "https://cloud.r-project.org"))`.
#' @examples
#' \donttest{
#' if (requireNamespace("gssr", quietly = TRUE)) {
#'   gss <- load_gss2024()
#'   head(gss$codebook)
#' }
#' }
#' @export
load_gss2024 <- function(raw = NULL, refresh = FALSE) {
  if (is.null(raw)) {
    raw <- if (!refresh) gss_cache$raw
    if (is.null(raw)) {
      if (!requireNamespace("gssr", quietly = TRUE)) {
        stop("The GSS example needs the gssr package: install.packages(\"gssr\", repos = ",
             "c(\"https://kjhealy.r-universe.dev\", \"https://cloud.r-project.org\"))", call. = FALSE)
      }
      raw <- tryCatch(gssr::gss_get_yr(2024), error = function(e) {
        stop("The GSS 2024 file could not be downloaded (", conditionMessage(e), "). Check the internet connection.", call. = FALSE)
      })
      gss_cache$raw <- raw
    }
  }
  clean_gss2024(raw)
}

gss_cache <- new.env(parent = emptyenv())

# ---- The example: each item's source, short label and recode -----------------

# Recodes by label meaning. scale(): ordered categories, one pattern per new
# code, in order. flag(): 1 for categories matching `yes`, 0 for `no` (default:
# every other category). number(): numeric values, optionally within limits.
# ordinal(): labelled codes kept as they are. Patterns are case-insensitive.
gss_scale <- function(levels, start = 1) list(type = "scale", levels = levels, start = start)
gss_flag <- function(yes = NULL, no = NULL, labels = c("No", "Yes")) list(type = "flag", yes = yes, no = no, labels = labels)
gss_number <- function(min = -Inf, max = Inf) list(type = "number", min = min, max = max)
gss_ordinal <- function() list(type = "ordinal")

gss2024_items <- function() {
  yes_no <- gss_flag("^yes$", "^no$")
  degree <- gss_scale(c("less than high school", "^high school$", "junior college", "bachelor", "graduate"), start = 0)
  list(
    rating_trump = list("ratechall124", "Rating of Donald Trump", gss_number(0, 100)),
    conservative = list("polviews", "Conservative", gss_flag("conservative", "liberal|moderate", c("Liberal/moderate", "Conservative"))),
    white = list("raceacs1", "White", yes_no),
    black = list("raceacs2", "Black", yes_no),
    hispanic = list("hispanic", "Hispanic", gss_flag(no = "^not hispanic")),
    age = list("age", "Age", gss_number()),
    educ = list("educ", "Years of education", gss_number()),
    income16 = list("income16", "Family income", gss_ordinal()),
    madeg = list("madeg", "Mother's education", degree),
    padeg = list("padeg", "Father's education", degree),
    prestg10 = list("prestg10", "Occupational prestige", gss_number()),
    adults = list("adults", "Adults in household", gss_number()),
    childs = list("childs", "Number of children", gss_number()),
    mobile16 = list("mobile16", "Moved since age 16", gss_scale(c("same city", "different city", "different state"))),
    born = list("born", "Born in the US", yes_no),
    attend = list("attend", "Religious attendance", gss_scale(c("^never$", "less than once a year", "once or twice a year",
      "several times a year", "once a month", "2-3 times a month", "nearly every week", "^every week$", "several times a week"), start = 0)),
    pray = list("pray", "Frequency of prayer", gss_scale(c("^never$", "less than once a week", "^once a week$",
      "several times a week", "once a day", "several times a day"), start = 0)),
    lkelyvot = list("lkelyvot", "Likelihood of voting", gss_scale(c("not at all likely", "not very likely", "somewhat likely",
      "^very likely$", "extremely likely"))),
    compuse = list("compuse", "Uses a computer", yes_no),
    othlang = list("othlang", "Speaks another language", yes_no),
    diagnosd = list("diagnosd", "Mental health diagnosis", yes_no),
    raclive = list("raclive", "Other race in neighbourhood", yes_no),
    vetyears = list("vetyears", "Years in armed forces", gss_scale(c("no active duty", "less than 2 years", "2-4 years",
      "more than 4 years"), start = 0)),
    happy = list("happy", "Happiness", gss_scale(c("not too happy", "pretty happy", "^very happy$"))),
    health = list("health", "Self-rated health", gss_scale(c("^poor$", "^fair$", "^good$", "^excellent$"))),
    finalter = list("finalter", "Change in finances", gss_scale(c("worse", "same", "better"))),
    satfin = list("satfin", "Financial satisfaction", gss_scale(c("not satisfied at all", "more or less", "pretty well"))),
    satdemoc = list("satdemoc", "Satisfaction with democracy", gss_scale(c("not at all satisfied", "not very satisfied",
      "fairly satisfied", "^very satisfied$"))),
    female = list("sex", "Female", gss_flag("^female$", "^male$")),
    lgb = list("sexornt", "Lesbian, gay or bisexual", gss_flag("gay|lesbian|homosexual|bisexual", "heterosexual|straight")),
    married = list("marital", "Married", gss_flag("^married$")),
    rural = list("res16", "Rural/small-town upbringing", gss_flag("open country|farm|small city or town")),
    midwest = list("region", "Lives in the Midwest", gss_flag("^midwest$"))
  )
}

# ---- Cleaning -------------------------------------------------------------------

clean_gss2024 <- function(raw) {
  raw <- as.data.frame(raw)
  items <- gss2024_items()
  needed <- c("id", vapply(items, `[[`, "", 1), "wtssnrps_as", "vstrat", "vpsu")
  absent <- setdiff(needed, names(raw))
  if (length(absent)) stop("The GSS file lacks: ", paste(absent, collapse = ", "), ".", call. = FALSE)
  recoded <- lapply(names(items), function(v) recode_gss(raw[[items[[v]][[1]]]], items[[v]][[3]], items[[v]][[1]]))
  names(recoded) <- names(items)
  d <- data.frame(id = plain_values(raw$id), lapply(recoded, `[[`, "values"), check.names = FALSE)
  d$svy_weight <- plain_values(raw$wtssnrps_as)
  d$svy_weight[!is.finite(d$svy_weight) | d$svy_weight < 0] <- NA_real_
  stratum <- plain_values(raw$vstrat); psu <- plain_values(raw$vpsu)
  d$psu_cluster <- ifelse(is.na(stratum) | is.na(psu), NA_integer_, as.integer(factor(paste(stratum, psu))))
  d <- d[!is.na(d$rating_trump) & !is.na(d$conservative), , drop = FALSE]
  d <- d[order(d$id), , drop = FALSE]; rownames(d) <- NULL
  covariates <- setdiff(names(items), c("rating_trump", "conservative"))
  labels <- c(id = "Respondent ID", vapply(items, `[[`, "", 2), svy_weight = "Survey weight", psu_cluster = "Sampling cluster")
  value_labels <- Filter(Negate(is.null), lapply(recoded, `[[`, "labels"))
  codebook <- data.frame(variable = names(d), label = unname(labels[names(d)]),
    coding = vapply(names(d), function(v) gss_coding(v, d[[v]], recoded[[v]]), ""),
    source = vapply(names(d), function(v) switch(v, id = "id", svy_weight = "wtssnrps_as", psu_cluster = "vstrat x vpsu",
      items[[v]][[1]]), ""), row.names = NULL)
  list(data = d, covariates = covariates, labels = labels[names(d)], value_labels = value_labels,
       codebook = codebook, effect_label = "Treatment effect on Trump rating (points)")
}

# Numbers without haven classes; negative codes and codes labelled as missing become NA.
plain_values <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  v <- unclass(x); attributes(v) <- NULL
  v <- suppressWarnings(as.numeric(v))
  missing_codes <- labels[!is.na(labels) & grepl(gss_missing_pattern, names(labels), ignore.case = TRUE)]
  v[!is.finite(v) | v < 0 | v %in% missing_codes] <- NA_real_
  v
}
gss_missing_pattern <- "don'?t know|\\bdk\\b|no answer|\\bna\\b|\\biap\\b|inapplicable|refused|skipped|not available|uncodeable|not imputable|see codebook|can'?t choose|cannot choose"

# Substantive categories: labelled codes that are not missing-value codes.
gss_categories <- function(x, item) {
  labels <- attr(x, "labels", exact = TRUE)
  labels <- labels[!is.na(labels) & labels >= 0 & !grepl(gss_missing_pattern, names(labels), ignore.case = TRUE)]
  if (!length(labels)) stop("GSS item ", item, " has no category labels; its coding may have changed.", call. = FALSE)
  labels
}

recode_gss <- function(x, rule, item) {
  v <- plain_values(x)
  changed <- function(what) stop("GSS item ", item, ": ", what, ". The GSS coding may have changed; update gss2024_items() in R/gss.R.", call. = FALSE)
  matching <- function(categories, pattern) categories[grepl(pattern, names(categories), ignore.case = TRUE)]
  cap <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))
  switch(rule$type,
    number = {
      v[v < rule$min | v > rule$max] <- NA_real_
      list(values = v, labels = NULL)
    },
    ordinal = {
      categories <- gss_categories(x, item)
      v[!v %in% categories] <- NA_real_
      list(values = v, labels = stats::setNames(cap(names(categories)), categories))
    },
    scale = {
      categories <- gss_categories(x, item)
      found <- lapply(rule$levels, function(p) matching(categories, p))
      counts <- lengths(found)
      if (any(counts != 1L)) changed(sprintf("expected one category matching '%s', found %d", rule$levels[counts != 1L][1], counts[counts != 1L][1]))
      unmatched <- setdiff(categories, unlist(found))
      if (length(unmatched)) changed(paste0("unexpected category '", names(categories)[categories %in% unmatched][1], "'"))
      codes <- rule$start + seq_along(found) - 1
      out <- rep(NA_real_, length(v))
      for (k in seq_along(found)) out[which(v == found[[k]])] <- codes[k]
      list(values = out, labels = stats::setNames(cap(vapply(found, names, "")), codes))
    },
    flag = {
      categories <- gss_categories(x, item)
      yes <- if (is.null(rule$yes)) categories[!categories %in% matching(categories, rule$no)] else matching(categories, rule$yes)
      no <- if (is.null(rule$no)) categories[!categories %in% yes] else matching(categories, rule$no)
      if (!length(yes) || !length(no) || any(yes %in% no)) changed("the categories for 1 and 0 could not be told apart")
      unmatched <- setdiff(categories, c(yes, no))
      if (length(unmatched)) changed(paste0("unexpected category '", names(categories)[categories %in% unmatched][1], "'"))
      out <- ifelse(v %in% yes, 1, ifelse(v %in% no, 0, NA_real_))
      list(values = out, labels = c(`0` = rule$labels[1], `1` = rule$labels[2]), yes = names(yes), no = names(no))
    })
}

# A one-line description of each column's coding, written from the data.
gss_coding <- function(v, values, recoded) {
  if (v == "id") return("GSS respondent identifier")
  if (v == "svy_weight") return("Post-stratification and nonresponse weight for the full sample; optional")
  if (v == "psu_cluster") return("Sampling stratum x primary sampling unit; optional")
  if (!is.null(recoded$yes)) return(sprintf("1 = %s; 0 = %s", paste(recoded$yes, collapse = " / "), paste(recoded$no, collapse = " / ")))
  labels <- recoded$labels
  if (is.null(labels)) {
    range <- range(values, na.rm = TRUE)
    return(sprintf("Numeric, %s to %s", format(range[1]), format(range[2])))
  }
  if (length(labels) > 10) return(sprintf("%d ordered categories: %s = %s to %s = %s", length(labels),
                                          names(labels)[1], labels[1], names(labels)[length(labels)], labels[length(labels)]))
  paste(sprintf("%s = %s", names(labels), labels), collapse = ", ")
}
