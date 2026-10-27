#' Load the GSS 2024 example data
#'
#' Reads the teaching example used in the tutorial and the app, which ships
#' with the package as `gss_demo.csv` and `gss_demo_codebook.csv` (in
#' `system.file("extdata", package = "heterogeneity")`), so no download is
#' needed. It holds 3,721 respondents to the 2024 General Social Survey
#' (NORC's Release 2, including the AmeriSpeak oversample of Black, Hispanic,
#' and Asian American, Native Hawaiian and Pacific Islander respondents) with a
#' value for both the 0-100 rating of Donald Trump (`rating_trump`) and
#' conservative self-identification (`conservative`: 1 = slightly to extremely
#' conservative, 0 = liberal or moderate), and 39 demographic, socioeconomic,
#' religious, attitudinal and wellbeing characteristics. For other analyses it
#' also holds further dependent variables (ratings of Kamala Harris, Joe Biden,
#' the Democratic and Republican parties and MAGA Republicans, the rating of
#' the economy and the preferred immigration level) and a further independent
#' variable (a college degree). Missing values stay missing (the forest
#' handles them). Unweighted results describe these respondents; the optional
#' `svy_weight` describes U.S. adults.
#'
#' The files are built from NORC's release by `data-raw/gss_demo.R` in the
#' package's repository, which defines every recode by the meaning of the GSS
#' category labels and checks them, so a changed category stops the build.
#' The codebook gives each column's coding, its GSS source item and notes,
#' such as who was not asked a question.
#'
#' @return A list with `data` (one row per respondent: `id`, the dependent
#'   variable `rating_trump`, the independent variable `conservative`, the
#'   characteristics, the further dependent and independent variables, and the
#'   optional `svy_weight` and `psu_cluster`), `covariates` (names of the
#'   characteristics), `labels` (short readable names), `value_labels`
#'   (category names for coded items), `codebook` (variable, label, role,
#'   coding, GSS source item and label, notes, and `same_as`: columns that
#'   measure the same thing) and `effect_label` (an axis label with units).
#' @source NORC at the University of Chicago, [General Social Survey
#'   2024](https://gss.norc.org/get-the-data.html), prepared with
#'   [gssr](https://kjhealy.github.io/gssr/). Cite the GSS when you use these
#'   data.
#' @examples
#' gss <- load_gss2024()
#' dim(gss$data)
#' gss$codebook[1:5, c("variable", "label", "coding")]
#' @export
load_gss2024 <- function() {
  path <- function(file) {
    p <- system.file("extdata", file, package = "heterogeneity")
    if (!nzchar(p)) {
      stop(
        "The GSS example file ",
        file,
        " is missing from the installed package.",
        call. = FALSE
      )
    }
    p
  }
  data <- utils::read.csv(path("gss_demo.csv"), na.strings = "")
  codebook <- utils::read.csv(
    path("gss_demo_codebook.csv"),
    na.strings = character(),
    colClasses = "character"
  )
  if (!identical(codebook$variable, names(data))) {
    stop("The GSS example's codebook does not match its data.", call. = FALSE)
  }
  # Category names, stored as "code=label|code=label".
  coded <- codebook[nzchar(codebook$values), , drop = FALSE]
  value_labels <- lapply(
    strsplit(coded$values, "|", fixed = TRUE),
    function(pairs) {
      split <- regexpr("=", pairs, fixed = TRUE)
      stats::setNames(
        substring(pairs, split + 1),
        substring(pairs, 1, split - 1)
      )
    }
  )
  names(value_labels) <- coded$variable
  list(
    data = data,
    covariates = codebook$variable[codebook$role == "characteristic"],
    labels = stats::setNames(codebook$label, codebook$variable),
    value_labels = value_labels,
    codebook = codebook[c(
      "variable",
      "label",
      "role",
      "coding",
      "source",
      "gss_label",
      "note",
      "same_as"
    )],
    effect_label = "Treatment effect on Trump rating (points)"
  )
}
