#' Launch the heterogeneity app
#'
#' A point-and-click version of the workflow, in one step: upload a CSV, TSV,
#' Excel, SPSS (.sav) or Stata (.dta) file, or try the GSS example (included
#' with the package; see [load_gss2024()]), see its first 10 rows, choose the
#' dependent variable (Y) and the independent variable (W), each binary (any two
#' values) or continuous, and the characteristics (X), split into candidate
#' moderators and optional controls, all shown together, and run the causal
#' forest. One progress note shows until every plot is drawn; every result then
#' appears on the same page: the average treatment effect, the distribution of
#' estimated treatment effects, the candidate moderators ranked by importance, a
#' plot of the treatment effect for each candidate moderator (with an optional
#' second characteristic for a heatmap), and a plain-language summary with each
#' row's estimated treatment effect as a CSV file, one row per row of the
#' uploaded file. Nothing is saved: Shiny deletes the uploaded file when the
#' session ends, and refreshing the page starts afresh. The app detects
#' delimiters, text encodings and a missing header row, and uses SPSS/Stata
#' variable and value labels as readable names. Technical terms and field labels
#' show short explanations on hover; longer ones, such as how to read each plot,
#' sit in small expanders. A switch in the header changes between light and dark
#' mode. Figures match the package's plotting functions. Requires \pkg{shiny}
#' and \pkg{bslib} (installed with shiny); Excel uploads also require
#' \pkg{readxl}, and SPSS/Stata uploads \pkg{haven}.
#'
#' @param ... Passed to [shiny::runApp()], for example `port` or
#'   `launch.browser`.
#' @return Called for its side effect of running the app.
#' @export
run_app <- function(...) {
  for (p in c("shiny", "bslib")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is needed for the app; install it with install.packages(\"%s\").",
        p,
        p
      ))
    }
  }
  app_dir <- system.file("app", package = "heterogeneity")
  if (app_dir == "") {
    stop("The app folder was not found; reinstall the package.")
  }
  shiny::runApp(app_dir, display.mode = "normal", ...)
}
