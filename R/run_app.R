#' Launch the heterogeneity app
#'
#' A point-and-click version of the four-step workflow, which the page lists
#' at the outset. Step 1: upload a CSV, TSV, Excel, SPSS (.sav) or Stata (.dta)
#' file, or try the GSS example (downloaded with [load_gss2024()]), and choose
#' the dependent variable (Y) and the independent variable (W), each binary
#' (any two values) or continuous, and the characteristics (X), all shown
#' together. Each
#' later step appears whole, and its Continue button opens the next: step 2,
#' the average treatment effect and three plots of the estimated treatment
#' effects; step 3, average treatment effects within groups; step 4, a
#' plain-language summary and each row's estimated treatment effect as a CSV
#' file. Nothing is saved:
#' Shiny deletes the uploaded file when the session ends, and refreshing the
#' page starts afresh. The app detects
#' delimiters, text encodings and a missing header row, and uses SPSS/Stata
#' variable and value labels as readable names. Technical terms and field
#' labels show short explanations on hover; longer ones, such as how to read
#' each plot, sit in small expanders. A switch in the header changes between
#' light and dark mode. Figures match the package's plotting functions.
#' Requires \pkg{shiny} and \pkg{bslib} (installed with shiny); Excel uploads
#' also require \pkg{readxl}, SPSS/Stata uploads \pkg{haven} and the example
#' \pkg{gssr}.
#'
#' @param ... Passed to [shiny::runApp()], for example `port` or
#'   `launch.browser`.
#' @return Called for its side effect of running the app.
#' @export
run_app <- function(...) {
  for (p in c("shiny", "bslib")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is needed for the app; install it with install.packages(\"", p, "\").")
    }
  }
  app_dir <- system.file("app", package = "heterogeneity")
  if (app_dir == "") stop("The app folder was not found; reinstall the package.")
  shiny::runApp(app_dir, display.mode = "normal", ...)
}
