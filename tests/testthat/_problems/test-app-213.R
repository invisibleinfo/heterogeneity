# Extracted from test-app.R:213

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "heterogeneity", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
app_env <- function() {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  withr::local_options(
    shiny.maxRequestSize = getOption("shiny.maxRequestSize"),
    .local_envir = parent.frame()
  )
  env <- new.env(parent = globalenv())
  sys.source(
    system.file("app", "app.R", package = "heterogeneity"),
    envir = env
  )
  env
}

# test -------------------------------------------------------------------------
app <- app_env()
set.seed(42)
d <- data.frame(
  y = rnorm(240),
  arm = rep(c("control", "treated"), 120),
  age = rnorm(240),
  region = rep(c("North", "South", "East"), 80),
  school = rep(letters[1:20], 12),
  weight = rep(c(1, 2), 120)
)
d$age[1:40] <- NA
path <- tempfile(fileext = ".csv")
on.exit(unlink(path), add = TRUE)
utils::write.csv(d, path, row.names = FALSE)
shiny::testServer(app$server, {
  session$setInputs(file = list(datapath = path, name = "upload.csv"))
  expect_equal(rv$source, "upload")
  # Every field is shown at once, ending with the one button that runs the
  # forest.
  fields <- output$fields$html
  for (label in c(
    "Dependent variable (Y)",
    "Independent variable (W)",
    "Candidate moderators (X)",
    "Controls (X, optional)",
    "Change default parameters",
    "Run the causal forest"
  )) {
    expect_match(fields, label, fixed = TRUE)
  }
  expect_false(grepl("Optional settings|Continue to step", fields))
  session$setInputs(run = 1)
  expect_null(rv$res)
  expect_match(
    rv$run_note,
    "Choose a dependent variable (Y), an independent variable (W) and at least one candidate moderator (X) first.",
    fixed = TRUE
  )
  session$setInputs(
    y = "y",
    w = "arm",
    treated = "treated",
    x = c("age", "region"),
    ids = "",
    weights = "weight",
    clusters = "school",
    num_trees = 100,
    seed = 21042020
  )
  expect_true(w_needs_coding())
  session$setInputs(run = 2)
  expect_null(rv$error)
  expect_null(rv$run_note)
  r <- rv$res$report
  expect_s3_class(r, "heterogeneity_report")
  expect_equal(sum(is.na(r$X$age)), 40L)
  expect_true(r$settings$weighted)
  expect_equal(r$settings$w_labels, c(`0` = "control", `1` = "treated"))
  # One page of results: the summary, the histogram, the importance ranking, a
  # plot for each candidate moderator in order of importance, and the export.
  html <- paste(as.character(results_ui(facts())))
  for (id in c(
    "unit-summary",
    "unit-spread",
    "unit-characteristics",
    "unit-moderators",
    "unit-export",
    "moderator-1",
    "moderator-2",
    "dl_cates"
  )) {
    expect_match(html, id)
  }
  expect_false(grepl(
    "unit-profiles|quintile|Continue to step|next_step",
    html
  ))
  expect_match(html, "Second characteristic (optional)", fixed = TRUE)
  expect_match(html, "used for the forest and all averages")
  expect_match(html, "candidate moderators")
  expect_match(html, "run from")
  # The key sentence gives the average treatment effect in Y's units and in
  # standard deviations; there is no summary table.
  expect_match(
    html,
    'average treatment effect<span class="tip" role="tooltip">',
    fixed = TRUE
  )
  expect_match(
    html,
    sprintf(
      '</span></span> of <code>arm</code> on <code>y</code> is <b>%s</b> (<b>%s</b> <span class="term" tabindex="0">standard deviations',
      app$fmt(r$ate$estimate),
      app$fmt(r$ate$standardized)
    ),
    fixed = TRUE
  )
  # Results are drawn hidden and shown once their plots are ready; notes carry
  # no count.
  expect_match(html, 'class="sec pending" id="results-section"', fixed = TRUE)
  expect_match(html, "<summary>Notes on the data</summary>", fixed = TRUE)
  expect_match(
    html,
    'conditional on its covariates (called a <a href="https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/hte-i-binary-treatment.html"',
    fixed = TRUE
  )
  expect_match(html, "doubly robust")
  expect_false(grepl(
    "Table of groups|effect_table|Copy text|<table|Quantity|Central 90|Mean prediction by group",
    html
  ))
  expect_match(
    paste(as.character(report_text(facts()))),
    "Average treatment effect: </b>",
    fixed = TRUE
  )
  expect_match(
    paste(as.character(report_text(facts()))),
    "standard deviations).",
    fixed = TRUE
  )
  expect_match(
    spread_text(facts())$long,
    "teal bars, above 0, mark effects where"
  )
  expect_match(spread_text(facts())$long, "the dashed line marks 0")
  expect_match(
    spread_text(facts())$short,
    "around the average treatment effect of"
  )
  for (plot in c("dist", "importance")) {
    expect_type(output[[plot]], "list")
  }
  # Dark mode recolours a copy of each plot; the cached plot keeps its
  # colours.
  session$setInputs(mode = "dark")
  dark_bars <- for_page(importance_plot())
  expect_identical(dark_bars$layers[[1]]$aes_params$fill, "#72a9ac")
  expect_identical(importance_plot()$layers[[1]]$aes_params$fill, "#286b70")
  expect_identical(
    for_page(plot(effect_by(r, "age")))$layers[[3]]$aes_params$colour,
    "#e4e4e7"
  )
  expect_type(output$importance, "list")
  session$setInputs(mode = "light")
  expect_identical(
    for_page(importance_plot())$layers[[1]]$aes_params$fill,
    "#286b70"
  )
  expect_equal(importance_height(), 260)
  # Each candidate moderator has its own plot and finding.
  vars <- r$moderators$variable
  for (k in seq_along(vars)) {
    expect_type(output[[paste0("effect_", k)]], "list")
  }
  age <- which(vars == "age")
  region <- which(vars == "region")
  expect_match(
    output[[paste0("effect_text_", age)]]$html,
    "Following the line, the average treatment effect is lowest at"
  )
  expect_match(
    output[[paste0("effect_text_", region)]]$html,
    "lowest for <code>region = [A-Za-z]+</code> .* and highest for"
  )
  expect_match(
    output[[paste0("effect_text_", region)]]$html,
    sprintf(
      'class="footnote">The average treatment effect is %s.',
      app$fmt(r$ate$estimate)
    ),
    fixed = TRUE
  )
  help_text <- moderator_help(facts())
  for (phrase in c(
    "doubly robust",
    "using the supplied weights",
    "dashed line marks a treatment effect of 0",
    "or with only one group of W",
    "three <span"
  )) {
    expect_match(help_text, phrase, fixed = TRUE)
  }
  # A second characteristic turns a plot into a heatmap, whose cells (about
  # 22 rows each) are compared; clearing it restores the groups.
  do.call(
    session$setInputs,
    stats::setNames(list("age"), paste0("second_", region))
  )
  expect_match(output[[paste0("effect_text_", region)]]$html, "lowest for")
  expect_type(output[[paste0("effect_", region)]], "list")
  # Groups too small to estimate are not compared.
  expect_match(
    effect_short(effect_by(r, "region", "age", min_n = 100)),
    "Too few groups could be estimated (each needs at least 10 observations",
    fixed = TRUE
  )
  do.call(
    session$setInputs,
    stats::setNames(list(""), paste0("second_", region))
  )
  expect_match(output[[paste0("effect_text_", region)]]$html, "lowest for")
  # The export has one row per row of the file, in order.
  out <- export_table()
  expect_named(out, c("row", "estimated_treatment_effect"))
  expect_identical(out$row, seq_len(nrow(d)))
  expect_match(
    export_note(facts()),
    "one row per row of your data, in the same order, with the row number and the estimated treatment effect.",
    fixed = TRUE
  )
  summary_text <- paste(as.character(report_text(facts())))
  expect_match(summary_text, "Averages use the supplied weights")
  expect_false(grepl(
    "Data: |Without weights|kept for the forest",
    summary_text
  ))
  expect_match(summary_text, "part of this spread could be estimation noise")
  expect_false(grepl("p-value|confidence|Holm|AUTOC", summary_text))
  session$setInputs(seed = 2)
  expect_null(rv$res)
  session$setInputs(run = 3)
  expect_s3_class(rv$res$report, "heterogeneity_report")
  session$setInputs(weights = "")
  expect_null(rv$res)
  session$setInputs(num_trees = 1)
  session$setInputs(run = 4)
  expect_null(rv$res)
  expect_match(rv$error, "whole number")
})
