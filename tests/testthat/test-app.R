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

test_that("an upload runs in one step, shows every result, keeps missing values and clears stale results", {
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
    # A second characteristic turns a plot into a heatmap; here its nine cells
    # are too small to estimate, and clearing it restores the groups.
    do.call(
      session$setInputs,
      stats::setNames(list("age"), paste0("second_", region))
    )
    expect_match(
      output[[paste0("effect_text_", region)]]$html,
      "Too few groups could be estimated"
    )
    expect_type(output[[paste0("effect_", region)]], "list")
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
})

test_that("the export keeps every row of the file, with the ID column under its own name", {
  app <- app_env()
  set.seed(43)
  d <- data.frame(
    pid = sprintf("p%03d", 1:200),
    y = rnorm(200),
    arm = rep(0:1, 100),
    age = rnorm(200)
  )
  d$y[c(5, 9, 150)] <- NA
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = path, name = "ids.csv"))
    session$setInputs(
      y = "y",
      w = "arm",
      x = "age",
      ids = "pid",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 1
    )
    session$setInputs(run = 1)
    out <- export_table()
    expect_named(out, c("row", "pid", "estimated_treatment_effect"))
    expect_equal(nrow(out), nrow(d))
    expect_identical(out$pid, d$pid)
    expect_identical(
      which(is.na(out$estimated_treatment_effect)),
      c(5L, 9L, 150L)
    )
    expect_match(
      export_note(facts()),
      "with the row number, your ID column (pid) and the estimated treatment effect",
      fixed = TRUE
    )
    expect_match(
      export_note(facts()),
      "it is empty for the 3 rows not analysed because Y or W was missing.",
      fixed = TRUE
    )
  })
})

test_that("the GSS example link downloads the data and fills in readable labels", {
  app <- app_env()
  shiny::testServer(app$server, {
    session$setInputs(example = 1)
    expect_true(example())
    expect_identical(data(), load_gss2024()$data)
    session$setInputs(
      y = "rating_trump",
      w = "conservative",
      x = c("lkelyvot", "age", "income16"),
      ids = "id",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 21042020
    )
    expect_false(w_needs_coding())
    session$setInputs(run = 1)
    r <- rv$res$report
    expect_identical(r$settings$effect_label, load_gss2024()$effect_label)
    expect_identical(r$labels[["lkelyvot"]], "Likelihood of voting")
    expect_identical(r$cates$id, load_gss2024()$data$id)
    expect_match(
      paste(as.character(results_ui(facts()))),
      "higher for Conservative than for Liberal/moderate"
    )
    expect_identical(names(export_table())[2], "id")
  })
})

test_that("the interface has the title, hover explanations and only the chosen settings", {
  app <- app_env()
  html <- as.character(app$ui)
  expect_match(html, "Exploring heterogeneity with causal forests")
  expect_match(html, 'class="term"')
  expect_match(html, "What your file needs")
  expect_match(html, '<details class="fold">', fixed = TRUE)
  expect_match(html, "bslib-input-dark-mode")
  expect_match(html, "dependent variable")
  expect_match(html, "are not stored; refreshing the page deletes them")
  expect_match(html, "R package and application")
  expect_match(html, "</span></span> in treatment effects.</p>", fixed = TRUE)
  expect_match(
    html,
    "The estimates can be noisier for smaller samples.",
    fixed = TRUE
  )
  expect_match(html, 'href="https://github.com/invisibleinfo/heterogeneity"')
  expect_false(grepl(
    "Glossary|accordion-glossary|known_probability|w_hat|Missing characteristic values|wikipedia|Learn more|At least 50",
    html,
    ignore.case = TRUE
  ))
  # One step: choose the variables and run the forest. There is no footer.
  expect_match(html, "Choose variables")
  expect_false(grepl('class="flow"|four-step|Compare groups|sec-num', html))
  expect_false(grepl(
    "built on grf|Code and tutorial on GitHub|class=\"footer\"",
    html
  ))
  expect_true(
    is.character(app$terms) &&
      all(nzchar(app$terms)) &&
      !any(grepl("http", app$terms))
  )
})

test_that("a numeric comparison runs as slopes in the app", {
  app <- app_env()
  set.seed(5)
  d <- data.frame(
    score = rnorm(200),
    hours = runif(200, 0, 10),
    age = rnorm(200)
  )
  d$score <- d$score + d$hours * (0.2 + 0.3 * (d$age > 0))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = path, name = "dose.csv"))
    session$setInputs(
      y = "score",
      w = "hours",
      x = "age",
      ids = "",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 21042020
    )
    expect_false(w_needs_coding())
    session$setInputs(run = 1)
    expect_null(rv$error)
    expect_identical(rv$res$report$settings$w_type, "continuous")
    expect_match(
      paste(as.character(results_ui(facts()))),
      "per one-unit increase in"
    )
    expect_match(spread_text(facts())$long, "rises with")
    expect_match(output$effect_text_1$html, "Following the line")
    expect_match(
      moderator_help(facts()),
      "or in which W does not vary",
      fixed = TRUE
    )
    expect_false(any(grepl(
      "n_control|n treated",
      names(effect_by(report(), "age"))
    )))
    expect_match(
      paste(as.character(report_text(facts()))),
      "per one-unit increase in hours"
    )
  })
})

test_that("uploads with odd delimiters, encodings, headers, blanks and labels are read", {
  app <- app_env()
  dir <- withr::local_tempdir()
  base <- data.frame(
    score = c(1.5, 2, 3.25, 4),
    group = c("a", "b", "a", "b"),
    city = c("K\u00f6ln", "Paris", "Z\u00fcrich", "Oslo")
  )
  semi <- file.path(dir, "semi.csv")
  utils::write.table(base, semi, sep = ";", row.names = FALSE, quote = FALSE)
  out <- app$read_upload(semi, "semi.csv")
  expect_identical(names(out$data), names(base))
  expect_equal(out$data$score, base$score)
  tsv <- file.path(dir, "tabs.tsv")
  utils::write.table(base, tsv, sep = "\t", row.names = FALSE)
  expect_equal(app$read_upload(tsv, "tabs.tsv")$data$city, base$city)
  latin <- file.path(dir, "latin.csv")
  utils::write.csv(base, latin, row.names = FALSE, fileEncoding = "latin1")
  latin_out <- app$read_upload(latin, "latin.csv")
  expect_equal(latin_out$data$city, base$city)
  expect_match(paste(latin_out$notes, collapse = " "), "Latin-1")
  bare <- file.path(dir, "bare.csv")
  writeLines(c("1,2,3", "4,5,6", "7,8,9"), bare)
  bare_out <- app$read_upload(bare, "bare.csv")
  expect_identical(names(bare_out$data), c("column1", "column2", "column3"))
  expect_equal(nrow(bare_out$data), 3L)
  messy <- file.path(dir, "messy.csv")
  writeLines(
    c("id,,score,empty", "1,x,2.5,", ",,,", "2,y,N/A,", "3,z,3.5,"),
    messy
  )
  messy_out <- app$read_upload(messy, "messy.csv")
  expect_identical(names(messy_out$data), c("id", "column2", "score"))
  # Empty rows stay, so exported rows line up with the file; the analysis leaves
  # them out.
  expect_equal(nrow(messy_out$data), 4L)
  expect_true(all(is.na(messy_out$data[2, ])))
  expect_true(is.na(messy_out$data$score[3]))
  expect_match(paste(messy_out$notes, collapse = " "), "empty column")
  empty <- file.path(dir, "empty.csv")
  file.create(empty)
  expect_error(app$read_upload(empty, "empty.csv"), "empty")
  expect_error(app$read_upload(semi, "data.json"), "please upload")
  skip_if_not_installed("haven")
  lab <- data.frame(y = rnorm(120), w = rep(1:2, 60), age = rnorm(120))
  lab$w <- haven::labelled(
    lab$w,
    labels = c(Control = 1, Treated = 2),
    label = "Study arm"
  )
  attr(lab$y, "label") <- "Wellbeing score"
  sav <- file.path(dir, "labelled.sav")
  haven::write_sav(lab, sav)
  sav_out <- app$read_upload(sav, "labelled.sav")
  expect_identical(
    unname(sav_out$labels[c("y", "w")]),
    c("Wellbeing score", "Study arm")
  )
  expect_identical(sav_out$value_labels$w, c(`1` = "Control", `2` = "Treated"))
  expect_true(is.numeric(sav_out$data$w) && is.null(attributes(sav_out$data$w)))
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = sav, name = "labelled.sav"))
    session$setInputs(
      y = "y",
      w = "w",
      treated = "2",
      x = "age",
      ids = "",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 1
    )
    session$setInputs(run = 1)
    expect_equal(
      rv$res$report$settings$w_labels,
      c(`0` = "Control", `1` = "Treated")
    )
    expect_identical(
      rv$res$report$settings$effect_label,
      "Treatment effect on Wellbeing score"
    )
  })
})

test_that("Y is chosen like W: two text values get a choice of the value coded 1", {
  app <- app_env()
  set.seed(15)
  d <- data.frame(
    voted = sample(c("yes", "no"), 300, TRUE),
    arm = rep(c("control", "treated"), 150),
    age = rnorm(300)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = path, name = "votes.csv"))
    expect_match(output$fields$html, "Dependent variable (Y)", fixed = TRUE)
    expect_true("voted" %in% y_pool(NULL))
    session$setInputs(y = "voted", w = "arm")
    expect_true(y_needs_coding())
    expect_match(output$pick_y_one$html, "Value of voted coded 1", fixed = TRUE)
    session$setInputs(
      y_one = "yes",
      treated = "treated",
      x = "age",
      ids = "",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 1
    )
    session$setInputs(run = 1)
    expect_null(rv$error)
    r <- rv$res$report
    expect_identical(unname(r$settings$y_labels), c("no", "yes"))
    expect_identical(
      r$settings$effect_label,
      "Treatment effect on the share with voted = yes"
    )
    expect_match(
      paste(as.character(results_ui(facts()))),
      "the share with <code>voted</code> = yes is on average",
      fixed = TRUE
    )
    expect_match(paste(as.character(results_ui(facts()))), "percentage points")
  })
})

test_that("the GSS example leaves Y, W and X for the researcher to choose", {
  app <- app_env()
  shiny::testServer(app$server, {
    session$setInputs(example = 1)
    html <- output$fields$html
    expect_false(grepl(
      "selected>Rating of Donald Trump|selected>Conservative",
      html
    ))
    expect_false(grepl('<option value="white" selected>', html, fixed = TRUE))
    expect_identical(
      as.character(output$data_note$html),
      '<p class="muted">Data from 3,721 respondents from the 2024 General Social Survey.</p>'
    )
    # The first 10 rows, all columns, before the fields.
    preview <- output$preview$html
    expect_match(preview, "First 10 rows", fixed = TRUE)
    expect_identical(
      lengths(regmatches(preview, gregexpr("<tr>", preview))),
      11L
    )
    for (column in c("id", "rating_trump", "region", "psu_cluster")) {
      expect_match(preview, sprintf(">%s</th>", column), fixed = TRUE)
    }
    # Select all chooses the codebook's characteristics: not the ID, weight,
    # cluster or the other dependent and independent variables.
    session$setInputs(y = "rating_trump", w = "conservative")
    expect_length(all_moderators(), 39L)
    expect_false(any(
      c(
        "id",
        "svy_weight",
        "psu_cluster",
        "rating_harris",
        "economy",
        "college"
      ) %in%
        all_moderators()
    ))
    # With a college degree as W, degree and years of education are left out
    # too.
    session$setInputs(w = "college")
    expect_false(any(c("degree", "educ") %in% all_moderators()))
  })
})

test_that("controls are chosen in their own field and left out of the results", {
  app <- app_env()
  set.seed(23)
  d <- data.frame(
    y = rnorm(300),
    arm = rep(0:1, 150),
    age = rnorm(300),
    conf = rnorm(300)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = path, name = "controls.csv"))
    expect_match(output$fields$html, "Candidate moderators (X)", fixed = TRUE)
    expect_match(output$fields$html, "Controls (X, optional)", fixed = TRUE)
    session$setInputs(
      y = "y",
      w = "arm",
      x = "age",
      controls = "conf",
      ids = "",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 1
    )
    session$setInputs(run = 1)
    expect_null(rv$error)
    r <- rv$res$report
    expect_identical(r$settings$controls, "conf")
    expect_identical(r$moderators$variable, "age")
    html <- paste(as.character(results_ui(facts())))
    expect_match(html, "1 candidate moderator and 1 control")
    expect_match(
      html,
      "The controls (conf) adjust the comparison",
      fixed = TRUE
    )
    expect_false(grepl('value="conf"|moderator-2', html))
  })
})

test_that("the key sentence names W and Y, also when W's values are just Yes and No", {
  app <- app_env()
  set.seed(16)
  d <- data.frame(
    married = sample(c("Yes", "No"), 300, TRUE),
    female = sample(c("Yes", "No"), 300, TRUE),
    age = rnorm(300)
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(d, path, row.names = FALSE)
  shiny::testServer(app$server, {
    session$setInputs(file = list(datapath = path, name = "married.csv"))
    session$setInputs(
      y = "married",
      w = "female",
      y_one = "Yes",
      treated = "Yes",
      x = "age",
      ids = "",
      weights = "",
      clusters = "",
      num_trees = 100,
      seed = 1
    )
    session$setInputs(run = 1)
    expect_null(rv$error)
    sentence <- average_sentence(facts())
    expect_match(
      sentence,
      'average treatment effect<span class="tip" role="tooltip">[^<]*</span></span> of <code>female</code> on the share with <code>married</code> = Yes is'
    )
    expect_match(
      sentence,
      "for <code>female</code> = Yes than for <code>female</code> = No.",
      fixed = TRUE
    )
    expect_match(
      paste(as.character(report_text(facts()))),
      "The treatment effect of female = Yes versus female = No (W)",
      fixed = TRUE
    )
  })
})
