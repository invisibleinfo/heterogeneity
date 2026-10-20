# The heterogeneity app: upload a data file (or try the GSS example), choose
# the dependent variable (Y), the independent variable (W) and the
# characteristics (X), then read the results in four steps; each step shows
# all of its output at once, and its Continue button opens the next step.
# Nothing is saved: Shiny deletes the uploaded file when the session ends.
#
# Launch with heterogeneity::run_app(), or, from the repository root during
# development, devtools::load_all() followed by shiny::runApp("inst/app").

library(shiny)
library(bslib, warn.conflicts = FALSE)
library(heterogeneity)

old_options <- options(shiny.maxRequestSize = 200 * 1024^2)
onStop(function() options(old_options))

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Reading uploaded files ---------------------------------------------------

# Common ways of writing a missing value in exported files.
missing_strings <- c("", "NA", "N/A", "n/a", "#N/A", "NaN", "NULL", "null", ".", "-")

# Returns list(data, labels, value_labels, notes). Handles CSV/TSV/TXT with any
# common delimiter and encoding, Excel, and SPSS/Stata files with labels.
read_upload <- function(path, name) {
  if (!file.exists(path) || file.size(path) == 0) stop("the file is empty.")
  ext <- tolower(tools::file_ext(name))
  out <- switch(ext,
    csv = , tsv = , txt = read_text_table(path),
    xlsx = , xls = read_excel_table(path),
    sav = , dta = read_labelled_table(path, ext),
    stop("please upload a .csv, .tsv, .txt, .xlsx, .xls, .sav or .dta file."))
  tidy_table(out)
}

read_text_table <- function(path) {
  sample <- readBin(path, "raw", min(file.size(path), 1e6))
  sample <- rawToChar(sample[sample != as.raw(0)])
  complete <- sub("[^\n]*$", "", sample, useBytes = TRUE)  # whole lines only, so a cut character cannot mislead
  encoding <- if (validUTF8(if (nzchar(complete)) complete else sample)) "UTF-8-BOM" else "latin1"
  read <- function(sep, header, nrows = -1) utils::read.table(path, sep = sep, header = header, nrows = nrows,
    quote = "\"", comment.char = "", fill = TRUE, check.names = FALSE, strip.white = TRUE, blank.lines.skip = TRUE,
    na.strings = missing_strings, stringsAsFactors = FALSE, fileEncoding = encoding, encoding = "UTF-8")
  # The delimiter that splits the first rows into the most columns.
  seps <- c(",", ";", "\t", "|")
  widths <- vapply(seps, function(s) tryCatch(ncol(read(s, TRUE, 50)), error = function(e) 0L), integer(1))
  if (max(widths) == 0L) stop("the file could not be read as a table.")
  sep <- seps[which.max(widths)]
  d <- read(sep, TRUE)
  notes <- if (encoding == "latin1") "The file was not UTF-8 and was read as Latin-1 text." else character()
  # A first row of numbers is data, not column names.
  if (ncol(d) > 1 && all(!is.na(suppressWarnings(as.numeric(names(d)))))) {
    d <- read(sep, FALSE)
    names(d) <- paste0("column", seq_along(d))
    notes <- c(notes, "No header row was found, so the columns are named column1, column2, and so on.")
  }
  list(data = d, notes = notes)
}

read_excel_table <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("install the readxl package to open Excel files, or upload a CSV.")
  d <- as.data.frame(readxl::read_excel(path, na = missing_strings, .name_repair = "minimal"))
  notes <- character()
  if (ncol(d) > 1 && all(!is.na(suppressWarnings(as.numeric(names(d)))))) {
    d <- as.data.frame(readxl::read_excel(path, na = missing_strings, col_names = FALSE, .name_repair = "minimal"))
    names(d) <- paste0("column", seq_along(d))
    notes <- "No header row was found, so the columns are named column1, column2, and so on."
  }
  list(data = d, notes = notes)
}

# SPSS and Stata files carry variable labels (readable names) and value labels
# (names for codes); both are used for display. Codes stay numbers.
read_labelled_table <- function(path, ext) {
  if (!requireNamespace("haven", quietly = TRUE)) stop("install the haven package to open SPSS or Stata files, or upload a CSV.")
  d <- as.data.frame(if (ext == "sav") haven::read_sav(path) else haven::read_dta(path))
  labels <- vapply(d, function(v) { l <- attr(v, "label", exact = TRUE); if (length(l) && nzchar(l)) as.character(l) else NA_character_ }, "")
  value_labels <- lapply(d, function(v) {
    l <- attr(v, "labels", exact = TRUE)
    l <- l[!is.na(l)]
    if (length(l)) stats::setNames(names(l), as.character(unname(l)))
  })
  value_labels <- Filter(Negate(is.null), value_labels)
  # Plain vectors: labels and formats are kept above, not on the columns.
  d[] <- lapply(d, function(v) {
    if (is.atomic(v) && !is.factor(v) && !inherits(v, c("Date", "POSIXt"))) { v <- unclass(v); attributes(v) <- NULL }
    v
  })
  list(data = d, labels = labels[!is.na(labels)], value_labels = value_labels,
       notes = if (any(!is.na(labels)) || length(value_labels))
         "Variable and value labels from the file are used as readable names; coded values stay numbers." else character())
}

# Usable names, no fully empty rows or columns.
tidy_table <- function(out) {
  d <- out$data
  if (!nrow(d) || !ncol(d)) stop("the file has no data.")
  nm <- trimws(names(d))
  blank <- is.na(nm) | nm == "" | grepl("^\\.\\.\\.[0-9]+$", nm)
  nm[blank] <- paste0("column", which(blank))
  names(d) <- make.unique(nm)
  notes <- out$notes
  if (any(blank)) notes <- c(notes, sprintf("%d unnamed column(s) were named by position.", sum(blank)))
  empty_col <- vapply(d, function(v) all(is.na(v) | (is.character(v) & trimws(v) %in% missing_strings)), logical(1))
  if (any(empty_col)) {
    d <- d[!empty_col]
    notes <- c(notes, sprintf("%d empty column(s) were removed.", sum(empty_col)))
  }
  if (!ncol(d)) stop("the file has no data.")
  empty_row <- rowSums(!is.na(d)) == 0
  if (any(empty_row)) {
    d <- d[!empty_row, , drop = FALSE]
    notes <- c(notes, sprintf("%d empty row(s) were removed.", sum(empty_row)))
  }
  if (!nrow(d)) stop("the file has no data.")
  rownames(d) <- NULL
  list(data = d, labels = out$labels[intersect(names(out$labels), names(d))],
       value_labels = out$value_labels[intersect(names(out$value_labels), names(d))], notes = notes)
}

# Observed values as the package sees them: missing-value text and non-finite
# numbers count as missing.
distinct <- function(v) {
  if (is.logical(v)) v <- as.numeric(v)
  if (is.factor(v)) v <- as.character(v)
  if (is.character(v)) { v <- trimws(v); v[v %in% missing_strings] <- NA_character_ }
  if (is.numeric(v)) v[!is.finite(v)] <- NA_real_
  sort(unique(v[!is.na(v)]))
}

# ---- Formatting -----------------------------------------------------------------

fmt <- function(x, d = 2) ifelse(is.finite(x), sub("^-", "\u2212", formatC(x, digits = d, format = "f", big.mark = ",")), "unavailable")
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
fmt_z <- function(x) sub("^-", "\u2212", sprintf("%+.2f", x))
count_of <- function(k, word) paste(fmt_n(k), if (k == 1) word else paste0(word, "s"))
code_tag <- function(x) as.character(tags$code(x))
more_less <- function(x, more = "larger", less = "smaller") if (x >= 0) more else less
and_list <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
one_line <- function(x) gsub("\\s*\n\\s*", " ", x)

# ---- Plain-language explanations shown on hover -------------------------------

terms <- c(
  dependent = "The variable you want to explain (Y), such as a test score. It must be numeric: continuous, or binary coded 0/1.",
  independent = "The variable whose effect on Y you want to study (W): two groups, such as treated and untreated, or a number, such as hours of use, which gives slopes.",
  characteristics = "Background information about each person (X), such as age or education, that might moderate the effect of W on Y. Text columns become 0/1 indicators, and missing values are kept, not filled in.",
  moderate = "A characteristic moderates an effect when the effect is larger or smaller depending on its value: a programme might, for example, help younger people more than older people.",
  heterogeneity = "Differences in the effect of W on Y from one kind of person to another. The effect is heterogeneous when it is larger for some people than for others.",
  causal_forest = "A machine-learning method that grows many decision trees to find people for whom the effect of W on Y is larger or smaller. It builds on random forests.",
  ate = "The effect of W on Y averaged over everyone: the gap in Y between the two groups of W among people with similar characteristics. It is a causal effect only if W was randomly assigned, or every common cause of W and Y is among the characteristics; otherwise it is an adjusted association.",
  ape = "The effect of a numeric W averaged over everyone: how much Y changes per one-unit increase in W among people with similar characteristics. It is a causal effect only if W was randomly assigned, or every common cause of W and Y is among the characteristics; otherwise it is an adjusted association.",
  sd_units = "The effect divided by how much Y varies (its standard deviation within the two groups of W, before any adjustment), so that effects on outcomes measured in different units can be compared. For a numeric W, it is the change in Y, in standard deviations, for a one-standard-deviation increase in W.",
  predicted = "The forest's estimate of the effect for people with a given combination of characteristics (a conditional average treatment effect). It is an average for similar people, not one person's own effect.",
  honest = "Each tree uses one half of its data to choose its splits and the other half to estimate effects, which guards against mistaking noise for a pattern.",
  doubly_robust = "A way of averaging that combines the forest's predictions with the observed values of Y, so the averages stay accurate even if one of its two underlying models is off.",
  moderator = "A characteristic along which the effect of W may change. Frequent use in the forest's splits makes it a candidate, not a proven moderator.",
  importance = "How often, and how early, the forest's trees split on a characteristic. It shows what the forest relied on, not whether the effect rises or falls.",
  z_score = "A value expressed in standard deviations from the average: 0 is the average, +1 one standard deviation above it, -1 one below.",
  prediction_group = "Up to five groups of about equal size that sort the observations from the smallest predicted effect to the largest.",
  quantile_bins = "Groups of about equal size, formed by cutting a numeric characteristic at its quantiles.",
  noise = "Random error from estimating with a limited sample. It makes predictions spread more widely than the true effects do.",
  association = "Unless W was randomly assigned, a difference between groups may reflect other causes, so it is an association rather than a causal effect.",
  missing = "Missing characteristics are kept: the forest can split on whether a value is missing, so nothing is filled in.",
  treated = "Differences are this group minus the other.",
  ids = "A column that identifies each row, such as a respondent number. It is kept in the downloaded predictions, so you can merge them back into your data.",
  weights = "Survey weights make a sample represent a population: people with larger weights count more in the forest and in every average.",
  clusters = "Groups of related observations, such as students in the same school or people in the same household. The forest then samples whole clusters.",
  trees = "A decision tree asks a series of yes/no questions about the characteristics (such as: is age over 50?) to sort people into small groups. More trees give steadier results but take longer.",
  seed = "A number that fixes the forest's random choices, so the same data and settings always give the same results."
)
# A word or phrase with its explanation on hover (and keyboard focus). Written
# as one string so no stray space appears before following punctuation.
term <- function(key, text) {
  sprintf('<span class="term" tabindex="0">%s<span class="tip" role="tooltip">%s</span></span>',
          htmltools::htmlEscape(text), htmltools::htmlEscape(terms[[key]]))
}
# A field label that explains itself on hover.
label <- function(key, text) HTML(term(key, text))
# A minimal expander for text that is useful but not needed at first sight.
fold <- function(summary, ..., open = FALSE) {
  tags$details(class = "fold", open = if (open) NA, tags$summary(summary), div(class = "fold-body", ...))
}

# The repository, and GitHub's mark for the link to it.
repo <- "https://github.com/invisibleinfo/heterogeneity"
github_mark <- '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"/></svg>'
# A link that works as a Shiny action button, without stray spaces.
action_link <- function(id, text) tags$a(id = id, href = "#", class = "action-button", text)

# ---- Look -------------------------------------------------------------------

fonts <- "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&family=IBM+Plex+Mono&display=swap"
theme <- bs_theme(
  version = 5, bg = "#ffffff", fg = "#18181b", primary = "#18181b", secondary = "#52525b",
  base_font = font_collection("Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  heading_font = font_collection("Source Serif 4", "Charter", "Iowan Old Style", "Georgia", "serif"),
  code_font = font_collection("IBM Plex Mono", "SFMono-Regular", "Menlo", "Consolas", "monospace"),
  "enable-shadows" = FALSE, "border-radius" = "6px", "link-color" = "#18181b",
  "input-border-color" = "#d4d4d8", "input-focus-border-color" = "#18181b", "focus-ring-color" = "rgba(24, 24, 27, .10)",
  "accordion-button-active-bg" = "#ffffff", "accordion-button-active-color" = "#18181b"
)

css <- "
  :root { --bg: #ffffff; --raised: #ffffff; --surface: #f8f8f9; --ink: #18181b; --ink-2: #3f3f46; --ink-3: #71717a;
          --ink-4: #a1a1aa; --rule: #e4e4e7; --rule-2: #d4d4d8; --hover: #000000; --danger: #7f1d1d;
          --shadow: rgba(24, 24, 27, .12); --serif: 'Source Serif 4', Charter, Georgia, serif;
          --sans: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; }
  html[data-bs-theme='dark'] { --bg: #0c0c0e; --raised: #18181b; --surface: #141417; --ink: #f4f4f5; --ink-2: #d4d4d8;
          --ink-3: #a1a1aa; --ink-4: #71717a; --rule: #27272a; --rule-2: #3f3f46; --hover: #ffffff; --danger: #fca5a5;
          --shadow: rgba(0, 0, 0, .5);
          --bs-body-bg: #0c0c0e; --bs-body-color: #f4f4f5; --bs-emphasis-color: #ffffff; --bs-border-color: #3f3f46;
          --bs-secondary-color: #a1a1aa; --bs-tertiary-color: #71717a; --bs-secondary-bg: #18181b; --bs-tertiary-bg: #18181b;
          --bs-link-color: #f4f4f5; --bs-link-color-rgb: 244, 244, 245; --bs-link-hover-color: #ffffff;
          --bs-link-hover-color-rgb: 255, 255, 255; color-scheme: dark; }
  html { -webkit-font-smoothing: antialiased; text-rendering: optimizeLegibility; }
  body { background: var(--bg); color: var(--ink); font-size: 15.5px; line-height: 1.6; }
  .wrap { max-width: 880px; margin: 0 auto; padding: 56px 28px 96px; }
  .eyebrow { display: flex; align-items: center; gap: 10px; margin-bottom: 20px; font: 600 11px/1 var(--sans);
             letter-spacing: .16em; text-transform: uppercase; color: var(--ink-3); }
  .eyebrow .dot { width: 3px; height: 3px; border-radius: 50%; background: var(--ink-4); }
  .eyebrow .gh { margin-left: auto; display: inline-flex; align-items: center; gap: 6px; padding: 6px 11px;
                 border: 1px solid var(--rule-2); border-radius: 999px; color: var(--ink-2); text-decoration: none;
                 letter-spacing: .08em; }
  .eyebrow .gh:hover, .eyebrow .gh:focus { color: var(--ink); border-color: var(--ink-4); background: var(--surface); }
  .eyebrow .gh svg { width: 13px; height: 13px; fill: currentColor; }
  .eyebrow bslib-input-dark-mode { --text-1: var(--ink-2); --text-2: var(--ink-4); }
  h1.title { font: 600 2.55rem/1.12 var(--serif); letter-spacing: -.02em; margin: 0 0 1rem; color: var(--ink); }
  .lead { font-size: 1.07rem; color: var(--ink-2); max-width: 41rem; margin-bottom: 1.4rem; }
  .flow { list-style: none; margin: 0 0 2.6rem; padding: 0; display: flex; flex-wrap: wrap; gap: 6px 22px;
          font: 500 .86rem/1.4 var(--sans); color: var(--ink-2); }
  .flow li { display: flex; align-items: baseline; gap: 7px; }
  .flow .n { font: 400 1rem/1 var(--serif); color: var(--ink-4); }
  .sec { display: grid; grid-template-columns: 56px minmax(0, 1fr); column-gap: 24px; border-top: 1px solid var(--ink);
         padding: 28px 0 36px; }
  .sec-num { font: 400 1.55rem/1.15 var(--serif); color: var(--ink-4); }
  .sec h2 { font: 600 1.5rem/1.25 var(--serif); letter-spacing: -.01em; margin: 0 0 1.2rem; color: var(--ink); }
  .sub { border-top: 1px solid var(--rule); padding-top: 1.5rem; margin-top: 1.9rem; }
  .sub:first-child { border-top: 0; padding-top: 0; margin-top: 0; }
  .sub h3 { font: 600 1.16rem/1.3 var(--serif); margin: 0 0 .75rem; color: var(--ink); }
  @media (max-width: 680px) { .sec { grid-template-columns: 1fr; } .sec-num { font-size: 1rem; margin-bottom: .4rem; }
                              h1.title { font-size: 2rem; } .next { margin-left: 0 !important; } }
  .control-label, .form-label { font: 600 11.5px/1.2 var(--sans); text-transform: uppercase; letter-spacing: .08em;
                                color: var(--ink-3); margin-bottom: .45rem; }
  .shiny-input-container { width: 100% !important; margin-bottom: 1.15rem; }
  .form-control, .selectize-control .selectize-input, .form-select { font-size: .95rem; border-color: var(--rule-2); box-shadow: none; }
  .form-control, .selectize-input, .selectize-input input, .selectize-dropdown, .form-select { background-color: var(--bg); color: var(--ink); }
  .selectize-dropdown { border-color: var(--rule-2); }
  .selectize-dropdown .option.active, .selectize-dropdown .active { background: var(--surface); color: var(--ink); }
  .selectize-control .selectize-input.focus, .form-control:focus, .form-select:focus { border-color: var(--ink); box-shadow: 0 0 0 3px var(--shadow); }
  .selectize-control.multi .selectize-input > div { background: var(--surface); border: 1px solid var(--rule);
                                                   border-radius: 4px; color: var(--ink); padding: 1px 6px; }
  .btn-file { background: var(--bg); color: var(--ink); border-color: var(--rule-2); }
  .form-check-input { background-color: var(--bg); border-color: var(--rule-2); }
  .form-check-input:checked { background-color: var(--ink); border-color: var(--ink); }
  html[data-bs-theme='dark'] .form-check-input:checked { background-color: #71717a; border-color: #71717a; }
  .help { font-size: .84rem; color: var(--ink-3); margin: -.6rem 0 1.1rem; }
  .help a, .help .action-button { color: var(--ink); font-weight: 500; }
  .links { font-size: .82rem; margin: -.7rem 0 1.1rem; color: var(--ink-4); }
  .links a { color: var(--ink-2); }
  .muted { color: var(--ink-2); }
  .prose p { max-width: 44rem; }
  a { color: var(--ink); }
  code { font-size: .85em; color: var(--ink); background: var(--surface); border: 1px solid var(--rule);
         border-radius: 4px; padding: .05em .35em; }
  .shiny-table table { width: 100%; border-collapse: collapse; border-top: 1.5px solid var(--ink);
         border-bottom: 1.5px solid var(--ink); font-variant-numeric: tabular-nums; font-size: .9rem; margin: .5rem 0 1.1rem;
         --bs-table-bg: transparent; --bs-table-color: var(--ink-2); color: var(--ink-2); }
  .shiny-table th { border-bottom: 1px solid var(--ink) !important; font-weight: 600; padding: 8px 14px 8px 0; color: var(--ink); }
  .shiny-table td { border: none !important; padding: 6px 14px 6px 0; vertical-align: top; color: var(--ink-2); }
  .shiny-table { width: 100%; }
  .chart { display: block; width: 100%; margin: .5rem 0 .7rem; }
  .caption { font-size: .92rem; color: var(--ink-2); line-height: 1.6; max-width: 44rem; margin-bottom: .4rem; }
  .caution { font-size: .88rem; color: var(--danger); margin: .2rem 0 .8rem; padding-left: 1.1rem; }
  /* Minimal expanders */
  details.fold { margin: .2rem 0 1rem; }
  details.fold > summary { list-style: none; display: inline-flex; align-items: center; gap: 7px; cursor: pointer;
                           font: 500 .84rem/1.4 var(--sans); color: var(--ink-3); }
  details.fold > summary::-webkit-details-marker { display: none; }
  details.fold > summary::before { content: ''; width: 6px; height: 6px; border-right: 1.5px solid currentColor;
                                   border-bottom: 1.5px solid currentColor; transform: rotate(-45deg); transition: transform .15s ease; }
  details.fold[open] > summary::before { transform: rotate(45deg); }
  details.fold > summary:hover, details.fold > summary:focus { color: var(--ink); outline: none; }
  .fold-body { margin-top: .55rem; font-size: .9rem; color: var(--ink-2); line-height: 1.6; max-width: 44rem; }
  .fold-body p { margin-bottom: .55rem; }
  .fold-body ul { margin: 0; padding-left: 1.05rem; } .fold-body li { margin: .18rem 0; } .fold-body li::marker { color: var(--ink-4); }
  .notes { margin: .2rem 0 0; padding-left: 1.1rem; color: var(--ink-3); font-size: .88rem; }
  .report-box { border: 1px solid var(--rule); background: var(--surface); border-radius: 8px; padding: 1.15rem 1.35rem; }
  .report-box ul { margin: 0; padding-left: 1.1rem; } .report-box li { margin-bottom: .55rem; color: var(--ink-2); }
  .report-box li:last-child { margin-bottom: 0; } .report-box b { color: var(--ink); font-weight: 600; }
  .btn { font: 500 .88rem/1 var(--sans); border-radius: 6px; padding: .66rem 1.15rem; }
  .btn-primary { background: var(--ink); border-color: var(--ink); color: var(--bg); }
  .btn-primary:hover, .btn-primary:focus, .btn-primary:active { background: var(--hover); border-color: var(--hover); color: var(--bg); }
  .btn-outline-dark { border-color: var(--rule-2); color: var(--ink); background: var(--bg); }
  .btn-outline-dark:hover, .btn-outline-dark:focus { background: var(--surface); color: var(--ink); border-color: var(--ink-4); }
  .btn-sm { padding: .5rem .85rem; font-size: .82rem; }
  .accordion { --bs-accordion-bg: var(--bg); --bs-accordion-color: var(--ink); --bs-accordion-border-color: var(--rule);
               --bs-accordion-btn-color: var(--ink); --bs-accordion-active-bg: var(--bg); --bs-accordion-active-color: var(--ink);
               --bs-accordion-btn-focus-box-shadow: none; }
  .accordion-button { font: 500 .92rem/1 var(--sans); color: var(--ink); }
  html[data-bs-theme='dark'] .accordion-button::after { filter: invert(1); }
  .next { margin: .4rem 0 0 80px; }
  .progress { height: 3px; background: var(--rule); } .progress-bar { background: var(--ink); color: transparent; }
  .shiny-notification { background: var(--raised); color: var(--ink); border-radius: 8px; border: 1px solid var(--rule-2);
                        box-shadow: 0 10px 30px var(--shadow); }
  .shiny-output-error-validation { color: var(--danger); }
  /* Explanations on hover and keyboard focus. */
  .term { position: relative; border-bottom: 1px dotted var(--ink-4); cursor: help; }
  .term:focus { outline: none; border-bottom-color: var(--ink); }
  .term .tip { position: absolute; left: 50%; bottom: calc(100% + 9px); transform: translateX(-50%); width: 18rem;
               max-width: 80vw; padding: .75rem .9rem; background: var(--raised); border: 1px solid var(--rule); border-radius: 8px;
               box-shadow: 0 12px 32px var(--shadow); font: 400 .82rem/1.5 var(--sans); color: var(--ink-2);
               text-transform: none; letter-spacing: normal; text-align: left; white-space: normal;
               visibility: hidden; opacity: 0; transition: opacity .12s ease; z-index: 30; pointer-events: none; }
  .term:hover .tip, .term:focus .tip { visibility: visible; opacity: 1; }
  .control-label .term .tip { left: 0; transform: none; }
"

js <- "
Shiny.addCustomMessageHandler('scroll_to', function(id) {
  setTimeout(function() { var el = document.getElementById(id); if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'}); }, 200);
});
function copyReport(btn) {
  var items = document.querySelectorAll('#report-text li');
  navigator.clipboard.writeText(Array.from(items).map(function(li) { return '- ' + li.innerText; }).join('\\n'));
  btn.innerText = 'Copied'; setTimeout(function() { btn.innerText = 'Copy text'; }, 1500);
}"

section <- function(n, title, ...) {
  div(class = "sec", id = paste0("step", n), div(class = "sec-num", n), div(tags$h2(title), ...))
}
sub_section <- function(id, title, ...) div(class = "sub", id = paste0("unit-", id), tags$h3(title), ...)
help <- function(...) div(class = "help", ...)

# The results come in three steps, each shown whole: step 2 (the average effect
# and the three plots that printing a report draws), step 3 (groups) and step 4.
units <- c("run", "groups", "export")
steps <- c("Choose variables", "Run the forest", "Compare groups", "Export results")

# ---- UI ---------------------------------------------------------------------

ui <- page(
  theme = theme, title = "Exploring heterogeneity with causal forests",
  tags$head(tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
            tags$link(rel = "stylesheet", href = fonts),
            tags$style(HTML(css)), tags$script(HTML(js))),
  div(class = "wrap",
    div(class = "eyebrow", span("heterogeneity"), span(class = "dot"), span("R package and app"),
        tags$a(class = "gh", href = repo, target = "_blank", rel = "noopener", HTML(github_mark), "GitHub"),
        input_dark_mode(id = "mode", mode = "light")),
    h1(class = "title", "Exploring heterogeneity with causal forests"),
    p(class = "lead", HTML(paste0(
      "Upload a dataset, choose a ", term("dependent", "dependent variable"), " (Y), an ",
      term("independent", "independent variable"), " (W), and the characteristics (X) that might ",
      term("moderate", "moderate"), " the effect of W on Y. A ", term("causal_forest", "causal forest"),
      " explores these patterns of ", term("heterogeneity", "heterogeneity"),
      "; the results are explained one step at a time below."))),
    tags$ol(class = "flow", lapply(seq_along(steps), function(k) tags$li(span(class = "n", k), steps[k]))),
    section(1, "Choose variables",
      fileInput("file", "Data file", accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".sav", ".dta"),
                placeholder = "No file selected"),
      help(HTML(paste0("No data at hand? ", action_link("example", "Try the GSS example"),
                       ". Nothing is saved: refreshing the page clears everything."))),
      uiOutput("example_error"),
      fold("What your file needs",
        tags$ul(
          tags$li("One row per person (or unit), with column names in the first row: CSV, TSV, Excel, SPSS (.sav) or Stata (.dta)."),
          tags$li("A dependent variable (Y): numeric, either continuous (such as a test score) or binary coded 0/1 (such as voted or not)."),
          tags$li("An independent variable (W): either binary, with any two values (such as treated and untreated), or numeric with more values (such as hours of use, which gives slopes)."),
          tags$li("Characteristics (X): numeric columns, or text columns with up to 20 categories."),
          tags$li("Rows missing Y or W are left out; missing characteristics are kept and handled by the forest."),
          tags$li("Small samples work, but their estimates are very noisy.")
        )),
      uiOutput("data_note"),
      uiOutput("fields")
    ),
    div(id = "results"),
    uiOutput("next_ui")
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(source = "none", res = NULL, unit = 0, error = NULL, run_note = NULL, gss = NULL, gss_error = NULL)
  # The example is downloaded with gssr the first time the link is used.
  observeEvent(input$example, {
    rv$gss_error <- NULL
    if (is.null(rv$gss)) {
      withProgress(message = "Downloading the GSS 2024 file", value = 0.3, {
        rv$gss <- tryCatch(load_gss2024(), error = function(e) { rv$gss_error <- conditionMessage(e); NULL })
      })
    }
    if (!is.null(rv$gss)) rv$source <- "example"
  })
  observeEvent(input$file, rv$source <- "upload")
  example <- reactive(identical(rv$source, "example"))
  output$example_error <- renderUI(if (!is.null(rv$gss_error)) p(class = "shiny-output-error-validation", rv$gss_error))

  # The data and any labels: from the example, or from the uploaded file.
  upload <- reactive({
    req(input$file)
    tryCatch(read_upload(input$file$datapath, input$file$name),
             error = function(e) validate(need(FALSE, paste("The file could not be read:", conditionMessage(e)))))
  })
  source_data <- reactive({
    if (example()) return(list(data = rv$gss$data, labels = rv$gss$labels, value_labels = rv$gss$value_labels, notes = character()))
    upload()
  })
  data <- reactive(source_data()$data)
  labels <- reactive(source_data()$labels)
  num_cols  <- reactive(names(Filter(function(x) (is.numeric(x) || is.logical(x)) && any(!is.na(x)), data())))
  text_cols <- reactive(names(Filter(function(x) (is.character(x) || is.factor(x)) && length(distinct(x)) %in% 2:20, data())))
  bin_cols  <- reactive(names(Filter(function(x) length(distinct(x)) == 2, data())))
  opt <- function(id) if (is.null(input[[id]]) || identical(input[[id]], "")) NULL else input[[id]]
  # Choices show a readable label next to the column name when one is known.
  choices_for <- function(vars) {
    lab <- labels()
    if (!length(vars) || !length(lab)) return(vars)
    shown <- unname(lab[vars])
    shown[is.na(shown)] <- vars[is.na(shown)]
    stats::setNames(vars, ifelse(shown == vars, vars, paste0(shown, " (", vars, ")")))
  }
  w_values <- reactive({ req(input$w, input$w %in% names(data())); distinct(data()[[input$w]]) })
  w_needs_coding <- reactive(length(w_values()) == 2L && !identical(as.character(w_values()), c("0", "1")))

  # ---- Step 1: once data are loaded, every field is shown together ---------

  output$data_note <- renderUI({
    src <- source_data(); d <- src$data
    if (example()) return(p(class = "muted", sprintf(
      "The GSS example: %s respondents to the 2024 General Social Survey, with the tutorial's choices filled in.",
      fmt_n(nrow(d)))))
    tagList(
      p(class = "muted", sprintf("Read %s rows and %d columns (%d numeric, %d text with 2 to 20 categories).",
                                 fmt_n(nrow(d)), ncol(d), length(num_cols()), length(text_cols()))),
      if (length(src$notes)) tags$ul(class = "notes", lapply(src$notes, tags$li)))
  })

  # The columns each field offers. Y, W and X exclude one another, so each
  # list is updated when Y or W changes.
  y_pool <- function(w) setdiff(num_cols(), w)
  w_pool <- function(y) setdiff(union(bin_cols(), num_cols()), y)
  x_pool <- function(y, w) setdiff(c(num_cols(), text_cols()), c(y, w))

  # Drawn once per data set (it does not depend on the choices themselves).
  output$fields <- renderUI({
    req(tryCatch(data(), error = function(e) NULL))  # a file that cannot be read is reported once, above
    ex <- example()
    y0 <- if (ex) "rating_trump"; w0 <- if (ex) "conservative"
    none <- c("None" = "")
    tagList(
      if (length(num_cols()))
        selectizeInput("y", label("dependent", "Dependent variable (Y)"), choices = c("", choices_for(y_pool(w0))),
                       selected = y0 %||% "", options = list(placeholder = "Choose a column"))
      else p(class = "muted", "No numeric columns were found, so none can be the dependent variable (Y)."),
      selectizeInput("w", label("independent", "Independent variable (W)"), choices = c("", choices_for(w_pool(y0))),
                     selected = w0 %||% "", options = list(placeholder = "Choose a column")),
      uiOutput("pick_treated"),
      selectizeInput("x", label("characteristics", "Characteristics (X)"), choices = choices_for(x_pool(y0, w0)), multiple = TRUE,
                     selected = if (ex) intersect(rv$gss$covariates, x_pool(y0, w0)),
                     options = list(placeholder = "Choose one or more columns", plugins = list("remove_button"))),
      div(class = "links", action_link("x_all", "Select all"), " / ", action_link("x_none", "Clear")),
      accordion(
        open = FALSE, class = "mt-1 mb-3",
        accordion_panel(
          "Optional settings",
          selectInput("ids", label("ids", "ID column"), c(none, choices_for(names(data()))), selected = if (ex) "id" else ""),
          selectInput("weights", label("weights", "Weight column"), c(none, choices_for(num_cols()))),
          selectInput("clusters", label("clusters", "Cluster column"), c(none, choices_for(names(data())))),
          fluidRow(column(6, numericInput("num_trees", label("trees", "Number of trees"), 2000, min = 100, step = 500)),
                   column(6, numericInput("seed", label("seed", "Random seed"), 21042020, min = 0, step = 1)))
        )
      ),
      actionButton("run", "Continue to step 2: Run the forest", class = "btn-primary mt-2"),
      uiOutput("run_note")
    )
  })
  update_x <- function(y, w) updateSelectizeInput(session, "x", choices = choices_for(x_pool(y, w)),
                                                  selected = setdiff(input$x, c(y, w)))
  observeEvent(input$y, {
    updateSelectizeInput(session, "w", choices = c("", choices_for(w_pool(opt("y")))), selected = opt("w") %||% "")
    update_x(opt("y"), opt("w"))
  }, ignoreInit = TRUE)
  observeEvent(input$w, {
    updateSelectizeInput(session, "y", choices = c("", choices_for(y_pool(opt("w")))), selected = opt("y") %||% "")
    update_x(opt("y"), opt("w"))
  }, ignoreInit = TRUE)
  observeEvent(input$x_all, updateSelectizeInput(session, "x", selected = x_pool(opt("y"), opt("w"))))
  observeEvent(input$x_none, updateSelectizeInput(session, "x", selected = character(0)))

  output$pick_treated <- renderUI({
    req(w_needs_coding())
    vals <- as.character(w_values())
    shown <- vals
    codes <- source_data()$value_labels[[input$w]]
    if (length(codes)) shown[vals %in% names(codes)] <- unname(codes[vals[vals %in% names(codes)]])
    radioButtons("treated", label("treated", paste("Value of", input$w, "coded 1")), choices = stats::setNames(vals, shown),
                 selected = vals[2], inline = TRUE)
  })
  output$run_note <- renderUI(if (!is.null(rv$run_note)) p(class = "shiny-output-error-validation mt-2", rv$run_note))

  # ---- Run, then show the results one whole step at a time -----------------

  clear_results <- function() {
    removeUI("#results > *", multiple = TRUE, immediate = TRUE)
    rv$res <- NULL
    rv$unit <- 0
    rv$error <- NULL
    rv$run_note <- NULL
  }
  # Any change to the data or the analysis settings makes shown results stale.
  observeEvent(list(rv$source, input$file, input$y, input$w, input$treated, input$x, input$ids,
                    input$weights, input$clusters, input$num_trees, input$seed),
               clear_results(), ignoreInit = TRUE)

  # Step k + 1 of the four (units[k]) is added below the earlier ones.
  show_unit <- function(k) {
    insertUI("#results", "beforeEnd", unit_ui(units[k]), immediate = TRUE)
    rv$unit <- k
    session$sendCustomMessage("scroll_to", paste0("step", k + 1))
  }

  observeEvent(input$run, {
    clear_results()
    missing <- c(if (is.null(opt("y"))) "a dependent variable (Y)", if (is.null(opt("w"))) "an independent variable (W)",
                 if (!length(input$x)) "at least one characteristic (X)")
    if (length(missing)) {
      rv$run_note <- paste0("Choose ", and_list(missing), " first.")
      return()
    }
    res <- tryCatch({
      is_example <- example()
      src <- source_data()
      named <- function(v) { l <- unname(src$labels[v]); if (length(l) && !is.na(l)) l else v }
      continuous <- length(w_values()) > 2L
      warnings <- character()
      report <- withProgress(message = "Growing the forest", detail = "This can take a minute.", value = 0.3, {
        withCallingHandlers(
          explore_heterogeneity(
            data = data(), y = input$y, w = input$w, x = input$x,
            treated = if (isTRUE(w_needs_coding())) opt("treated") else NULL,
            ids = opt("ids"), weights = opt("weights"), clusters = opt("clusters"),
            num_trees = input$num_trees %||% 2000, seed = input$seed %||% 21042020,
            labels = if (length(src$labels)) src$labels, value_labels = if (length(src$value_labels)) src$value_labels,
            effect_label = if (is_example && input$y == "rating_trump" && !continuous) rv$gss$effect_label else
              if (continuous) paste("Change in", named(input$y), "per unit of", named(input$w)) else paste("Difference in", named(input$y))
          ),
          warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
      })
      list(report = report, warnings = unique(warnings), y = input$y, w = input$w)
    }, error = function(e) list(error = conditionMessage(e)))
    if (!is.null(res$error)) {
      rv$error <- res$error
      insertUI("#results", "beforeEnd", immediate = TRUE,
               div(class = "sec", id = "step-error", div(class = "sec-num", "!"),
                   div(tags$h2("The analysis could not run"), p(res$error))))
      session$sendCustomMessage("scroll_to", "step-error")
      return()
    }
    rv$res <- res
    show_unit(1)
  })

  observeEvent(input$next_step, { req(rv$res, rv$unit >= 1, rv$unit < length(units)); show_unit(rv$unit + 1) })
  output$next_ui <- renderUI({
    req(rv$res, rv$unit >= 1, rv$unit < length(units))
    label <- switch(units[rv$unit + 1], groups = "Continue to step 3: Compare groups", export = "Continue to step 4: Export results")
    div(class = "next", actionButton("next_step", label, class = "btn-outline-dark"))
  })

  # ---- Numbers and words used in the text -------------------------------------

  report <- reactive({ req(rv$res); rv$res$report })
  # Readable labels are shown as text; raw column names and values as code.
  name_tag <- function(r, v) if (is.null(r$settings$labels)) vapply(v, code_tag, "") else unname(r$labels[v])
  facts <- reactive({
    r <- report()
    continuous <- identical(r$settings$w_type, "continuous")
    lab <- r$settings$w_labels
    groups <- if (continuous) c("", "") else if (identical(unname(lab), c("0", "1"))) paste0(rv$res$w, " = ", lab) else unname(lab)
    group_tags <- if (continuous || !is.null(r$settings$value_labels[[rv$res$w]])) groups else vapply(groups, code_tag, "")
    list(report = r, y = rv$res$y, w = rv$res$w, continuous = continuous,
         noun = if (continuous) "slope" else "difference", plural = if (continuous) "slopes" else "differences",
         average_key = if (continuous) "ape" else "ate",
         average_name = if (continuous) "average partial effect" else "average treatment effect",
         g0 = groups[1], g1 = groups[2], g0_tag = group_tags[[1]], g1_tag = group_tags[[2]],
         ate = r$ate$estimate, std = r$ate$standardized %||% NA_real_, top = utils::head(r$moderators, 3),
         n_missing = sum(r$missingness$n_missing), notes = r$notes[!caution(r$notes)],
         cautions = c(r$notes[caution(r$notes)], rv$res$warnings))
  })
  # Notes that call for caution are shown; the rest sit in an expander.
  caution <- function(notes) grepl("^Only |^Some profiles have few|^Adjusted average unavailable", notes)
  sub_title <- function(u) {
    plural <- if (!is.null(rv$res) && identical(rv$res$report$settings$w_type, "continuous")) "slopes" else "differences"
    switch(u, spread = paste("How much do predicted", plural, "vary?"),
           characteristics = "Which candidate moderators did the forest use most?",
           profiles = "Who has smaller and larger predictions?")
  }
  average_sentence <- function(f) {
    if (!is.finite(f$ate)) return(sprintf("The average adjusted %s (the %s) is unavailable; see the notes below.", f$noun, f$average_name))
    effect <- if (f$continuous) sprintf("per one-unit increase in %s", code_tag(f$w)) else sprintf("for %s than for %s", f$g1_tag, f$g0_tag)
    in_sd <- if (is.finite(f$std)) sprintf(", or <b>%s</b> %s%s", fmt(abs(f$std)), term("sd_units", "standard deviations"),
                                           if (f$continuous) paste(" per standard deviation of", code_tag(f$w)) else "") else ""
    sprintf("Comparing people with similar characteristics, %s is on average <b>%s</b> %s %s%s. This average adjusted %s is the %s.",
            code_tag(f$y), fmt(abs(f$ate)), more_less(f$ate, "higher", "lower"), effect, in_sd, f$noun,
            term(f$average_key, f$average_name))
  }
  importance_plot <- reactive(plot(report(), "importance"))
  profile_plot <- reactive(plot(report(), "profiles"))
  # Every characteristic gets a row, so these plots grow with their number.
  importance_height <- function() max(260, 90 + 24 * nrow(report()$moderators))
  profile_height <- function() max(280, 110 + 28 * length(unique(profile_plot()$data$feature)))
  # Plots as shown on the page. Headings and the text below each plot replace
  # its title and caption, and the background is transparent. In dark mode,
  # text and grid lines turn light, white outlines take the page colour, and
  # dark teal turns lighter; text on heatmap tiles stays dark.
  dark <- reactive(identical(input$mode, "dark"))
  for_page <- function(p) {
    p <- p + ggplot2::labs(title = NULL, caption = NULL) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent", colour = NA))
    if (!dark()) return(p)
    on_tiles <- any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomTile"), logical(1)))
    p$layers <- lapply(p$layers, function(layer) {
      copy <- ggplot2::ggproto(NULL, layer)  # a copy, so the cached plot keeps its colours
      a <- copy$aes_params
      for (k in intersect(c("fill", "colour"), names(a))) {
        if (identical(a[[k]], "#286b70")) a[[k]] <- "#72a9ac"
        if (identical(a[[k]], "white")) a[[k]] <- "#0c0c0e"
      }
      if (inherits(copy$geom, "GeomText") && !on_tiles) a$colour <- "#e4e4e7"
      copy$aes_params <- a
      copy
    })
    p + ggplot2::theme(text = ggplot2::element_text(colour = "#e4e4e7"), axis.text = ggplot2::element_text(colour = "#a1a1aa"),
                       panel.grid.major = ggplot2::element_line(colour = "#27272a"),
                       panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                       legend.background = ggplot2::element_rect(fill = "transparent", colour = NA))
  }

  # ---- The content of each part ---------------------------------------------

  unit_ui <- function(u) {
    f <- facts(); r <- f$report
    switch(u,
      "run" = section(2, "Run the forest", div(class = "prose", div(class = "sub", id = "unit-summary",
        p(HTML(average_sentence(f))),
        if (length(f$cautions)) tags$ul(class = "caution", lapply(f$cautions, tags$li)),
        if (length(f$notes)) fold(sprintf("Notes on the data (%d)", length(f$notes)), tags$ul(lapply(f$notes, tags$li))),
        fold("How these results were computed", p(HTML(computed_text(f))))
      ),
      sub_section("spread", sub_title("spread"),
        div(class = "chart", plotOutput("dist", height = "400px")),
        explained(spread_text(f))
      ),
      sub_section("characteristics", sub_title("characteristics"),
        div(class = "chart", plotOutput("importance", height = "auto")),
        explained(list(
          short = paste0("The forest split most often, and earliest, on ",
                         and_list(sprintf("%s (%s)", name_tag(r, f$top$variable), fmt(f$top$importance))), "."),
          long = paste0("These are the leading ", term("moderator", "candidate moderators"), ". ", term("importance", "Importance"),
            " shows which characteristics the predictions depend on, but not whether the ", f$noun,
            " rises or falls with them; step 3 shows that. Related characteristics share importance. The chart shows all ",
            count_of(nrow(r$moderators), "usable characteristic"), " you chose.",
            if (any(grepl("^Highly correlated", r$notes))) " Highly correlated pairs are listed in the notes on the data." else "")))
      ),
      sub_section("profiles", sub_title("profiles"),
        div(class = "chart", plotOutput("profiles", height = "auto")),
        explained(profile_text())
      ))),
      "groups" = section(3, "Compare groups", div(class = "prose",
        p(class = "muted", paste0("The adjusted ", f$noun, " within the groups of a characteristic, starting with the top ",
                                  "candidate moderator. Add a second characteristic for a heatmap.")),
        fluidRow(
          column(6, selectizeInput("x1", "Characteristic", choices = covariate_choices(r), selected = r$moderators$variable[1],
                                   options = list(placeholder = "Choose a characteristic"))),
          column(6, selectizeInput("x2", "Second characteristic (optional)", choices = c("", covariate_choices(r)),
                                   options = list(placeholder = "None")))),
        div(class = "chart", plotOutput("effect_plot", height = "440px")),
        uiOutput("effect_text"),
        fold("Table of groups", tableOutput("effect_table"))
      )),
      "export" = section(4, "Export results", div(class = "prose",
        div(class = "report-box", id = "report-text", report_text(f)),
        div(class = "mt-3",
            tags$button(class = "btn btn-outline-dark btn-sm me-2", onclick = "copyReport(this)", "Copy text"),
            downloadButton("dl_cates", "Predictions for each row (CSV)", class = "btn-outline-dark btn-sm"))
      ))
    )
  }

  covariate_choices <- function(r) stats::setNames(r$settings$covariates, unname(r$labels[r$settings$covariates]))
  computed_text <- function(f) {
    r <- f$report
    sd_used <- if (!is.finite(f$std)) "" else if (f$continuous)
      sprintf(" In standard deviations, the average uses %s (SD %s) and %s (SD %s), measured before adjustment.",
              code_tag(f$y), fmt(r$ate$sd_y), code_tag(f$w), fmt(r$ate$sd_w))
      else sprintf(" In standard deviations, the average is divided by the spread of %s within the two groups, before adjustment (SD %s).",
                   code_tag(f$y), fmt(r$ate$sd_y))
    paste0(sprintf("The %s grew %s %s %s from %s observations and %s. It predicts the effect of W for every observation, and its averages, overall and within groups, are %s.",
                   term("causal_forest", "causal forest"), fmt_n(r$settings$num_trees), term("honest", "honest"),
                   term("trees", "trees"), fmt_n(r$settings$n), count_of(length(r$settings$covariates), "characteristic"),
                   term("doubly_robust", "doubly robust")),
           sd_used,
           if (r$settings$weighted) " Survey weights were used for the forest and all averages." else "",
           if (r$settings$clustered) " Clusters were used for sampling." else "",
           " The three plots below describe the predictions; step 3 compares groups of your choice.")
  }
  # A plot's key finding, with the rest of its explanation in an expander.
  explained <- function(text) tagList(p(class = "caption", HTML(text$short)),
                                      if (length(text$long)) fold("How to read this plot", p(HTML(text$long))))

  # The histogram: the range of predictions and, in its expander, what the
  # colours mean here.
  spread_text <- function(f) {
    r <- f$report
    below <- sum(r$weights[r$cates$cate_estimate < 0]) / sum(r$weights)
    share <- if (below == 0) "All are above 0." else if (below == 1) "All are below 0."
      else sprintf("%s%% of %s %s a predicted %s below 0.", formatC(100 * below, digits = 0, format = "f"),
                   if (r$settings$weighted) "the weighted sample" else "observations", if (r$settings$weighted) "has" else "have", f$noun)
    meaning <- if (f$continuous) sprintf("slopes where %s rises with %s, and red bars, below 0, slopes where it falls", code_tag(f$y), code_tag(f$w))
      else sprintf("predictions where %s is higher for %s than for %s, and red bars, below 0, predictions where it is lower",
                   code_tag(f$y), f$g1_tag, f$g0_tag)
    list(short = sprintf("%s run from %s to %s, around the average of %s. %s", term("predicted", paste("Predicted", f$plural)),
                         fmt(r$prediction_range[1]), fmt(r$prediction_range[2]), fmt(f$ate), share),
         long = sprintf(paste("Each bar shows how many observations have a predicted %s in its range. As in the heatmaps, colours deepen",
                              "with distance from 0: teal bars, above 0, mark %s.",
                              "Part of this spread is %s, especially at the extremes, which rest on few observations. A prediction is the",
                              "expected %s for people with similar characteristics, not one person's own %s."),
                        f$noun, meaning, term("noise", "estimation noise"), f$noun, f$noun))
  }

  # The profiles heatmap: where the end groups differ most and, in its
  # expander, how to read the cells.
  profile_text <- function() {
    d <- profile_plot()$data
    groups <- sort(unique(d$quintile))
    if (length(groups) < 2) return(list(short = "All observations have the same prediction, so there are no profiles to compare."))
    first <- d[d$quintile == groups[1], ]; last <- d[d$quintile == groups[length(groups)], ]
    last <- last[match(first$feature, last$feature), ]
    gap <- last$z - first$z
    top <- utils::head(order(-abs(gap)), 2)
    top <- top[is.finite(gap[top])]
    labels <- one_line(as.character(first$label[top]))
    if (is.null(report()$settings$labels)) labels <- vapply(labels, code_tag, "")
    list(
      short = if (length(top)) paste0("The groups with the smallest and largest predictions differ most on ",
        paste(sprintf("%s (%s vs %s)", labels, fmt_z(first$z[top]), fmt_z(last$z[top])), collapse = " and "), ".")
        else "The groups with the smallest and largest predictions do not differ on these characteristics.",
      long = paste0(
        "Each row is a characteristic, in order of importance",
        if (any(!is.na(report()$feature_map$level))) " (a text characteristic has one row per category)", ". Each column is a ",
        term("prediction_group", "prediction group"), ": the observations are sorted by their predictions and split into up to five ",
        "groups of about equal size, from the smallest predictions on the left to the largest on the right. A cell shows the group's average ",
        term("z_score", "z-score"), ": 0 is the sample average, and +1 or \u22121 is one standard deviation above or below it. ",
        if (length(top)) paste0("In the original units, the end groups' means are ",
          paste(sprintf("%s vs %s for %s", fmt(first$mean[top]), fmt(last$mean[top]), labels), collapse = " and "), ". ") else "",
        "Missing values are ignored. Because the groups are formed by sorting the predictions, some differences come from the ",
        "sorting itself: read this plot as a description of the forest's pattern, and check meaningful groups in step 3."))
  }

  report_text <- function(f) {
    r <- f$report
    item <- function(label, ...) tags$li(tags$b(paste0(label, if (grepl("[?]$", label)) " " else ": ")), ...)
    data_parts <- c(
      if (r$n_dropped > 0) sprintf("%s rows with missing information were left out", fmt_n(r$n_dropped)),
      if (f$n_missing > 0) sprintf("%s missing characteristic values were kept for the forest rather than filled in", fmt_n(f$n_missing)),
      if (any(grepl("^Text characteristics became 0/1 indicators", r$notes))) "text characteristics were turned into yes/no (0/1) indicators",
      if (any(grepl("left out", r$notes))) "characteristics that could not be used were set aside (see step 2)"
    )
    sample <- sprintf("using %s and %s (X).", count_of(r$settings$n, "observation"), count_of(length(r$settings$covariates), "characteristic"))
    analysed <- if (f$continuous) sprintf("How %s (Y) changes with %s (W), %s", f$y, f$w, sample)
                else sprintf("How %s (Y) differs between %s and %s (W), %s", f$y, f$g1, f$g0, sample)
    effect <- if (f$continuous) sprintf("per one-unit increase in %s", f$w) else sprintf("for %s than for %s", f$g1, f$g0)
    in_sd <- if (is.finite(f$std)) sprintf(", or %s standard deviations%s", fmt(abs(f$std)),
                                           if (f$continuous) paste(" per standard deviation of", f$w) else "") else ""
    average <- if (!is.finite(f$ate)) sprintf("The average adjusted %s (the %s) could not be estimated.", f$noun, f$average_name) else
      sprintf("Comparing people with similar characteristics, %s was on average %s %s %s%s; this is the %s.",
              f$y, fmt(abs(f$ate)), more_less(f$ate, "higher", "lower"), effect, in_sd, f$average_name)
    tags$ul(
      item("What was analysed", analysed),
      if (length(data_parts)) item("Data", paste0(toupper(substring(data_parts[1], 1, 1)), substring(and_list(data_parts), 2), ".")),
      item("Average effect", average),
      item("How much does it vary?", sprintf("Predicted %s ranged from %s to %s; part of this spread is estimation noise.",
                                             f$plural, fmt(r$prediction_range[1]), fmt(r$prediction_range[2]))),
      item("Candidate moderators", sprintf("The forest split most often on %s.", and_list(f$top$label))),
      item("Keep in mind", paste0("These patterns are exploratory: use them to decide what to study next. Each prediction is an average for people with similar characteristics, not one person's own ",
           f$noun, ". ", sprintf("Unless %s was randomly assigned, the effects are adjusted associations: they may reflect other causes. ", f$w),
           if (r$settings$weighted) "Averages use the supplied weights." else "Without weights, the results describe this sample."))
    )
  }

  # ---- Outputs ----------------------------------------------------------------

  output$dist <- renderPlot(for_page(plot(report())), res = 110, bg = "transparent")
  output$importance <- renderPlot(for_page(importance_plot()), res = 110, bg = "transparent", height = function() importance_height())
  output$profiles <- renderPlot(for_page(profile_plot()), res = 110, bg = "transparent", height = function() profile_height())

  x2_or_null <- reactive(if (is.null(input$x2) || !nzchar(input$x2) || identical(input$x2, input$x1)) NULL else input$x2)
  effect_tab <- reactive({
    req(input$x1, input$x1 %in% report()$settings$covariates)
    effect_by(report(), input$x1, x2 = x2_or_null())
  })
  output$effect_plot <- renderPlot(for_page(plot(effect_tab())), res = 110, bg = "transparent")
  output$effect_text <- renderUI({
    t <- effect_tab(); r <- report(); f <- facts(); info <- attr(t, "plot")
    t$label <- do.call(paste, c(lapply(seq_along(info$titles), function(k) paste0(info$titles[k], " = ", info$levels[[k]])), sep = ", "))
    # Describe the groups the plot shows: estimated, without missing values.
    est <- t[is.finite(t$estimate) & !info$missing, ]
    need <- if (f$continuous) "in which W varies" else "including both groups of W"
    rule <- if (f$continuous) "or in which W does not vary" else "or with only one group of W"
    long <- paste0(
      "These groups also differ in other ways, so the plot describes ", term("association", "differences between groups of people"),
      ", not what would happen if a characteristic changed. Numeric characteristics are split into ",
      term("quantile_bins", "groups of about equal size"), ". Groups with fewer than 30 observations, ", rule, ", are not estimated.",
      if (any(info$missing)) " The plot leaves out missing values; the table of groups includes them." else "")
    short <- if (nrow(est) < 2) paste0("Too few groups could be estimated to compare (each needs at least 30 observations, ", need, ").") else {
      lo <- est[which.min(est$estimate), ]; hi <- est[which.max(est$estimate), ]
      desc <- function(row) sprintf("%s (%s; n = %s)", if (is.null(r$settings$labels)) code_tag(row$label) else row$label,
                                    fmt(row$estimate), fmt_n(row$n))
      paste0("The adjusted ", f$noun, " is smallest for ", desc(lo), " and largest for ", desc(hi),
             ", against an overall average of ", fmt(r$ate$estimate), ".")
    }
    explained(list(short = short, long = long))
  })
  output$effect_table <- renderTable({
    t <- effect_tab(); f <- facts(); info <- attr(t, "plot")
    out <- stats::setNames(lapply(info$levels, as.character), info$titles)
    out[["n"]] <- fmt_n(t$n)
    if (!f$continuous) {
      out[[paste("n", f$g0)]] <- fmt_n(t$n_control)
      out[[paste("n", f$g1)]] <- fmt_n(t$n_treated)
    }
    out[[paste("Adjusted", f$noun)]] <- ifelse(is.finite(t$estimate), fmt(t$estimate), "")
    out[["Note"]] <- ifelse(t$status == "ok", "", t$status)
    as.data.frame(out, check.names = FALSE)
  }, align = "l")

  # The table sits in a closed expander, so it must render while hidden.
  outputOptions(output, "effect_table", suspendWhenHidden = FALSE)

  output$dl_cates <- downloadHandler(function() "predictions.csv",
                                     function(file) utils::write.csv(report()$cates, file, row.names = FALSE))
}

shinyApp(ui, server)
