# The heterogeneity app: upload a data file (or try the GSS example), choose the
# dependent variable (Y), the independent variable (W) and the characteristics
# (X), and run the causal forest. Every result then appears at once: the average
# treatment effect, the distribution of estimated treatment effects, the
# importance ranking, a plot for each candidate moderator and the export.
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

# ---- Reading uploaded files --------------------------------------------------

# Common ways of writing a missing value in exported files.
missing_strings <- c(
  "",
  "NA",
  "N/A",
  "n/a",
  "#N/A",
  "NaN",
  "NULL",
  "null",
  ".",
  "-"
)

# Returns list(data, labels, value_labels, notes). Handles CSV/TSV/TXT with any
# common delimiter and encoding, Excel, and SPSS/Stata files with labels.
read_upload <- function(path, name) {
  if (!file.exists(path) || file.size(path) == 0) {
    stop("the file is empty.")
  }
  ext <- tolower(tools::file_ext(name))
  out <- switch(
    ext,
    csv = ,
    tsv = ,
    txt = read_text_table(path),
    xlsx = ,
    xls = read_excel_table(path),
    sav = ,
    dta = read_labelled_table(path, ext),
    stop("please upload a .csv, .tsv, .txt, .xlsx, .xls, .sav or .dta file.")
  )
  tidy_table(out)
}

read_text_table <- function(path) {
  sample <- readBin(path, "raw", min(file.size(path), 1e6))
  sample <- rawToChar(sample[sample != as.raw(0)])
  # Whole lines only, so a cut character cannot mislead.
  complete <- sub("[^\n]*$", "", sample, useBytes = TRUE)
  encoding <- if (validUTF8(if (nzchar(complete)) complete else sample)) {
    "UTF-8-BOM"
  } else {
    "latin1"
  }
  read <- function(sep, header, nrows = -1) {
    utils::read.table(
      path,
      sep = sep,
      header = header,
      nrows = nrows,
      quote = "\"",
      comment.char = "",
      fill = TRUE,
      check.names = FALSE,
      strip.white = TRUE,
      blank.lines.skip = TRUE,
      na.strings = missing_strings,
      stringsAsFactors = FALSE,
      fileEncoding = encoding,
      encoding = "UTF-8"
    )
  }
  # The delimiter that splits the first rows into the most columns.
  seps <- c(",", ";", "\t", "|")
  widths <- vapply(
    seps,
    function(s) tryCatch(ncol(read(s, TRUE, 50)), error = function(e) 0L),
    integer(1)
  )
  if (max(widths) == 0L) {
    stop("the file could not be read as a table.")
  }
  sep <- seps[which.max(widths)]
  d <- read(sep, TRUE)
  notes <- if (encoding == "latin1") {
    "The file was not UTF-8 and was read as Latin-1 text."
  } else {
    character()
  }
  # A first row of numbers is data, not column names.
  if (ncol(d) > 1 && all(!is.na(suppressWarnings(as.numeric(names(d)))))) {
    d <- read(sep, FALSE)
    names(d) <- paste0("column", seq_along(d))
    notes <- c(
      notes,
      "No header row was found, so the columns are named column1, column2, and so on."
    )
  }
  list(data = d, notes = notes)
}

read_excel_table <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("install the readxl package to open Excel files, or upload a CSV.")
  }
  d <- as.data.frame(readxl::read_excel(
    path,
    na = missing_strings,
    .name_repair = "minimal"
  ))
  notes <- character()
  if (ncol(d) > 1 && all(!is.na(suppressWarnings(as.numeric(names(d)))))) {
    d <- as.data.frame(readxl::read_excel(
      path,
      na = missing_strings,
      col_names = FALSE,
      .name_repair = "minimal"
    ))
    names(d) <- paste0("column", seq_along(d))
    notes <- "No header row was found, so the columns are named column1, column2, and so on."
  }
  list(data = d, notes = notes)
}

# SPSS and Stata files carry variable labels (readable names) and value labels
# (names for codes); both are used for display. Codes stay numbers.
read_labelled_table <- function(path, ext) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "install the haven package to open SPSS or Stata files, or upload a CSV."
    )
  }
  d <- as.data.frame(
    if (ext == "sav") haven::read_sav(path) else haven::read_dta(path)
  )
  labels <- vapply(
    d,
    function(v) {
      l <- attr(v, "label", exact = TRUE)
      if (length(l) && nzchar(l)) as.character(l) else NA_character_
    },
    ""
  )
  value_labels <- lapply(d, function(v) {
    l <- attr(v, "labels", exact = TRUE)
    l <- l[!is.na(l)]
    if (length(l)) stats::setNames(names(l), as.character(unname(l)))
  })
  value_labels <- Filter(Negate(is.null), value_labels)
  # Plain vectors: labels and formats are kept above, not on the columns.
  d[] <- lapply(d, function(v) {
    if (is.atomic(v) && !is.factor(v) && !inherits(v, c("Date", "POSIXt"))) {
      v <- unclass(v)
      attributes(v) <- NULL
    }
    v
  })
  list(
    data = d,
    labels = labels[!is.na(labels)],
    value_labels = value_labels,
    notes = if (any(!is.na(labels)) || length(value_labels)) {
      "Variable and value labels from the file are used as readable names; coded values stay numbers."
    } else {
      character()
    }
  )
}

# Usable names and no fully empty columns.
tidy_table <- function(out) {
  d <- out$data
  if (!nrow(d) || !ncol(d)) {
    stop("the file has no data.")
  }
  nm <- trimws(names(d))
  blank <- is.na(nm) | nm == "" | grepl("^\\.\\.\\.[0-9]+$", nm)
  nm[blank] <- paste0("column", which(blank))
  names(d) <- make.unique(nm)
  notes <- out$notes
  if (any(blank)) {
    notes <- c(
      notes,
      sprintf("%d unnamed column(s) were named by position.", sum(blank))
    )
  }
  empty_col <- vapply(
    d,
    function(v) {
      all(is.na(v) | (is.character(v) & trimws(v) %in% missing_strings))
    },
    logical(1)
  )
  if (any(empty_col)) {
    d <- d[!empty_col]
    notes <- c(
      notes,
      sprintf("%d empty column(s) were removed.", sum(empty_col))
    )
  }
  if (!ncol(d)) {
    stop("the file has no data.")
  }
  # Empty rows stay (the analysis leaves them out), so the exported rows line
  # up with the rows of the file.
  rownames(d) <- NULL
  list(
    data = d,
    labels = out$labels[intersect(names(out$labels), names(d))],
    value_labels = out$value_labels[intersect(
      names(out$value_labels),
      names(d)
    )],
    notes = notes
  )
}

# Observed values as the package sees them: missing-value text and non-finite
# numbers count as missing.
distinct <- function(v) {
  if (is.logical(v)) {
    v <- as.numeric(v)
  }
  if (is.factor(v)) {
    v <- as.character(v)
  }
  if (is.character(v)) {
    v <- trimws(v)
    v[v %in% missing_strings] <- NA_character_
  }
  if (is.numeric(v)) {
    v[!is.finite(v)] <- NA_real_
  }
  sort(unique(v[!is.na(v)]))
}

# ---- Formatting --------------------------------------------------------------

fmt <- function(x, d = 2) {
  ifelse(
    is.finite(x),
    sub("^-", "\u2212", formatC(x, digits = d, format = "f", big.mark = ",")),
    "unavailable"
  )
}
fmt_n <- function(x) formatC(x, format = "d", big.mark = ",")
count_of <- function(k, word) {
  paste(fmt_n(k), if (k == 1) word else paste0(word, "s"))
}
code_tag <- function(x) as.character(tags$code(x))
more_less <- function(x, more = "larger", less = "smaller") {
  if (x >= 0) more else less
}
and_list <- function(x) {
  if (length(x) <= 1) {
    paste(x)
  } else {
    paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
  }
}
or_list <- function(x) {
  if (length(x) <= 1) {
    paste(x)
  } else {
    paste(paste(x[-length(x)], collapse = ", "), "or", x[length(x)])
  }
}

# ---- Plain-language explanations shown on hover ------------------------------

terms <- c(
  dependent = "The variable you want to explain (Y): binary, with any two values (such as voted (1) and did not vote (0)), or continuous (such as a test score).",
  independent = "The variable whose effect on Y you want to study (W): binary, with any two values (such as treated (1) and untreated (0)), or continuous (such as hours of use).",
  characteristics = "Background information about each person (X), such as age or education, that might moderate the treatment effect of W on Y. Text columns become 0/1 indicators.",
  controls = "Characteristics to adjust for that are not of interest as moderators, such as common causes of W and Y. The forest uses them to adjust the comparison of Y between values of W.",
  moderate = "A characteristic moderates the treatment effect when the effect is larger or smaller depending on its value: a programme might, for example, help younger people more than older people.",
  treatment_effect = "How much Y changes with W for people with similar characteristics: the difference between the two groups of a binary W, or the change per one-unit increase in a continuous W. It is a causal effect if W was randomly assigned, or every common cause of W and Y is among the characteristics X.",
  heterogeneity = "Differences in the treatment effect of W on Y from one kind of person to another: the effect is heterogeneous when it is larger for some people than for others.",
  causal_forest = "A machine-learning method that grows many decision trees to find people for whom the treatment effect of W on Y is larger or smaller. It builds on random forests.",
  ate = "The effect of W on Y averaged over everyone: the gap in Y between the two groups of W among people with similar characteristics. It is a causal effect only if W was randomly assigned, or every common cause of W and Y is among the characteristics X.",
  ape = "The effect of a numeric W averaged over everyone: how much Y changes per one-unit increase in W among people with similar characteristics. It is a causal effect only if W was randomly assigned, or every common cause of W and Y is among the characteristics X.",
  sd_units = "The effect divided by how much Y varies (its standard deviation within the two groups of W, before any adjustment), so that effects on dependent variables measured in different units can be compared. For a numeric W, it is the change in Y, in standard deviations, for a one-standard-deviation increase in W.",
  cate = "The forest's estimate of the treatment effect for people with a given combination of characteristics: a conditional average treatment effect (CATE). It is an average for similar people, not one person's own effect.",
  honest = "Each tree uses one half of its data to choose its splits and the other half to estimate effects, which guards against mistaking noise for a pattern.",
  doubly_robust = "A way of averaging that combines the forest's estimates with the observed values of Y, so the averages stay accurate even if one of its two underlying models is off.",
  moderator = "A characteristic along which the effect of W may change. The forest can split on a potential moderator to find people for whom the effect is larger or smaller.",
  importance = "How often, and how early, the forest's trees split on a characteristic. It shows what the forest relied on to find people with larger or smaller effects.",
  quantile_bins = "Groups of about equal size, formed by cutting a numeric characteristic at its quantiles.",
  loess = "A smooth line that follows the average: at each value, it averages the people with nearby values, giving the closest the most weight.",
  noise = "Random error from estimating with a limited sample. It can make estimated treatment effects spread more widely than the true effects do.",
  association = "Unless W was randomly assigned, a difference between groups may reflect other causes, so it is an association rather than a causal effect.",
  treated = "Treatment effects are this group minus the other.",
  y_one = "Effects are differences in the share of this value.",
  ids = "A column that identifies each row, such as a respondent number. It is copied into the downloaded file of estimated treatment effects, next to each row's number, so you can merge them back into your data.",
  weights = "Survey weights make a sample represent a population: people with larger weights count more in the forest and in every average.",
  clusters = "Groups of related observations, such as students in the same school or people in the same household. The forest then samples whole clusters.",
  trees = "A decision tree asks a series of yes/no questions about the characteristics (such as: is age over 50?) to sort people into small groups. More trees give steadier results but take longer.",
  seed = "A number that fixes the forest's random choices, so the same data and settings always give the same results."
)
# A word or phrase with its explanation on hover (and keyboard focus). Written
# as one string so no stray space appears before following punctuation.
term <- function(key, text) {
  template <- paste0(
    '<span class="term" tabindex="0">%s',
    '<span class="tip" role="tooltip">%s</span></span>'
  )
  sprintf(
    template,
    htmltools::htmlEscape(text),
    htmltools::htmlEscape(terms[[key]])
  )
}
# A field label that explains itself on hover.
label <- function(key, text) HTML(term(key, text))
# A minimal expander for text that is useful but not needed at first sight.
fold <- function(summary, ..., open = FALSE) {
  tags$details(
    class = "fold",
    open = if (open) NA,
    tags$summary(summary),
    div(class = "fold-body", ...)
  )
}

# The repository, and GitHub's mark for the link to it.
repo <- "https://github.com/invisibleinfo/heterogeneity"
# An introduction to conditional average treatment effects (Stanford tutorial).
cate_url <- paste0(
  "https://bookdown.org/stanfordgsbsilab/ml-ci-tutorial/",
  "hte-i-binary-treatment.html"
)
github_mark <- paste0(
  '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="',
  "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0",
  "-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.",
  "13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33",
  ".66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-",
  "2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.",
  "68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51",
  ".56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1",
  ".07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.5",
  "8-8-8-8z",
  '"/></svg>'
)
# A link that works as a Shiny action button, without stray spaces.
action_link <- function(id, text) {
  tags$a(id = id, href = "#", class = "action-button", text)
}

# ---- Look --------------------------------------------------------------------

fonts <- paste0(
  "https://fonts.googleapis.com/css2",
  "?family=Inter:wght@400;500;600",
  "&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600",
  "&family=IBM+Plex+Mono",
  "&display=swap"
)
theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#18181b",
  primary = "#18181b",
  secondary = "#52525b",
  base_font = font_collection(
    "Inter",
    "-apple-system",
    "BlinkMacSystemFont",
    "Segoe UI",
    "Helvetica Neue",
    "Arial",
    "sans-serif"
  ),
  heading_font = font_collection(
    "Source Serif 4",
    "Charter",
    "Iowan Old Style",
    "Georgia",
    "serif"
  ),
  code_font = font_collection(
    "IBM Plex Mono",
    "SFMono-Regular",
    "Menlo",
    "Consolas",
    "monospace"
  ),
  "enable-shadows" = FALSE,
  "border-radius" = "6px",
  "link-color" = "#18181b",
  "input-border-color" = "#d4d4d8",
  "input-focus-border-color" = "#18181b",
  "focus-ring-color" = "rgba(24, 24, 27, .10)",
  "accordion-button-active-bg" = "#ffffff",
  "accordion-button-active-color" = "#18181b"
)

css <- "
  :root {
    --bg: #ffffff;
    --raised: #ffffff;
    --surface: #f8f8f9;
    --ink: #18181b;
    --ink-2: #3f3f46;
    --ink-3: #71717a;
    --ink-4: #a1a1aa;
    --rule: #e4e4e7;
    --rule-2: #d4d4d8;
    --hover: #000000;
    --danger: #7f1d1d;
    --shadow: rgba(24, 24, 27, .12);
    --serif: 'Source Serif 4', Charter, Georgia, serif;
    --sans: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica,
      Arial, sans-serif;
  }

  html[data-bs-theme='dark'] {
    --bg: #0c0c0e;
    --raised: #18181b;
    --surface: #141417;
    --ink: #f4f4f5;
    --ink-2: #d4d4d8;
    --ink-3: #a1a1aa;
    --ink-4: #71717a;
    --rule: #27272a;
    --rule-2: #3f3f46;
    --hover: #ffffff;
    --danger: #fca5a5;
    --shadow: rgba(0, 0, 0, .5);
    --bs-body-bg: #0c0c0e;
    --bs-body-color: #f4f4f5;
    --bs-emphasis-color: #ffffff;
    --bs-border-color: #3f3f46;
    --bs-secondary-color: #a1a1aa;
    --bs-tertiary-color: #71717a;
    --bs-secondary-bg: #18181b;
    --bs-tertiary-bg: #18181b;
    --bs-link-color: #f4f4f5;
    --bs-link-color-rgb: 244, 244, 245;
    --bs-link-hover-color: #ffffff;
    --bs-link-hover-color-rgb: 255, 255, 255;
    color-scheme: dark;
  }

  html {
    -webkit-font-smoothing: antialiased;
    text-rendering: optimizeLegibility;
  }

  body {
    background: var(--bg);
    color: var(--ink);
    font-size: 15.5px;
    line-height: 1.6;
  }

  .wrap {
    max-width: 880px;
    margin: 0 auto;
    padding: 56px 28px 96px;
  }

  .eyebrow {
    display: flex;
    align-items: center;
    gap: 10px;
    margin-bottom: 20px;
    font: 600 11px/1 var(--sans);
    letter-spacing: .16em;
    text-transform: uppercase;
    color: var(--ink-3);
  }

  .eyebrow .dot {
    width: 3px;
    height: 3px;
    border-radius: 50%;
    background: var(--ink-4);
  }

  .eyebrow .gh {
    margin-left: auto;
    display: inline-flex;
    align-items: center;
    gap: 6px;
    padding: 6px 11px;
    border: 1px solid var(--rule-2);
    border-radius: 999px;
    color: var(--ink-2);
    text-decoration: none;
    letter-spacing: .08em;
  }

  .eyebrow .gh:hover,
  .eyebrow .gh:focus {
    color: var(--ink);
    border-color: var(--ink-4);
    background: var(--surface);
  }

  .eyebrow .gh svg {
    width: 13px;
    height: 13px;
    fill: currentColor;
  }

  .eyebrow bslib-input-dark-mode {
    --text-1: var(--ink-2);
    --text-2: var(--ink-4);
  }

  h1.title {
    font: 600 2.55rem/1.12 var(--serif);
    letter-spacing: -.02em;
    margin: 0 0 1rem;
    color: var(--ink);
  }

  .lead {
    font-size: 1.07rem;
    color: var(--ink-2);
    max-width: 41rem;
    margin-bottom: 1.4rem;
  }

  .sec {
    border-top: 1px solid var(--ink);
    padding: 28px 0 36px;
  }

  .sec h2 {
    font: 600 1.5rem/1.25 var(--serif);
    letter-spacing: -.01em;
    margin: 0 0 1.2rem;
    color: var(--ink);
  }

  .sub {
    border-top: 1px solid var(--rule);
    padding-top: 1.5rem;
    margin-top: 1.9rem;
  }

  .sub:first-child {
    border-top: 0;
    padding-top: 0;
    margin-top: 0;
  }

  .sub h3 {
    font: 600 1.16rem/1.3 var(--serif);
    margin: 0 0 .75rem;
    color: var(--ink);
  }

  .moderator {
    border-top: 1px solid var(--rule);
    padding-top: 1.2rem;
    margin-top: 1.5rem;
  }

  .moderator h4 {
    font: 600 1.04rem/1.3 var(--serif);
    margin: 0 0 .7rem;
    color: var(--ink);
  }

  .moderator h4 .rank {
    color: var(--ink-4);
    font-weight: 400;
    margin-right: .5rem;
  }

  .moderator .second {
    max-width: 24rem;
  }

  .footnote {
    font-size: .84rem;
    color: var(--ink-3);
    margin: -.3rem 0 .6rem;
  }

  /* The first rows of the data, scrolling sideways. */
  .preview-label {
    font: 600 11.5px/1.2 var(--sans);
    text-transform: uppercase;
    letter-spacing: .08em;
    color: var(--ink-3);
    margin: .3rem 0 .45rem;
  }

  .preview {
    overflow-x: auto;
    border: 1px solid var(--rule);
    border-radius: 6px;
    margin-bottom: 1.5rem;
  }

  .preview table {
    border-collapse: collapse;
    width: max-content;
    min-width: 100%;
    font-size: .8rem;
    white-space: nowrap;
    font-variant-numeric: tabular-nums;
  }

  .preview th {
    background: var(--surface);
    color: var(--ink);
    font-weight: 600;
    text-align: left;
    padding: 6px 12px;
    border-bottom: 1px solid var(--rule);
  }

  .preview td {
    color: var(--ink-2);
    padding: 5px 12px;
    border-bottom: 1px solid var(--rule);
  }

  .preview tr:last-child td {
    border-bottom: 0;
  }

  .preview td.na {
    color: var(--ink-4);
  }

  /* Results stay hidden (but are drawn) until every plot is ready. */
  .sec.pending {
    visibility: hidden;
    height: 0;
    overflow: hidden;
    padding: 0;
    border: 0;
  }

  @media (max-width: 680px) {
    h1.title {
      font-size: 2rem;
    }
  }

  .control-label,
  .form-label {
    font: 600 11.5px/1.2 var(--sans);
    text-transform: uppercase;
    letter-spacing: .08em;
    color: var(--ink-3);
    margin-bottom: .45rem;
  }

  .shiny-input-container {
    width: 100% !important;
    margin-bottom: 1.15rem;
  }

  .form-control,
  .selectize-control .selectize-input,
  .form-select {
    font-size: .95rem;
    border-color: var(--rule-2);
    box-shadow: none;
  }

  .form-control,
  .selectize-input,
  .selectize-input input,
  .selectize-dropdown,
  .form-select {
    background-color: var(--bg);
    color: var(--ink);
  }

  .selectize-dropdown {
    border-color: var(--rule-2);
  }

  .selectize-dropdown .option.active,
  .selectize-dropdown .active {
    background: var(--surface);
    color: var(--ink);
  }

  .selectize-control .selectize-input.focus,
  .form-control:focus,
  .form-select:focus {
    border-color: var(--ink);
    box-shadow: 0 0 0 3px var(--shadow);
  }

  .selectize-control.multi .selectize-input > div {
    background: var(--surface);
    border: 1px solid var(--rule);
    border-radius: 4px;
    color: var(--ink);
    padding: 1px 6px;
  }

  .btn-file {
    background: var(--bg);
    color: var(--ink);
    border-color: var(--rule-2);
  }

  .form-check-input {
    background-color: var(--bg);
    border-color: var(--rule-2);
  }

  .form-check-input:checked {
    background-color: var(--ink);
    border-color: var(--ink);
  }

  html[data-bs-theme='dark'] .form-check-input:checked {
    background-color: #71717a;
    border-color: #71717a;
  }

  .help {
    font-size: .84rem;
    color: var(--ink-3);
    margin: -.6rem 0 1.1rem;
  }

  .help a,
  .help .action-button {
    color: var(--ink);
    font-weight: 500;
  }

  .links {
    font-size: .82rem;
    margin: -.7rem 0 1.1rem;
    color: var(--ink-4);
  }

  .links a {
    color: var(--ink-2);
  }

  .muted {
    color: var(--ink-2);
  }

  .prose p {
    max-width: 44rem;
  }

  a {
    color: var(--ink);
  }

  code {
    font-size: .85em;
    color: var(--ink);
    background: var(--surface);
    border: 1px solid var(--rule);
    border-radius: 4px;
    padding: .05em .35em;
  }

  .shiny-table table {
    width: 100%;
    border-collapse: collapse;
    border-top: 1.5px solid var(--ink);
    border-bottom: 1.5px solid var(--ink);
    font-variant-numeric: tabular-nums;
    font-size: .9rem;
    margin: .5rem 0 1.1rem;
    --bs-table-bg: transparent;
    --bs-table-color: var(--ink-2);
    color: var(--ink-2);
  }

  .shiny-table th {
    border-bottom: 1px solid var(--ink) !important;
    font-weight: 600;
    padding: 8px 14px 8px 0;
    color: var(--ink);
  }

  .shiny-table td {
    border: none !important;
    padding: 6px 14px 6px 0;
    vertical-align: top;
    color: var(--ink-2);
  }

  .shiny-table {
    width: 100%;
  }

  .chart {
    display: block;
    width: 100%;
    margin: .5rem 0 .7rem;
  }

  .caption {
    font-size: .92rem;
    color: var(--ink-2);
    line-height: 1.6;
    max-width: 44rem;
    margin-bottom: .4rem;
  }

  .caution {
    font-size: .88rem;
    color: var(--danger);
    margin: .2rem 0 .8rem;
    padding-left: 1.1rem;
  }

  /* Minimal expanders */
  details.fold {
    margin: .2rem 0 1rem;
  }

  details.fold > summary {
    list-style: none;
    display: inline-flex;
    align-items: center;
    gap: 7px;
    cursor: pointer;
    font: 500 .84rem/1.4 var(--sans);
    color: var(--ink-3);
  }

  details.fold > summary::-webkit-details-marker {
    display: none;
  }

  details.fold > summary::before {
    content: '';
    width: 6px;
    height: 6px;
    border-right: 1.5px solid currentColor;
    border-bottom: 1.5px solid currentColor;
    transform: rotate(-45deg);
    transition: transform .15s ease;
  }

  details.fold[open] > summary::before {
    transform: rotate(45deg);
  }

  details.fold > summary:hover,
  details.fold > summary:focus {
    color: var(--ink);
    outline: none;
  }

  .fold-body {
    margin-top: .55rem;
    font-size: .9rem;
    color: var(--ink-2);
    line-height: 1.6;
    max-width: 44rem;
  }

  .fold-body p {
    margin-bottom: .55rem;
  }

  .fold-body ul {
    margin: 0;
    padding-left: 1.05rem;
  }

  .fold-body li {
    margin: .18rem 0;
  }

  .fold-body li::marker {
    color: var(--ink-4);
  }

  .notes {
    margin: .2rem 0 0;
    padding-left: 1.1rem;
    color: var(--ink-3);
    font-size: .88rem;
  }

  .report-box {
    border: 1px solid var(--rule);
    background: var(--surface);
    border-radius: 8px;
    padding: 1.15rem 1.35rem;
  }

  .report-box ul {
    margin: 0;
    padding-left: 1.1rem;
  }

  .report-box li {
    margin-bottom: .55rem;
    color: var(--ink-2);
  }

  .report-box li:last-child {
    margin-bottom: 0;
  }

  .report-box b {
    color: var(--ink);
    font-weight: 600;
  }

  .btn {
    font: 500 .88rem/1 var(--sans);
    border-radius: 6px;
    padding: .66rem 1.15rem;
  }

  .btn-primary {
    background: var(--ink);
    border-color: var(--ink);
    color: var(--bg);
  }

  .btn-primary:hover,
  .btn-primary:focus,
  .btn-primary:active {
    background: var(--hover);
    border-color: var(--hover);
    color: var(--bg);
  }

  .btn-outline-dark {
    border-color: var(--rule-2);
    color: var(--ink);
    background: var(--bg);
  }

  .btn-outline-dark:hover,
  .btn-outline-dark:focus {
    background: var(--surface);
    color: var(--ink);
    border-color: var(--ink-4);
  }

  .btn-sm {
    padding: .5rem .85rem;
    font-size: .82rem;
  }

  .accordion {
    --bs-accordion-bg: var(--bg);
    --bs-accordion-color: var(--ink);
    --bs-accordion-border-color: var(--rule);
    --bs-accordion-btn-color: var(--ink);
    --bs-accordion-active-bg: var(--bg);
    --bs-accordion-active-color: var(--ink);
    --bs-accordion-btn-focus-box-shadow: none;
  }

  .accordion-button {
    font: 500 .92rem/1 var(--sans);
    color: var(--ink);
  }

  html[data-bs-theme='dark'] .accordion-button::after {
    filter: invert(1);
  }

  .progress {
    height: 3px;
    background: var(--rule);
  }

  .progress-bar {
    background: var(--ink);
    color: transparent;
  }

  .shiny-notification {
    background: var(--raised);
    color: var(--ink);
    border-radius: 8px;
    border: 1px solid var(--rule-2);
    box-shadow: 0 10px 30px var(--shadow);
  }

  .shiny-output-error-validation {
    color: var(--danger);
  }

  /* Explanations on hover and keyboard focus. */
  .term {
    position: relative;
    border-bottom: 1px dotted var(--ink-4);
    cursor: help;
  }

  .term:focus {
    outline: none;
    border-bottom-color: var(--ink);
  }

  .term .tip {
    position: absolute;
    left: 50%;
    bottom: calc(100% + 9px);
    transform: translateX(-50%);
    width: 18rem;
    max-width: 80vw;
    padding: .75rem .9rem;
    background: var(--raised);
    border: 1px solid var(--rule);
    border-radius: 8px;
    box-shadow: 0 12px 32px var(--shadow);
    font: 400 .82rem/1.5 var(--sans);
    color: var(--ink-2);
    text-transform: none;
    letter-spacing: normal;
    text-align: left;
    white-space: normal;
    visibility: hidden;
    opacity: 0;
    transition: opacity .12s ease;
    z-index: 30;
    pointer-events: none;
  }

  .term:hover .tip,
  .term:focus .tip {
    visibility: visible;
    opacity: 1;
  }

  .control-label .term .tip {
    left: 0;
    transform: none;
  }
"

js <- "
Shiny.addCustomMessageHandler('scroll_to', function(id) {
  setTimeout(function() {
    var el = document.getElementById(id);
    if (el) {
      el.scrollIntoView({behavior: 'smooth', block: 'start'});
    }
  }, 200);
});

// Show the results once every plot in them has been drawn, so that no heading
// appears before its plot (after two minutes, show them anyway). Meanwhile the
// forest's progress bar keeps moving; the server closes it when told.
Shiny.addCustomMessageHandler('reveal_results', function(id) {
  var started = Date.now();

  var drawn = function(out) {
    var img = out.querySelector('img');
    var failed = out.classList.contains('shiny-output-error');
    return failed || (img && img.complete && img.naturalWidth > 0);
  };

  var check = function() {
    var sec = document.getElementById(id);
    if (!sec) {
      return;
    }
    var outputs = sec.querySelectorAll('.shiny-plot-output');
    var plots = Array.prototype.slice.call(outputs);
    var done = plots.filter(drawn).length;
    var bar = document.querySelector('.shiny-notification .progress-bar');
    if (bar && plots.length) {
      bar.style.width = (60 + 40 * done / plots.length) + '%';
    }
    if (done === plots.length || Date.now() - started > 120000) {
      sec.classList.remove('pending');
      sec.scrollIntoView({behavior: 'smooth', block: 'start'});
      Shiny.setInputValue('results_drawn', Date.now(), {priority: 'event'});
    } else {
      setTimeout(check, 150);
    }
  };

  setTimeout(check, 150);
});"

section <- function(id, title, ...) {
  div(class = "sec", id = id, tags$h2(title), ...)
}
sub_section <- function(id, title, ...) {
  div(class = "sub", id = paste0("unit-", id), tags$h3(title), ...)
}
help <- function(...) div(class = "help", ...)

# ---- UI ----------------------------------------------------------------------

ui <- page(
  theme = theme,
  title = "Exploring heterogeneity with causal forests",
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", href = fonts),
    tags$style(HTML(css)),
    tags$script(HTML(js))
  ),
  div(
    class = "wrap",
    div(
      class = "eyebrow",
      span("heterogeneity"),
      span(class = "dot"),
      span("R package and application"),
      tags$a(
        class = "gh",
        href = repo,
        target = "_blank",
        rel = "noopener",
        HTML(github_mark),
        "GitHub"
      ),
      input_dark_mode(id = "mode", mode = "light")
    ),
    h1(class = "title", "Exploring heterogeneity with causal forests"),
    p(
      class = "lead",
      HTML(paste0(
        "Upload a dataset, choose a ",
        term("dependent", "dependent variable"),
        " (Y), an ",
        term("independent", "independent variable"),
        " (W), and relevant ",
        term("characteristics", "characteristics"),
        " (X) that might ",
        term("moderate", "moderate"),
        " the ",
        term("treatment_effect", "treatment effect"),
        " of W on Y. The application then uses a ",
        term("causal_forest", "causal forest"),
        " to explore patterns of ",
        term("heterogeneity", "heterogeneity"),
        " in treatment effects."
      ))
    ),
    section(
      "choose",
      "Choose variables",
      fileInput(
        "file",
        "Data file",
        accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".sav", ".dta"),
        placeholder = "No file selected"
      ),
      help(HTML(paste0(
        "Without data of your own, you can ",
        action_link("example", "load the 2024 General Social Survey example"),
        ". Uploaded files are used only during your session and are not stored; refreshing the page deletes them."
      ))),
      uiOutput("example_error"),
      fold(
        "What your file needs",
        tags$ul(
          tags$li(
            "One row per person (or unit), with column names in the first row: CSV, TSV, Excel, SPSS (.sav) or Stata (.dta)."
          ),
          tags$li(
            "A dependent variable (Y): binary, with any two values (such as voted or did not vote), or continuous (such as a test score)."
          ),
          tags$li(
            "An independent variable (W): binary, with any two values (such as treated or untreated), or continuous (such as hours of use)."
          ),
          tags$li(
            "Characteristics (X): numeric columns, or text columns with up to 20 categories."
          ),
          tags$li(
            "Rows missing Y or W are not included in the analysis; missingness in X is handled by the causal forest."
          ),
          tags$li("The estimates can be noisier for smaller samples.")
        )
      ),
      uiOutput("data_note"),
      uiOutput("preview"),
      uiOutput("fields")
    ),
    div(id = "results")
  )
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(
    source = "none",
    res = NULL,
    error = NULL,
    run_note = NULL,
    gss = NULL,
    gss_error = NULL
  )
  # The example ships with the package and is read the first time the link is
  # used.
  observeEvent(input$example, {
    rv$gss_error <- NULL
    if (is.null(rv$gss)) {
      rv$gss <- tryCatch(load_gss2024(), error = function(e) {
        rv$gss_error <- conditionMessage(e)
        NULL
      })
    }
    if (!is.null(rv$gss)) rv$source <- "example"
  })
  observeEvent(input$file, rv$source <- "upload")
  example <- reactive(identical(rv$source, "example"))
  output$example_error <- renderUI(
    if (!is.null(rv$gss_error)) {
      p(class = "shiny-output-error-validation", rv$gss_error)
    }
  )

  # The data and any labels: from the example, or from the uploaded file.
  upload <- reactive({
    req(input$file)
    tryCatch(
      read_upload(input$file$datapath, input$file$name),
      error = function(e) {
        validate(need(
          FALSE,
          paste("The file could not be read:", conditionMessage(e))
        ))
      }
    )
  })
  source_data <- reactive({
    if (example()) {
      return(list(
        data = rv$gss$data,
        labels = rv$gss$labels,
        value_labels = rv$gss$value_labels,
        notes = character()
      ))
    }
    upload()
  })
  data <- reactive(source_data()$data)
  labels <- reactive(source_data()$labels)
  num_cols <- reactive(names(Filter(
    function(x) (is.numeric(x) || is.logical(x)) && any(!is.na(x)),
    data()
  )))
  text_cols <- reactive(names(Filter(
    function(x) {
      (is.character(x) || is.factor(x)) && length(distinct(x)) %in% 2:20
    },
    data()
  )))
  bin_cols <- reactive(names(Filter(
    function(x) length(distinct(x)) == 2,
    data()
  )))
  opt <- function(id) {
    if (is.null(input[[id]]) || identical(input[[id]], "")) {
      NULL
    } else {
      input[[id]]
    }
  }
  # Choices show a readable label next to the column name when one is known.
  choices_for <- function(vars) {
    lab <- labels()
    if (!length(vars) || !length(lab)) {
      return(vars)
    }
    shown <- unname(lab[vars])
    shown[is.na(shown)] <- vars[is.na(shown)]
    stats::setNames(
      vars,
      ifelse(shown == vars, vars, paste0(shown, " (", vars, ")"))
    )
  }
  # Y and W are coded alike: two values that are not 0 and 1 get a choice of the
  # value coded 1.
  values_of <- function(v) {
    req(v, v %in% names(data()))
    distinct(data()[[v]])
  }
  needs_coding <- function(vals) {
    length(vals) == 2L && !identical(as.character(vals), c("0", "1"))
  }
  y_values <- reactive(values_of(input$y))
  w_values <- reactive(values_of(input$w))
  y_needs_coding <- reactive(needs_coding(y_values()))
  w_needs_coding <- reactive(needs_coding(w_values()))
  # The two values with readable names, and a radio button to choose the one
  # coded 1.
  coding_choice <- function(id, key, v, vals) {
    vals <- as.character(vals)
    shown <- vals
    codes <- source_data()$value_labels[[v]]
    if (length(codes)) {
      shown[vals %in% names(codes)] <- unname(codes[vals[
        vals %in% names(codes)
      ]])
    }
    radioButtons(
      id,
      label(key, paste("Value of", v, "coded 1")),
      choices = stats::setNames(vals, shown),
      selected = vals[2],
      inline = TRUE
    )
  }

  # ---- Choosing variables: once data are loaded, every field is shown --------

  output$data_note <- renderUI({
    src <- source_data()
    d <- src$data
    if (example()) {
      return(p(
        class = "muted",
        sprintf(
          "Data from %s respondents from the 2024 General Social Survey.",
          fmt_n(nrow(d))
        )
      ))
    }
    tagList(
      p(
        class = "muted",
        sprintf(
          "Read %s rows and %d columns (%d numeric, %d text with 2 to 20 categories).",
          fmt_n(nrow(d)),
          ncol(d),
          length(num_cols()),
          length(text_cols())
        )
      ),
      if (length(src$notes)) {
        tags$ul(class = "notes", lapply(src$notes, tags$li))
      }
    )
  })

  # The first 10 rows, all columns, as read; a column's label shows on hover.
  output$preview <- renderUI({
    d <- tryCatch(data(), error = function(e) NULL)
    req(d)
    top <- utils::head(d, 10)
    lab <- labels()
    cell <- function(v) {
      if (length(v) != 1L || is.na(v)) {
        return(tags$td(class = "na", "NA"))
      }
      tags$td(
        if (is.numeric(v)) {
          format(v, digits = 6, trim = TRUE)
        } else {
          as.character(v)
        }
      )
    }
    head_cell <- function(n) {
      tags$th(title = if (length(lab) && !is.na(lab[n])) unname(lab[n]), n)
    }
    tagList(
      div(class = "preview-label", sprintf("First %d rows", nrow(top))),
      div(
        class = "preview",
        tags$table(
          tags$thead(tags$tr(lapply(names(top), head_cell))),
          tags$tbody(lapply(seq_len(nrow(top)), function(i) {
            tags$tr(lapply(top[i, , drop = TRUE], cell))
          }))
        )
      )
    )
  })

  # The columns each field offers. Y, W and X exclude one another, so each
  # list is updated when Y or W changes.
  y_pool <- function(w) setdiff(union(bin_cols(), num_cols()), w)
  w_pool <- function(y) setdiff(union(bin_cols(), num_cols()), y)
  x_pool <- function(y, w) setdiff(c(num_cols(), text_cols()), c(y, w))

  # Drawn once per data set (it does not depend on the choices themselves).
  output$fields <- renderUI({
    # A file that cannot be read is reported once, above.
    req(tryCatch(data(), error = function(e) NULL))
    ex <- example()
    none <- c("None" = "")
    tagList(
      if (length(y_pool(NULL))) {
        selectizeInput(
          "y",
          label("dependent", "Dependent variable (Y)"),
          choices = c("", choices_for(y_pool(NULL))),
          options = list(placeholder = "Choose a column")
        )
      } else {
        p(
          class = "muted",
          "No column has two values or numbers, so none can be the dependent variable (Y)."
        )
      },
      uiOutput("pick_y_one"),
      selectizeInput(
        "w",
        label("independent", "Independent variable (W)"),
        choices = c("", choices_for(w_pool(NULL))),
        options = list(placeholder = "Choose a column")
      ),
      uiOutput("pick_treated"),
      selectizeInput(
        "x",
        label("characteristics", "Candidate moderators (X)"),
        choices = choices_for(x_pool(NULL, NULL)),
        multiple = TRUE,
        options = list(
          placeholder = "Choose one or more columns",
          plugins = list("remove_button")
        )
      ),
      div(
        class = "links",
        action_link("x_all", "Select all"),
        " / ",
        action_link("x_none", "Clear")
      ),
      selectizeInput(
        "controls",
        label("controls", "Controls (X, optional)"),
        choices = choices_for(x_pool(NULL, NULL)),
        multiple = TRUE,
        options = list(placeholder = "None", plugins = list("remove_button"))
      ),
      accordion(
        open = FALSE,
        class = "mt-1 mb-3",
        accordion_panel(
          "Change default parameters",
          selectInput(
            "ids",
            label("ids", "ID column"),
            c(none, choices_for(names(data()))),
            selected = if (ex) "id" else ""
          ),
          selectInput(
            "weights",
            label("weights", "Weight column"),
            c(none, choices_for(num_cols()))
          ),
          selectInput(
            "clusters",
            label("clusters", "Cluster column"),
            c(none, choices_for(names(data())))
          ),
          fluidRow(
            column(
              6,
              numericInput(
                "num_trees",
                label("trees", "Number of trees"),
                2000,
                min = 100,
                step = 500
              )
            ),
            column(
              6,
              numericInput(
                "seed",
                label("seed", "Random seed"),
                21042020,
                min = 0,
                step = 1
              )
            )
          )
        )
      ),
      actionButton("run", "Run the causal forest", class = "btn-primary mt-2"),
      uiOutput("run_note")
    )
  })
  # Moderators and controls share X's columns but never the same one.
  update_x <- function(y, w) {
    pool <- x_pool(y, w)
    moderators <- intersect(input$x, pool)
    controls <- setdiff(intersect(input$controls, pool), moderators)
    updateSelectizeInput(
      session,
      "x",
      choices = choices_for(setdiff(pool, controls)),
      selected = moderators
    )
    updateSelectizeInput(
      session,
      "controls",
      choices = choices_for(setdiff(pool, moderators)),
      selected = controls
    )
  }
  observeEvent(
    input$y,
    {
      updateSelectizeInput(
        session,
        "w",
        choices = c("", choices_for(w_pool(opt("y")))),
        selected = opt("w") %||% ""
      )
      update_x(opt("y"), opt("w"))
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$w,
    {
      updateSelectizeInput(
        session,
        "y",
        choices = c("", choices_for(y_pool(opt("w")))),
        selected = opt("y") %||% ""
      )
      update_x(opt("y"), opt("w"))
    },
    ignoreInit = TRUE
  )
  observeEvent(
    input$x,
    updateSelectizeInput(
      session,
      "controls",
      choices = choices_for(setdiff(x_pool(opt("y"), opt("w")), input$x)),
      selected = setdiff(input$controls, input$x)
    ),
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  observeEvent(
    input$controls,
    updateSelectizeInput(
      session,
      "x",
      choices = choices_for(setdiff(
        x_pool(opt("y"), opt("w")),
        input$controls
      )),
      selected = setdiff(input$x, input$controls)
    ),
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )
  # "Select all" leaves out the controls and the columns chosen as ID, weight
  # or cluster. In the GSS example it chooses the codebook's characteristics,
  # without those that measure the same thing as the chosen Y or W.
  all_moderators <- function() {
    pool <- setdiff(
      x_pool(opt("y"), opt("w")),
      c(input$controls, opt("ids"), opt("weights"), opt("clusters"))
    )
    if (!example()) {
      return(pool)
    }
    cb <- rv$gss$codebook
    same <- unlist(strsplit(
      cb$same_as[cb$variable %in% c(opt("y"), opt("w"))],
      ";",
      fixed = TRUE
    ))
    setdiff(intersect(pool, cb$variable[cb$role == "characteristic"]), same)
  }
  observeEvent(
    input$x_all,
    updateSelectizeInput(session, "x", selected = all_moderators())
  )
  observeEvent(
    input$x_none,
    updateSelectizeInput(session, "x", selected = character(0))
  )

  output$pick_y_one <- renderUI({
    req(y_needs_coding())
    coding_choice("y_one", "y_one", input$y, y_values())
  })
  output$pick_treated <- renderUI({
    req(w_needs_coding())
    coding_choice("treated", "treated", input$w, w_values())
  })
  output$run_note <- renderUI(
    if (!is.null(rv$run_note)) {
      p(class = "shiny-output-error-validation mt-2", rv$run_note)
    }
  )

  # ---- Run the forest, then show every result at once ------------------------

  # One progress note, "Growing the forest", from Run until every result is
  # drawn: the page reports when its plots are ready (input$results_drawn).
  waiting <- NULL
  stop_waiting <- function() {
    if (!is.null(waiting)) {
      waiting$close()
    }
    waiting <<- NULL
  }
  observeEvent(input$results_drawn, stop_waiting())
  clear_results <- function() {
    removeUI("#results > *", multiple = TRUE, immediate = TRUE)
    stop_waiting()
    rv$res <- NULL
    rv$error <- NULL
    rv$run_note <- NULL
  }
  # Any change to the data or the analysis settings makes shown results stale.
  observeEvent(
    list(
      rv$source,
      input$file,
      input$y,
      input$y_one,
      input$w,
      input$treated,
      input$x,
      input$controls,
      input$ids,
      input$weights,
      input$clusters,
      input$num_trees,
      input$seed
    ),
    clear_results(),
    ignoreInit = TRUE
  )

  observeEvent(input$run, {
    clear_results()
    missing <- c(
      if (is.null(opt("y"))) "a dependent variable (Y)",
      if (is.null(opt("w"))) "an independent variable (W)",
      if (!length(input$x)) "at least one candidate moderator (X)"
    )
    if (length(missing)) {
      rv$run_note <- paste0("Choose ", and_list(missing), " first.")
      return()
    }
    waiting <<- Progress$new(session)
    waiting$set(
      message = "Growing the forest",
      detail = "This can take a minute.",
      value = 0.3
    )
    res <- tryCatch(
      {
        is_example <- example()
        src <- source_data()
        named <- function(v) {
          l <- unname(src$labels[v])
          if (length(l) && !is.na(l)) l else v
        }
        continuous <- length(w_values()) > 2L
        # For a binary Y, the effect is on the share with the value coded 1.
        target <- if (length(y_values()) != 2L) {
          named(input$y)
        } else {
          one <- if (isTRUE(y_needs_coding())) {
            opt("y_one") %||% as.character(y_values()[2])
          } else {
            "1"
          }
          codes <- src$value_labels[[input$y]]
          paste0(
            "the share with ",
            named(input$y),
            " = ",
            if (one %in% names(codes)) codes[[one]] else one
          )
        }
        warnings <- character()
        report <- withCallingHandlers(
          explore_heterogeneity(
            data = data(),
            y = input$y,
            w = input$w,
            x = input$x,
            controls = if (length(input$controls)) input$controls,
            treated = if (isTRUE(w_needs_coding())) opt("treated") else NULL,
            y_one = if (isTRUE(y_needs_coding())) opt("y_one") else NULL,
            ids = opt("ids"),
            weights = opt("weights"),
            clusters = opt("clusters"),
            num_trees = input$num_trees %||% 2000,
            seed = input$seed %||% 21042020,
            labels = if (length(src$labels)) src$labels,
            value_labels = if (length(src$value_labels)) src$value_labels,
            effect_label = if (
              is_example && input$y == "rating_trump" && !continuous
            ) {
              rv$gss$effect_label
            } else {
              paste0(
                "Treatment effect on ",
                target,
                if (continuous) paste(" per unit of", named(input$w))
              )
            }
          ),
          warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        list(
          report = report,
          warnings = unique(warnings),
          y = input$y,
          w = input$w,
          ids = opt("ids")
        )
      },
      error = function(e) list(error = conditionMessage(e))
    )
    if (!is.null(res$error)) {
      stop_waiting()
      rv$error <- res$error
      insertUI(
        "#results",
        "beforeEnd",
        immediate = TRUE,
        section("run-error", "The analysis could not run", p(res$error))
      )
      session$sendCustomMessage("scroll_to", "run-error")
      return()
    }
    rv$res <- res
    waiting$set(value = 0.6)
    moderator_outputs(res$report)
    insertUI("#results", "beforeEnd", results_ui(facts()), immediate = TRUE)
    session$sendCustomMessage("reveal_results", "results-section")
  })

  # ---- Numbers and words used in the text ------------------------------------

  report <- reactive({
    req(rv$res)
    rv$res$report
  })
  # Readable labels are shown as text; raw column names and values as code.
  name_tag <- function(r, v) {
    if (is.null(r$settings$labels)) {
      vapply(v, code_tag, "")
    } else {
      unname(r$labels[v])
    }
  }
  facts <- reactive({
    r <- report()
    continuous <- identical(r$settings$w_type, "continuous")
    lab <- r$settings$w_labels
    generic <- !continuous &&
      all(
        tolower(unname(lab)) %in%
          c("0", "1", "no", "yes", "false", "true", "n", "y")
      )
    groups <- if (continuous) {
      c("", "")
    } else if (generic) {
      paste0(rv$res$w, " = ", lab)
    } else {
      unname(lab)
    }
    group_tags <- if (continuous) {
      groups
    } else if (generic) {
      paste0(code_tag(rv$res$w), " = ", unname(lab))
    } else if (!is.null(r$settings$value_labels[[rv$res$w]])) {
      groups
    } else {
      vapply(groups, code_tag, "")
    }
    y_binary <- identical(r$settings$y_type, "binary")
    y_one <- if (y_binary) unname(r$settings$y_labels[2])
    list(
      report = r,
      y = rv$res$y,
      w = rv$res$w,
      continuous = continuous,
      y_binary = y_binary,
      y_tag = if (y_binary) {
        sprintf("the share with %s = %s", code_tag(rv$res$y), y_one)
      } else {
        code_tag(rv$res$y)
      },
      y_text = if (y_binary) {
        sprintf("the share with %s = %s", rv$res$y, y_one)
      } else {
        rv$res$y
      },
      per = if (continuous) paste(" per unit of", code_tag(rv$res$w)) else "",
      per_text = if (continuous) paste(" per unit of", rv$res$w) else "",
      average_key = if (continuous) "ape" else "ate",
      average_name = if (continuous) {
        "average partial effect"
      } else {
        "average treatment effect"
      },
      g0 = groups[1],
      g1 = groups[2],
      g0_tag = group_tags[[1]],
      g1_tag = group_tags[[2]],
      ate = r$ate$estimate,
      std = r$ate$standardized %||% NA_real_,
      top = utils::head(r$moderators, 3),
      notes = r$notes[!caution(r$notes)],
      cautions = c(r$notes[caution(r$notes)], rv$res$warnings)
    )
  })
  # Notes that call for caution are shown; the rest sit in an expander.
  caution <- function(notes) {
    grepl(
      "^Only |^Some observations have few|^Average treatment effect unavailable",
      notes
    )
  }
  sub_title <- function(u) {
    switch(
      u,
      spread = "Distribution of estimated treatment effects, conditional on covariates",
      characteristics = "Candidate moderators of the treatment effect, ranked by importance",
      moderators = "Treatment effect by candidate moderator",
      export = "Export results"
    )
  }
  average_sentence <- function(f) {
    name <- paste0(
      term(f$average_key, "average treatment effect"),
      " of ",
      code_tag(f$w),
      " on ",
      f$y_tag,
      if (f$continuous) {
        paste0(
          ", per one-unit increase in ",
          code_tag(f$w),
          " (the average partial effect),"
        )
      }
    )
    if (!is.finite(f$ate)) {
      return(sprintf("The %s is unavailable; see the notes below.", name))
    }
    effect <- if (f$continuous) {
      sprintf("per one-unit increase in %s", code_tag(f$w))
    } else {
      sprintf("for %s than for %s", f$g1_tag, f$g0_tag)
    }
    in_sd <- if (is.finite(f$std)) {
      sprintf(
        " (<b>%s</b> %s%s)",
        fmt(f$std),
        term("sd_units", "standard deviations"),
        if (f$continuous) {
          paste(" per standard deviation of", code_tag(f$w))
        } else {
          ""
        }
      )
    } else {
      ""
    }
    points <- if (f$y_binary) {
      sprintf(" (%s percentage points)", fmt(100 * abs(f$ate), 1))
    } else {
      ""
    }
    sprintf(
      "The %s is <b>%s</b>%s: comparing people with similar characteristics, %s is on average %s%s %s %s.",
      name,
      fmt(f$ate),
      in_sd,
      f$y_tag,
      fmt(abs(f$ate)),
      points,
      more_less(f$ate, "higher", "lower"),
      effect
    )
  }
  importance_plot <- reactive(plot(report(), "importance"))
  # Every characteristic gets a row, so this plot grows with their number.
  importance_height <- function() max(260, 90 + 24 * nrow(report()$moderators))
  # Plots as shown on the page. Headings and the text below each plot replace
  # its title and caption, and the background is transparent. In dark mode,
  # text and grid lines turn light, white outlines take the page colour, and
  # dark teal turns lighter; text on heatmap tiles stays dark.
  dark <- reactive(identical(input$mode, "dark"))
  for_page <- function(p) {
    p <- p +
      ggplot2::labs(title = NULL, caption = NULL) +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(
          fill = "transparent",
          colour = NA
        )
      )
    if (!dark()) {
      return(p)
    }
    on_tiles <- any(vapply(
      p$layers,
      function(layer) inherits(layer$geom, "GeomTile"),
      logical(1)
    ))
    p$layers <- lapply(p$layers, function(layer) {
      # A copy, so the cached plot keeps its colours.
      copy <- ggplot2::ggproto(NULL, layer)
      a <- copy$aes_params
      for (k in intersect(c("fill", "colour"), names(a))) {
        if (identical(a[[k]], "#286b70")) {
          a[[k]] <- "#72a9ac"
        }
        if (identical(a[[k]], "#18181b")) {
          a[[k]] <- "#e4e4e7"
        }
        if (identical(a[[k]], "white")) a[[k]] <- "#0c0c0e"
      }
      if (inherits(copy$geom, "GeomText") && !on_tiles) {
        a$colour <- "#e4e4e7"
      }
      copy$aes_params <- a
      copy
    })
    p +
      ggplot2::theme(
        text = ggplot2::element_text(colour = "#e4e4e7"),
        axis.text = ggplot2::element_text(colour = "#a1a1aa"),
        panel.grid.major = ggplot2::element_line(colour = "#27272a"),
        panel.background = ggplot2::element_rect(
          fill = "transparent",
          colour = NA
        ),
        legend.background = ggplot2::element_rect(
          fill = "transparent",
          colour = NA
        )
      )
  }

  # ---- The results -----------------------------------------------------------

  results_ui <- function(f) {
    r <- f$report
    pending(section(
      "results-section",
      "Results",
      div(
        class = "prose",
        div(
          class = "sub",
          id = "unit-summary",
          p(HTML(average_sentence(f))),
          if (length(f$cautions)) {
            tags$ul(class = "caution", lapply(f$cautions, tags$li))
          },
          if (length(f$notes)) {
            fold("Notes on the data", tags$ul(lapply(f$notes, tags$li)))
          },
          fold("How these results were computed", p(HTML(computed_text(f))))
        ),
        sub_section(
          "spread",
          sub_title("spread"),
          div(class = "chart", plotOutput("dist", height = "400px")),
          explained(spread_text(f))
        ),
        sub_section(
          "characteristics",
          sub_title("characteristics"),
          div(class = "chart", plotOutput("importance", height = "auto")),
          explained(list(
            short = paste0(
              "The forest split most often, and earliest, on ",
              and_list(sprintf(
                "%s (%s)",
                name_tag(r, f$top$variable),
                fmt(f$top$importance)
              )),
              "."
            ),
            long = paste0(
              "These are the leading ",
              term("moderator", "candidate moderators"),
              " among your chosen characteristics X ",
              "as per our exploration with the causal forest. ",
              term("importance", "Importance"),
              " shows which characteristics the estimated treatment effects depend on. Highly correlated characteristics can share ",
              "importance and should be handled before running the causal forest."
            )
          ))
        ),
        sub_section(
          "moderators",
          sub_title("moderators"),
          p(
            class = "caption",
            paste0(
              "How the treatment effect varies with each candidate moderator, in order of importance. ",
              "Choose a second characteristic to see the two together in a heatmap."
            )
          ),
          fold("How to read these plots", HTML(moderator_help(f))),
          lapply(seq_along(r$moderators$variable), function(k) {
            moderator_ui(r, k)
          })
        ),
        sub_section(
          "export",
          sub_title("export"),
          div(class = "report-box", id = "report-text", report_text(f)),
          div(
            class = "mt-3",
            downloadButton(
              "dl_cates",
              "Estimated treatment effect for each row (CSV)",
              class = "btn-outline-dark btn-sm"
            )
          ),
          p(class = "caption mt-2", export_note(f))
        )
      )
    ))
  }
  pending <- function(tag) tagAppendAttributes(tag, class = "pending")

  covariate_choices <- function(r, vars = r$settings$covariates) {
    stats::setNames(vars, unname(r$labels[vars]))
  }
  # Readable names from the supplied labels, or the column names.
  readable <- function(r, v) {
    shown <- v
    known <- intersect(v, names(r$settings$labels))
    shown[match(known, v)] <- unname(r$settings$labels[known])
    shown
  }
  # "39 characteristics", or "25 candidate moderators and 6 controls".
  x_counts <- function(r) {
    k <- length(r$settings$covariates)
    controls <- length(r$settings$controls)
    if (controls) {
      paste(
        count_of(k, "candidate moderator"),
        "and",
        count_of(controls, "control")
      )
    } else {
      count_of(k, "characteristic")
    }
  }
  computed_text <- function(f) {
    r <- f$report
    paste0(
      sprintf(
        "The %s grew %s %s %s from %s observations and %s. It estimates a treatment effect for every observation conditional on its covariates (called a %s).",
        term("causal_forest", "causal forest"),
        fmt_n(r$settings$num_trees),
        term("honest", "honest"),
        term("trees", "trees"),
        fmt_n(r$settings$n),
        x_counts(r),
        as.character(tags$a(
          href = cate_url,
          target = "_blank",
          rel = "noopener",
          "conditional average treatment effect"
        ))
      ),
      if (r$settings$weighted) {
        " Survey weights were used for the forest and all averages."
      } else {
        ""
      },
      if (r$settings$clustered) " Clusters were used for sampling." else "",
      if (length(r$settings$controls)) {
        paste0(
          " The controls (",
          and_list(readable(r, r$settings$controls)),
          ") adjust the comparison and are used by the forest like every other characteristic, but the results show only the candidate moderators."
        )
      } else {
        ""
      },
      " The histogram below shows these estimated treatment effects. Every average (overall, within groups and along a characteristic) ",
      "combines them with the observed values of Y, as a ",
      term("doubly_robust", "doubly robust"),
      " average, which keeps the averages accurate."
    )
  }
  # A plot's key finding, with the rest of its explanation in an expander.
  explained <- function(text) {
    tagList(
      p(class = "caption", HTML(text$short)),
      if (length(text$long)) fold("How to read this plot", p(HTML(text$long)))
    )
  }

  # The histogram: the range of estimated treatment effects and, in its
  # expander, what the colours mean here.
  spread_text <- function(f) {
    r <- f$report
    below <- sum(r$weights[r$estimates < 0]) / sum(r$weights)
    share <- if (below == 0) {
      "All are above 0."
    } else if (below == 1) {
      "All are below 0."
    } else {
      sprintf(
        "%s%% of %s %s an estimated treatment effect below 0.",
        formatC(100 * below, digits = 0, format = "f"),
        if (r$settings$weighted) "the weighted sample" else "observations",
        if (r$settings$weighted) "has" else "have"
      )
    }
    meaning <- if (f$continuous) {
      sprintf(
        "effects where %s rises with %s, and red bars, below 0, effects where it falls",
        f$y_tag,
        code_tag(f$w)
      )
    } else {
      sprintf(
        "effects where %s is higher for %s than for %s, and red bars, below 0, effects where it is lower",
        f$y_tag,
        f$g1_tag,
        f$g0_tag
      )
    }
    list(
      short = sprintf(
        "%s%s run from %s to %s, around the average treatment effect of %s. %s",
        term("cate", "Estimated treatment effects"),
        f$per,
        fmt(r$prediction_range[1]),
        fmt(r$prediction_range[2]),
        fmt(f$ate),
        share
      ),
      long = sprintf(
        paste(
          "Each estimate is a %s: the expected effect for people with similar characteristics X, not one person's",
          "own effect. Each bar shows how many observations have an estimated treatment effect in its range; the dashed line marks 0 and the solid line the average treatment effect. The colours",
          "deepen with distance from 0: teal bars, above 0, mark %s. Part of this spread can come from %s, especially at",
          "the extremes, which rest on few observations."
        ),
        term("cate", "conditional average treatment effect"),
        meaning,
        term("noise", "estimation noise")
      )
    )
  }

  # One block per candidate moderator: its plot, a second characteristic
  # that turns the plot into a heatmap, and the plot's key finding.
  moderator_ui <- function(r, k) {
    v <- r$moderators$variable[k]
    title <- unname(r$labels[v])
    div(
      class = "moderator",
      id = paste0("moderator-", k),
      tags$h4(
        span(class = "rank", k),
        if (identical(title, v)) title else paste0(title, " (", v, ")")
      ),
      div(
        class = "second",
        selectizeInput(
          paste0("second_", k),
          "Second characteristic (optional)",
          choices = c(
            "",
            covariate_choices(r, setdiff(r$moderators$variable, v))
          ),
          options = list(placeholder = "None")
        )
      ),
      div(class = "chart", plotOutput(paste0("effect_", k), height = "auto")),
      uiOutput(paste0("effect_text_", k))
    )
  }
  # The outputs behind those blocks, made for each new report.
  moderator_outputs <- function(r) {
    for (k in seq_along(r$moderators$variable)) {
      local({
        k <- k
        v <- r$moderators$variable[k]
        groups <- reactive({
          second <- input[[paste0("second_", k)]]
          effect_by(
            report(),
            v,
            x2 = if (
              length(second) && nzchar(second) && !identical(second, v)
            ) {
              second
            }
          )
        })
        output[[paste0("effect_", k)]] <- renderPlot(
          for_page(plot(groups())),
          res = 110,
          bg = "transparent",
          height = function() {
            if (length(attr(groups(), "plot")$covariates) == 2L) 440 else 340
          }
        )
        output[[paste0("effect_text_", k)]] <- renderUI(tagList(
          p(class = "caption", HTML(effect_short(groups()))),
          p(class = "footnote", average_note(report()$ate$estimate))
        ))
      })
    }
  }
  # The key finding of one moderator's plot: where the line, or the average
  # treatment effect across groups, is lowest and highest.
  effect_short <- function(t) {
    r <- report()
    f <- facts()
    info <- attr(t, "plot")
    if (!is.null(info$rows)) {
      at <- info$smooth_at
      if (is.null(at)) {
        return(
          "Each point is one row's estimated treatment effect; there are too few rows or values to draw a line."
        )
      }
      name <- if (is.null(r$settings$labels)) {
        code_tag(info$covariates[1])
      } else {
        info$titles[1]
      }
      # Category names for a characteristic with few values; otherwise whole
      # numbers when it is recorded in whole numbers.
      whole <- all(info$rows$value == round(info$rows$value))
      value <- function(x) {
        if (!is.null(info$breaks)) {
          info$break_labels[match(x, info$breaks)]
        } else {
          format(
            if (whole) round(x) else signif(x, 3),
            big.mark = ",",
            trim = TRUE
          )
        }
      }
      lo <- at[which.min(at$effect), ]
      hi <- at[which.max(at$effect), ]
      return(sprintf(
        "Following the line, the average treatment effect is lowest at %s = %s (%s) and highest at %s = %s (%s).",
        name,
        value(lo$value),
        fmt(lo$effect),
        name,
        value(hi$value),
        fmt(hi$effect)
      ))
    }
    t$label <- do.call(
      paste,
      c(
        lapply(seq_along(info$titles), function(k) {
          paste0(info$titles[k], " = ", info$levels[[k]])
        }),
        sep = ", "
      )
    )
    # Describe the groups the plot shows: estimated, without missing values.
    est <- t[is.finite(t$estimate) & !info$missing, ]
    if (nrow(est) < 2) {
      return(paste0(
        "Too few groups could be estimated to compare (each needs at least 10 observations, ",
        if (f$continuous) "in which W varies" else "including both groups of W",
        ")."
      ))
    }
    lo <- est[which.min(est$estimate), ]
    hi <- est[which.max(est$estimate), ]
    desc <- function(row) {
      sprintf(
        "%s (%s; n = %s)",
        if (is.null(r$settings$labels)) code_tag(row$label) else row$label,
        fmt(row$estimate),
        fmt_n(row$n)
      )
    }
    paste0(
      "The average treatment effect is lowest for ",
      desc(lo),
      " and highest for ",
      desc(hi),
      "."
    )
  }
  average_note <- function(x) {
    if (is.finite(x)) {
      paste0("The average treatment effect is ", fmt(x), ".")
    } else {
      "The average treatment effect is unavailable."
    }
  }
  # How to read the moderator plots, once for all of them.
  moderator_help <- function(f) {
    r <- f$report
    paste0(
      "<p>For a numeric characteristic, each point is one person's ",
      term("cate", "estimated treatment effect"),
      " (spread slightly sideways where values repeat), and the line shows the average treatment effect along the characteristic: a ",
      term("loess", "loess"),
      " smooth of the same ",
      term("doubly_robust", "doubly robust"),
      " scores behind every average here",
      if (r$settings$weighted) ", using the supplied weights",
      ". For a characteristic with eight values or fewer, the line is smoothed more widely, so it shows the trend across its ",
      "categories rather than each category's own average. The points are pulled toward the overall average, so the line can rise ",
      "and fall more than they do; it is least certain where few people have a value.</p>",
      "<p>For a text characteristic, whose categories have no order, each point is the average treatment effect in one group, ",
      "with its number of people (n). Groups with fewer than 10 people, ",
      if (f$continuous) {
        "or in which W does not vary"
      } else {
        "or with only one group of W"
      },
      ", are not estimated.</p>",
      "<p>With a second characteristic, each cell of the heatmap is the average treatment effect in one combination of groups. ",
      "Numeric characteristics with more than eight values are cut into three ",
      term("quantile_bins", "groups of about equal size"),
      ", and the colours are centred on the overall average treatment effect (",
      fmt(f$ate),
      ").</p>",
      "<p>The dashed line marks a treatment effect of 0, and missing values are left out. People in different groups also differ ",
      "in other ways, so the plots describe ",
      term("association", "associations"),
      ", not what would happen if a characteristic changed.</p>"
    )
  }
  # What the downloaded file holds.
  export_note <- function(f) {
    r <- f$report
    reasons <- c(
      "Y",
      "W",
      if (r$settings$weighted) "the weight",
      if (r$settings$clustered) "the cluster"
    )
    paste0(
      "The file has one row per row of your data, in the same order, with the row number",
      if (!is.null(rv$res$ids)) paste0(", your ID column (", rv$res$ids, ")"),
      " and the estimated treatment effect",
      if (r$n_dropped) {
        sprintf(
          "; it is empty for the %s not analysed because %s was missing.",
          if (r$n_dropped == 1) "row" else paste(fmt_n(r$n_dropped), "rows"),
          or_list(reasons)
        )
      } else {
        "."
      }
    )
  }

  report_text <- function(f) {
    r <- f$report
    item <- function(label, ...) {
      tags$li(
        tags$b(paste0(label, if (grepl("[?]$", label)) " " else ": ")),
        ...
      )
    }
    sample <- sprintf(
      "using %s and %s (X).",
      count_of(r$settings$n, "observation"),
      x_counts(r)
    )
    analysed <- if (f$continuous) {
      sprintf(
        "The treatment effect of %s (W) on %s (Y), per one-unit increase, %s",
        f$w,
        f$y_text,
        sample
      )
    } else {
      sprintf(
        "The treatment effect of %s versus %s (W) on %s (Y), %s",
        f$g1,
        f$g0,
        f$y_text,
        sample
      )
    }
    effect <- if (f$continuous) {
      sprintf("per one-unit increase in %s", f$w)
    } else {
      sprintf("for %s than for %s", f$g1, f$g0)
    }
    in_sd <- if (is.finite(f$std)) {
      sprintf(
        " (%s standard deviations%s)",
        fmt(abs(f$std)),
        if (f$continuous) paste(" per standard deviation of", f$w) else ""
      )
    } else {
      ""
    }
    average <- if (!is.finite(f$ate)) {
      "The average treatment effect could not be estimated."
    } else {
      sprintf(
        "Comparing people with similar characteristics, %s was on average %s%s %s %s%s%s.",
        f$y_text,
        fmt(abs(f$ate)),
        if (f$y_binary) {
          sprintf(" (%s percentage points)", fmt(100 * abs(f$ate), 1))
        } else {
          ""
        },
        more_less(f$ate, "higher", "lower"),
        effect,
        in_sd,
        if (f$continuous) "; this is the average partial effect" else ""
      )
    }
    tags$ul(
      item("What was analysed", analysed),
      item("Average treatment effect", average),
      item(
        "How much does it vary?",
        sprintf(
          "Estimated treatment effects%s ranged from %s to %s; part of this spread could be estimation noise.",
          f$per_text,
          fmt(r$prediction_range[1]),
          fmt(r$prediction_range[2])
        )
      ),
      item(
        "Candidate moderators",
        sprintf("The forest split most often on %s.", and_list(f$top$label))
      ),
      item(
        "Keep in mind",
        paste0(
          "These patterns are exploratory: use them to decide what to study next. Each estimated treatment effect is an average for people with similar characteristics, not one person's own effect. ",
          sprintf(
            "Unless %s was randomly assigned, the effects are adjusted associations and may reflect other causes.",
            f$w
          ),
          if (r$settings$weighted) " Averages use the supplied weights." else ""
        )
      )
    )
  }

  # ---- Outputs ---------------------------------------------------------------

  output$dist <- renderPlot(
    for_page(plot(report())),
    res = 110,
    bg = "transparent"
  )
  output$importance <- renderPlot(
    for_page(importance_plot()),
    res = 110,
    bg = "transparent",
    height = function() importance_height()
  )
  # The exported table keeps the ID column's own name; rows not analysed are
  # empty.
  export_table <- function() {
    out <- report()$cates
    if (!is.null(rv$res$ids)) {
      names(out)[names(out) == "id"] <- make.unique(c(
        "row",
        "estimated_treatment_effect",
        rv$res$ids
      ))[3]
    }
    out
  }
  output$dl_cates <- downloadHandler(
    function() "treatment_effects.csv",
    function(file) {
      utils::write.csv(export_table(), file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui, server)
