# Builds the GSS example that ships with the package:
#   inst/extdata/gss_demo.csv           one row per respondent
#   inst/extdata/gss_demo_codebook.csv  one row per column: label, role,
#                                       coding, value labels, source, notes
# Run from the repository root after a new GSS release (needs gssr and an
# internet connection): Rscript data-raw/gss_demo.R
#
# gssr::gss_get_yr(2024) downloads NORC's 2024 cross-section (Release 2, with
# the AmeriSpeak oversample). Every recode is defined by the meaning of the
# GSS category labels, not by assumed codes, and the labels are checked: if a
# release changes a category, the script stops and names the item. Codes
# labelled as missing (don't know, no answer, skipped and so on) become NA.
# Missing characteristics stay missing (the forest handles them); only
# respondents missing the rating or political views are left out.

# ---- Each column's source, short label, recode and note ----------------------

# scale(): ordered categories, one pattern per new code, in order; categories
# matching `missing` become NA. flag(): 1 for categories matching `yes`, 0 for
# `no` (default: every other category); a plain yes/no flag gets no category
# names, so it shows as 0/1, as in the data, while flags with meaningful names
# (such as Liberal/moderate and Conservative) keep them. number(): numeric
# values, optionally within limits. ordinal(): labelled codes kept as they are.
# category(): unordered categories kept as text. Patterns are case-insensitive.
gss_scale <- function(levels, start = 1, missing = NULL) {
  list(type = "scale", levels = levels, start = start, missing = missing)
}
gss_flag <- function(yes = NULL, no = NULL, labels = c("No", "Yes")) {
  list(type = "flag", yes = yes, no = no, labels = labels)
}
gss_number <- function(min = -Inf, max = Inf) {
  list(type = "number", min = min, max = max)
}
gss_ordinal <- function() list(type = "ordinal")
gss_category <- function() list(type = "category")

# Columns in the order of the file. Roles: the tutorial's dependent and
# independent variables, its characteristics (X), and further dependent and
# independent variables for other analyses. `same_as` names columns that
# measure the same thing, which "Select all" in the app leaves out when this
# column is Y or W.
gss2024_items <- function() {
  yes_no <- gss_flag("^yes$", "^no$")
  degree <- gss_scale(
    c(
      "less than high school",
      "^high school$",
      "junior college",
      "bachelor",
      "graduate"
    ),
    start = 0
  )
  mention <- "A race mention: respondents could name more than one race, so a respondent can be 1 here and in another race column."
  thermometer <- "Feeling thermometer from 0 (least favourable) to 100 (most favourable), from the GSS-ANES module."
  item <- function(
    source,
    label,
    rule,
    note = "",
    role = "characteristic",
    same_as = ""
  ) {
    list(
      source = source,
      label = label,
      rule = rule,
      note = note,
      role = role,
      same_as = same_as
    )
  }
  list(
    rating_trump = item(
      "ratechall124",
      "Rating of Donald Trump",
      gss_number(0, 100),
      thermometer,
      role = "dependent"
    ),
    conservative = item(
      "polviews",
      "Conservative",
      gss_flag(
        "conservative",
        "liberal|moderate",
        c("Liberal/moderate", "Conservative")
      ),
      "From the seven-point liberal-conservative scale; moderates are grouped with liberals.",
      role = "independent"
    ),
    # Demographics
    female = item("sex", "Female", gss_flag("^female$", "^male$")),
    age = item("age", "Age", gss_number(), "89 = 89 or older."),
    white = item("raceacs1", "White", yes_no, mention),
    black = item("raceacs2", "Black", yes_no, mention),
    hispanic = item(
      "hispanic",
      "Hispanic",
      gss_flag(no = "^not hispanic"),
      "1 = any Hispanic, Latino or Spanish origin."
    ),
    born = item("born", "Born in the US", yes_no),
    othlang = item(
      "othlang",
      "Speaks another language",
      yes_no,
      "Another language than the one of the interview (English or Spanish)."
    ),
    lgb = item(
      "sexornt",
      "Lesbian, gay or bisexual",
      gss_flag("gay|lesbian|homosexual|bisexual", "heterosexual|straight")
    ),
    married = item(
      "marital",
      "Married",
      gss_flag("^married$"),
      "0 = widowed, divorced, separated or never married."
    ),
    adults = item(
      "adults",
      "Adults in household",
      gss_number(),
      "From the household roster; 8 = 8 or more."
    ),
    childs = item(
      "childs",
      "Number of children",
      gss_number(),
      "Children ever born alive; 8 = 8 or more."
    ),
    sibs = item(
      "sibs",
      "Number of siblings",
      gss_number(),
      "Including step- and adopted siblings; 6 = 6 or more."
    ),
    vetyears = item(
      "vetyears",
      "Time on active military duty",
      gss_scale(
        c(
          "no active duty",
          "less than 2 years",
          "2-4 years",
          "more than 4 years"
        ),
        start = 0
      ),
      "Respondents who served but did not know for how long are missing."
    ),
    # Education, income and work
    educ = item(
      "educ",
      "Years of education",
      gss_number(),
      "Highest year of school completed; 20 = 8 or more years of college.",
      same_as = "degree;college"
    ),
    degree = item(
      "degree",
      "Highest degree",
      degree,
      "Highest degree completed.",
      same_as = "educ;college"
    ),
    madeg = item(
      "madeg",
      "Mother's education",
      degree,
      "Highest degree of the mother (or youngest same-sex parent)."
    ),
    padeg = item(
      "padeg",
      "Father's education",
      degree,
      "Highest degree of the father (or oldest same-sex parent)."
    ),
    income16 = item(
      "income16",
      "Family income",
      gss_ordinal(),
      "Total family income last year, in 26 ordered dollar ranges."
    ),
    finrela = item(
      "finrela",
      "Relative family income",
      gss_scale(c(
        "^far below average$",
        "^below average$",
        "^average$",
        "^above average$",
        "^far above average$"
      )),
      "Family income compared with American families in general."
    ),
    class = item(
      "class",
      "Social class",
      gss_scale(
        c("lower class", "working class", "middle class", "upper class"),
        missing = "no class"
      ),
      "Subjective social class; the option 'no class' (chosen by nobody in 2024) is missing."
    ),
    prestg10 = item(
      "prestg10",
      "Occupational prestige",
      gss_number(),
      "Prestige score of the respondent's occupation (2010 scores)."
    ),
    employed = item(
      "wrkstat",
      "Employed",
      gss_flag(
        "working full time|working part time|with a job, but not at work",
        "unemployed|retired|in school|keeping house|^other$"
      ),
      "From labour force status: 1 = working full or part time, or with a job but temporarily not at work; 0 = unemployed, retired, in school, keeping house or other."
    ),
    govt_employee = item(
      "wrkgovt1",
      "Government employee",
      yes_no,
      "Employed by federal, state or local government in the current or last job."
    ),
    compuse = item("compuse", "Uses a computer", yes_no),
    # Place
    region = item(
      "region",
      "Region",
      gss_category(),
      "Region of interview: the four U.S. Census regions."
    ),
    rural = item(
      "res16",
      "Rural/small-town upbringing",
      gss_flag("open country|farm|small city or town"),
      "Place lived at 16: 1 = open country, farm, or a town under 50,000."
    ),
    mobile16 = item(
      "mobile16",
      "Moved since age 16",
      gss_scale(c("same city", "different city", "different state")),
      "Where the respondent lived at 16, compared with now."
    ),
    raclive = item(
      "raclive",
      "Neighbours of the other race (Black/white)",
      yes_no,
      "Black respondents were asked about white neighbours; all other respondents about Black neighbours."
    ),
    # Religion
    attend = item(
      "attend",
      "Religious attendance",
      gss_scale(
        c(
          "^never$",
          "less than once a year",
          "once or twice a year",
          "several times a year",
          "once a month",
          "2-3 times a month",
          "nearly every week",
          "^every week$",
          "several times a week"
        ),
        start = 0
      )
    ),
    pray = item(
      "pray",
      "Frequency of prayer",
      gss_scale(
        c(
          "^never$",
          "less than once a week",
          "^once a week$",
          "several times a week",
          "once a day",
          "several times a day"
        ),
        start = 0
      )
    ),
    reborn = item(
      "reborn",
      "Born again",
      yes_no,
      "Has had a 'born again' experience."
    ),
    no_religion = item(
      "relig",
      "No religion",
      gss_flag("^none$", labels = c("Any religion", "No religion")),
      "From religious preference: 1 = none; 0 = any religion."
    ),
    # Politics and wellbeing
    lkelyvot = item(
      "lkelyvot",
      "Likelihood of voting",
      gss_scale(c(
        "not at all likely",
        "not very likely",
        "somewhat likely",
        "^very likely$",
        "extremely likely"
      )),
      "Likelihood of voting in the 2024 election. Not asked of respondents who were not U.S. citizens (or becoming citizens), so they are missing."
    ),
    satdemoc = item(
      "satdemoc",
      "Satisfaction with democracy",
      gss_scale(c(
        "not at all satisfied",
        "not very satisfied",
        "fairly satisfied",
        "^very satisfied$"
      )),
      "Satisfaction with the way democracy works in America."
    ),
    happy = item(
      "happy",
      "Happiness",
      gss_scale(c("not too happy", "pretty happy", "^very happy$"))
    ),
    health = item(
      "health",
      "Self-rated health",
      gss_scale(c("^poor$", "^fair$", "^good$", "^excellent$"))
    ),
    finalter = item(
      "finalter",
      "Change in finances",
      gss_scale(c("worse", "same", "better")),
      "Over the last few years."
    ),
    satfin = item(
      "satfin",
      "Financial satisfaction",
      gss_scale(c("not satisfied at all", "more or less", "pretty well"))
    ),
    diagnosd = item(
      "diagnosd",
      "Mental health diagnosis",
      yes_no,
      "Ever diagnosed with a mental health problem."
    ),
    # Further dependent and independent variables for other analyses
    rating_harris = item(
      "ratechall224",
      "Rating of Kamala Harris",
      gss_number(0, 100),
      thermometer,
      role = "other dependent"
    ),
    rating_biden = item(
      "rateincumb24",
      "Rating of Joe Biden",
      gss_number(0, 100),
      thermometer,
      role = "other dependent"
    ),
    rating_democrats = item(
      "ratedemp",
      "Rating of the Democratic Party",
      gss_number(0, 100),
      thermometer,
      role = "other dependent"
    ),
    rating_republicans = item(
      "raterepp",
      "Rating of the Republican Party",
      gss_number(0, 100),
      thermometer,
      role = "other dependent"
    ),
    rating_maga = item(
      "ratemaga",
      "Rating of MAGA Republicans",
      gss_number(0, 100),
      thermometer,
      role = "other dependent"
    ),
    economy = item(
      "econstat",
      "Rating of the economy",
      gss_scale(c(
        "very bad",
        "^bad$",
        "neither bad nor good",
        "^good$",
        "very good"
      )),
      "The state of the economy these days in the United States.",
      role = "other dependent"
    ),
    immigration = item(
      "letin1a",
      "Preferred immigration level",
      gss_scale(c(
        "reduced a lot",
        "reduced a little",
        "remain the same",
        "increased a little",
        "increased a lot"
      )),
      "The number of immigrants to America nowadays should be...",
      role = "other dependent"
    ),
    college = item(
      "degree",
      "College degree",
      gss_flag(
        "bachelor|graduate",
        "less than high school|^high school$|junior college"
      ),
      "From highest degree: 1 = bachelor's or graduate degree. As W, leave degree and educ out of X: they determine it.",
      role = "other independent",
      same_as = "degree;educ"
    )
  )
}

# ---- Recoding by label meaning -----------------------------------------------

# Numbers without haven classes; negative codes and codes labelled as missing
# become NA.
plain_values <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  v <- unclass(x)
  attributes(v) <- NULL
  v <- suppressWarnings(as.numeric(v))
  missing_codes <- labels[
    !is.na(labels) &
      grepl(gss_missing_pattern, names(labels), ignore.case = TRUE)
  ]
  v[!is.finite(v) | v < 0 | v %in% missing_codes] <- NA_real_
  v
}
gss_missing_pattern <- paste(
  c(
    "don'?t know",
    "\\bdk\\b",
    "no answer",
    "\\bna\\b",
    "\\biap\\b",
    "inapplicable",
    "refused",
    "skipped",
    "not available",
    "uncodeable",
    "not imputable",
    "see codebook",
    "can'?t choose",
    "cannot choose"
  ),
  collapse = "|"
)

# Substantive categories: labelled codes that are not missing-value codes.
gss_categories <- function(x, item) {
  labels <- attr(x, "labels", exact = TRUE)
  labels <- labels[
    !is.na(labels) &
      labels >= 0 &
      !grepl(gss_missing_pattern, names(labels), ignore.case = TRUE)
  ]
  if (!length(labels)) {
    msg <- sprintf(
      "GSS item %s has no category labels; its coding may have changed.",
      item
    )
    stop(msg, call. = FALSE)
  }
  labels
}

recode_gss <- function(x, rule, item) {
  v <- plain_values(x)
  changed <- function(what) {
    msg <- sprintf(
      "GSS item %s: %s. The GSS coding may have changed; update gss2024_items() in data-raw/gss_demo.R.",
      item,
      what
    )
    stop(msg, call. = FALSE)
  }
  matching <- function(categories, pattern) {
    categories[grepl(pattern, names(categories), ignore.case = TRUE)]
  }
  cap <- function(s) paste0(toupper(substring(s, 1, 1)), substring(s, 2))
  switch(
    rule$type,
    number = {
      v[v < rule$min | v > rule$max] <- NA_real_
      list(values = v, labels = NULL)
    },
    ordinal = {
      categories <- gss_categories(x, item)
      v[!v %in% categories] <- NA_real_
      list(
        values = v,
        labels = stats::setNames(cap(names(categories)), categories)
      )
    },
    scale = {
      categories <- gss_categories(x, item)
      if (!is.null(rule$missing)) {
        dropped <- matching(categories, rule$missing)
        if (!length(dropped)) {
          changed(sprintf("no category matching '%s'", rule$missing))
        }
        v[v %in% dropped] <- NA_real_
        categories <- categories[!categories %in% dropped]
      }
      found <- lapply(rule$levels, function(p) matching(categories, p))
      counts <- lengths(found)
      if (any(counts != 1L)) {
        changed(sprintf(
          "expected one category matching '%s', found %d",
          rule$levels[counts != 1L][1],
          counts[counts != 1L][1]
        ))
      }
      unmatched <- setdiff(categories, unlist(found))
      if (length(unmatched)) {
        changed(paste0(
          "unexpected category '",
          names(categories)[categories %in% unmatched][1],
          "'"
        ))
      }
      codes <- rule$start + seq_along(found) - 1
      out <- rep(NA_real_, length(v))
      for (k in seq_along(found)) {
        out[which(v == found[[k]])] <- codes[k]
      }
      list(
        values = out,
        labels = stats::setNames(cap(vapply(found, names, "")), codes)
      )
    },
    category = {
      categories <- gss_categories(x, item)
      list(
        values = cap(names(categories))[match(v, categories)],
        labels = NULL,
        levels = cap(names(categories))
      )
    },
    flag = {
      categories <- gss_categories(x, item)
      yes <- if (is.null(rule$yes)) {
        categories[!categories %in% matching(categories, rule$no)]
      } else {
        matching(categories, rule$yes)
      }
      no <- if (is.null(rule$no)) {
        categories[!categories %in% yes]
      } else {
        matching(categories, rule$no)
      }
      if (!length(yes) || !length(no) || any(yes %in% no)) {
        changed("the categories for 1 and 0 could not be told apart")
      }
      unmatched <- setdiff(categories, c(yes, no))
      if (length(unmatched)) {
        changed(paste0(
          "unexpected category '",
          names(categories)[categories %in% unmatched][1],
          "'"
        ))
      }
      out <- ifelse(v %in% yes, 1, ifelse(v %in% no, 0, NA_real_))
      named <- !identical(rule$labels, c("No", "Yes"))
      list(
        values = out,
        labels = if (named) c(`0` = rule$labels[1], `1` = rule$labels[2]),
        yes = names(yes),
        no = names(no)
      )
    }
  )
}

# A one-line description of each column's coding, written from the data.
gss_coding <- function(values, recoded) {
  if (!is.null(recoded$yes)) {
    return(sprintf(
      "1 = %s; 0 = %s",
      paste(recoded$yes, collapse = " / "),
      paste(recoded$no, collapse = " / ")
    ))
  }
  if (!is.null(recoded$levels)) {
    return(paste("Text:", paste(recoded$levels, collapse = ", ")))
  }
  labels <- recoded$labels
  if (is.null(labels)) {
    range <- range(values, na.rm = TRUE)
    return(sprintf("Numeric, %s to %s", format(range[1]), format(range[2])))
  }
  if (length(labels) > 10) {
    return(sprintf(
      "%d ordered categories: %s = %s to %s = %s",
      length(labels),
      names(labels)[1],
      labels[1],
      names(labels)[length(labels)],
      labels[length(labels)]
    ))
  }
  paste(sprintf("%s = %s", names(labels), labels), collapse = ", ")
}

# ---- The example's data and codebook -----------------------------------------

build_gss_demo <- function(raw) {
  raw <- as.data.frame(raw)
  items <- gss2024_items()
  needed <- unique(c(
    "id",
    vapply(items, `[[`, "", "source"),
    "wtssnrps_as",
    "vstrat",
    "vpsu"
  ))
  absent <- setdiff(needed, names(raw))
  if (length(absent)) {
    stop(
      "The GSS file lacks: ",
      paste(absent, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  recoded <- lapply(items, function(it) {
    recode_gss(raw[[it$source]], it$rule, it$source)
  })
  d <- data.frame(
    id = as.integer(plain_values(raw$id)),
    lapply(recoded, `[[`, "values"),
    check.names = FALSE
  )
  d$svy_weight <- plain_values(raw$wtssnrps_as)
  d$svy_weight[!is.finite(d$svy_weight) | d$svy_weight < 0] <- NA_real_
  stratum <- plain_values(raw$vstrat)
  psu <- plain_values(raw$vpsu)
  d$psu_cluster <- ifelse(
    is.na(stratum) | is.na(psu),
    NA_integer_,
    as.integer(factor(paste(stratum, psu)))
  )
  d <- d[!is.na(d$rating_trump) & !is.na(d$conservative), , drop = FALSE]
  d <- d[order(d$id), , drop = FALSE]
  rownames(d) <- NULL
  stopifnot(!anyDuplicated(d$id))

  gss_label <- function(source) {
    l <- attr(raw[[source]], "label", exact = TRUE)
    if (is.null(l)) "" else as.character(l)
  }
  values <- function(labels) {
    if (is.null(labels)) {
      ""
    } else {
      paste(sprintf("%s=%s", names(labels), labels), collapse = "|")
    }
  }
  item_rows <- do.call(
    rbind,
    lapply(names(items), function(v) {
      it <- items[[v]]
      data.frame(
        variable = v,
        label = it$label,
        role = it$role,
        coding = gss_coding(d[[v]], recoded[[v]]),
        values = values(recoded[[v]]$labels),
        source = it$source,
        gss_label = gss_label(it$source),
        note = it$note,
        same_as = it$same_as
      )
    })
  )
  codebook <- rbind(
    data.frame(
      variable = "id",
      label = "Respondent ID",
      role = "id",
      coding = "GSS respondent identifier",
      values = "",
      source = "id",
      gss_label = gss_label("id"),
      note = "Unique within the 2024 cross-section.",
      same_as = ""
    ),
    item_rows,
    data.frame(
      variable = c("svy_weight", "psu_cluster"),
      label = c("Survey weight", "Sampling cluster"),
      role = c("weight", "cluster"),
      coding = c(
        "Post-stratification and nonresponse weight for the full sample; optional",
        "Sampling stratum x primary sampling unit; optional"
      ),
      values = "",
      source = c("wtssnrps_as", "vstrat x vpsu"),
      gss_label = c(gss_label("wtssnrps_as"), ""),
      note = c(
        paste(
          "NORC's recommended weight (WTSSNRPS), extended to the AmeriSpeak oversample of Black, Hispanic, and",
          "Asian American, Native Hawaiian and Pacific Islander respondents; use it to describe U.S. adults."
        ),
        "Variance stratum and primary sampling unit, for clustered sampling."
      ),
      same_as = ""
    )
  )
  stopifnot(identical(codebook$variable, names(d)))
  list(data = d, codebook = codebook)
}

main <- function() {
  stopifnot(file.exists("DESCRIPTION"))
  if (!requireNamespace("gssr", quietly = TRUE)) {
    stop(
      "Install gssr first: install.packages(\"gssr\", repos = c(\"https://kjhealy.r-universe.dev\", \"https://cloud.r-project.org\"))"
    )
  }
  demo <- build_gss_demo(gssr::gss_get_yr(2024))
  dir.create(
    file.path("inst", "extdata"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  utils::write.csv(
    demo$data,
    file.path("inst", "extdata", "gss_demo.csv"),
    row.names = FALSE,
    na = ""
  )
  utils::write.csv(
    demo$codebook,
    file.path("inst", "extdata", "gss_demo_codebook.csv"),
    row.names = FALSE,
    na = ""
  )
  cat(sprintf(
    "Wrote inst/extdata/gss_demo.csv (%d respondents, %d columns) and its codebook.\n",
    nrow(demo$data),
    ncol(demo$data)
  ))
}
if (sys.nframe() == 0L) {
  main()
}
