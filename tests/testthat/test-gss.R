# A small file shaped like gssr::gss_get_yr(2024): numeric codes with labels.
fake_gss <- function(n = 120, seed = 1) {
  set.seed(seed)
  lab <- function(values, labels) structure(values, labels = labels)
  pick <- function(codes) sample(codes, n, TRUE)
  yes_no <- c(yes = 1, no = 2)
  degree <- c(
    "less than high school" = 0,
    "high school" = 1,
    "associate/junior college" = 2,
    "bachelor's" = 3,
    graduate = 4
  )
  data.frame(
    id = seq_len(n),
    ratechall124 = lab(pick(c(0:100, NA)), c(dk = NA)),
    polviews = lab(
      pick(c(1:7, NA)),
      c(
        "extremely liberal" = 1,
        liberal = 2,
        "slightly liberal" = 3,
        "moderate, middle of the road" = 4,
        "slightly conservative" = 5,
        conservative = 6,
        "extremely conservative" = 7
      )
    ),
    wtssnrps_as = runif(n, .2, 3),
    vstrat = pick(1:5),
    vpsu = pick(1:2),
    sex = lab(pick(1:2), c(male = 1, female = 2)),
    raceacs1 = lab(pick(0:1), c(no = 0, yes = 1)),
    raceacs2 = lab(pick(0:1), c(no = 0, yes = 1)),
    hispanic = lab(
      pick(1:3),
      c("not hispanic" = 1, "puerto rican" = 2, cuban = 3)
    ),
    age = lab(pick(c(18:89, 98)), c("89 or older" = 89, "don't know" = 98)),
    sexornt = lab(
      pick(1:3),
      c(
        "gay, lesbian, or homosexual" = 1,
        bisexual = 2,
        "heterosexual or straight" = 3
      )
    ),
    educ = lab(pick(0:20), c("no formal schooling" = 0)),
    income16 = lab(
      pick(1:4),
      c(
        "under $1,000" = 1,
        "$1,000 to $2,999" = 2,
        "$3,000 to $3,999" = 3,
        "$4,000 or over" = 4
      )
    ),
    madeg = lab(pick(0:4), degree),
    padeg = lab(pick(0:4), degree),
    prestg10 = lab(pick(16:80), c(dk = NA)),
    marital = lab(
      pick(1:5),
      c(
        married = 1,
        widowed = 2,
        divorced = 3,
        separated = 4,
        "never married" = 5
      )
    ),
    adults = lab(pick(1:4), c("1 adult in household" = 1)),
    childs = lab(pick(0:8), c("8 or more" = 8)),
    res16 = lab(
      pick(1:6),
      c(
        "in open country but not on a farm" = 1,
        farm = 2,
        "in a small city or town (under 50,000)" = 3,
        "in a medium-size city (50,000-250,000)" = 4,
        "in a suburb near a large city" = 5,
        "in a large city (over 250,000)" = 6
      )
    ),
    region = lab(pick(1:4), c(northeast = 1, midwest = 2, south = 3, west = 4)),
    mobile16 = lab(
      pick(1:3),
      c(
        "same state, same city" = 1,
        "same state, different city" = 2,
        "different state" = 3
      )
    ),
    born = lab(pick(1:2), yes_no),
    attend = lab(
      pick(0:8),
      c(
        never = 0,
        "less than once a year" = 1,
        "about once or twice a year" = 2,
        "several times a year" = 3,
        "about once a month" = 4,
        "2-3 times a month" = 5,
        "nearly every week" = 6,
        "every week" = 7,
        "several times a week" = 8
      )
    ),
    pray = lab(
      pick(1:6),
      c(
        "several times a day" = 1,
        "once a day" = 2,
        "several times a week" = 3,
        "once a week" = 4,
        "less than once a week" = 5,
        never = 6
      )
    ),
    lkelyvot = lab(
      pick(1:5),
      c(
        "extremely likely" = 1,
        "very likely" = 2,
        "somewhat likely" = 3,
        "not very likely" = 4,
        "not at all likely" = 5
      )
    ),
    compuse = lab(pick(1:2), yes_no),
    othlang = lab(pick(1:2), yes_no),
    diagnosd = lab(pick(1:2), yes_no),
    raclive = lab(pick(1:2), yes_no),
    vetyears = lab(
      pick(0:4),
      c(
        "no active duty" = 0,
        "yes, less than 2 years" = 1,
        "yes, 2-4 years" = 2,
        "yes, more than 4 years" = 3,
        "some, don't know how long" = 4
      )
    ),
    happy = lab(
      pick(1:3),
      c("very happy" = 1, "pretty happy" = 2, "not too happy" = 3)
    ),
    health = lab(pick(1:4), c(excellent = 1, good = 2, fair = 3, poor = 4)),
    finalter = lab(pick(1:3), c(better = 1, worse = 2, "stayed same" = 3)),
    satfin = lab(
      pick(1:3),
      c(
        "pretty well satisfied" = 1,
        "more or less satisfied" = 2,
        "not satisfied at all" = 3
      )
    ),
    satdemoc = lab(
      pick(1:4),
      c(
        "very satisfied" = 1,
        "fairly satisfied" = 2,
        "not very satisfied" = 3,
        "not at all satisfied" = 4
      )
    ),
    degree = lab(pick(0:4), degree),
    class = lab(
      pick(1:4),
      c(
        "lower class" = 1,
        "working class" = 2,
        "middle class" = 3,
        "upper class" = 4,
        "no class" = 5
      )
    ),
    finrela = lab(
      pick(1:5),
      c(
        "far below average" = 1,
        "below average" = 2,
        average = 3,
        "above average" = 4,
        "far above average" = 5
      )
    ),
    wrkstat = lab(
      pick(1:8),
      c(
        "working full time" = 1,
        "working part time" = 2,
        "with a job, but not at work because of temporary illness, vacation, strike" = 3,
        "unemployed, laid off, looking for work" = 4,
        retired = 5,
        "in school" = 6,
        "keeping house" = 7,
        other = 8
      )
    ),
    wrkgovt1 = lab(pick(1:2), yes_no),
    reborn = lab(pick(1:2), yes_no),
    relig = lab(
      pick(1:5),
      c(protestant = 1, catholic = 2, jewish = 3, none = 4, other = 5)
    ),
    sibs = lab(pick(0:6), c("6 or more" = 6)),
    econstat = lab(
      pick(1:5),
      c(
        "very good" = 1,
        good = 2,
        "neither bad nor good" = 3,
        bad = 4,
        "very bad" = 5
      )
    ),
    letin1a = lab(
      pick(1:5),
      c(
        "increased a lot" = 1,
        "increased a little" = 2,
        "remain the same as it is" = 3,
        "reduced a little" = 4,
        "reduced a lot" = 5
      )
    ),
    ratechall224 = lab(pick(0:100), c(dk = NA)),
    rateincumb24 = lab(pick(0:100), c(dk = NA)),
    ratedemp = lab(pick(0:100), c(dk = NA)),
    raterepp = lab(pick(0:100), c(dk = NA)),
    ratemaga = lab(pick(0:100), c(dk = NA))
  )
}

# The build script lives in the source repository only (data-raw/ is not part
# of the package), so its tests are skipped where it is absent.
builder <- function() {
  path <- testthat::test_path("..", "..", "data-raw", "gss_demo.R")
  skip_if_not(
    file.exists(path),
    "The GSS build script is only in the source repository."
  )
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)
  env
}

test_that("the shipped example has the documented structure and agrees with its codebook", {
  gss <- load_gss2024()
  d <- gss$data
  expect_equal(nrow(d), 3721L)
  roles <- stats::setNames(gss$codebook$role, gss$codebook$variable)
  others <- names(roles)[roles %in% c("other dependent", "other independent")]
  expect_identical(
    names(d),
    c(
      "id",
      "rating_trump",
      "conservative",
      gss$covariates,
      others,
      "svy_weight",
      "psu_cluster"
    )
  )
  expect_length(gss$covariates, 39L)
  expect_setequal(
    others,
    c(
      "rating_harris",
      "rating_biden",
      "rating_democrats",
      "rating_republicans",
      "rating_maga",
      "economy",
      "immigration",
      "college"
    )
  )
  # Region is text (its categories have no order); every other column is
  # numeric.
  expect_setequal(unique(d$region), c("Northeast", "Midwest", "South", "West"))
  expect_true(all(vapply(
    d[setdiff(names(d), "region")],
    is.numeric,
    logical(1)
  )))
  for (v in grep("^rating_", names(d), value = TRUE)) {
    expect_true(all(d[[v]] >= 0 & d[[v]] <= 100, na.rm = TRUE), info = v)
  }
  # Derived columns agree with their sources.
  expect_identical(
    d$college[!is.na(d$degree)],
    as.integer(d$degree[!is.na(d$degree)] >= 3)
  )
  expect_identical(
    gss$codebook$same_as[gss$codebook$variable == "college"],
    "degree;educ"
  )
  expect_false(anyNA(d[c("id", "rating_trump", "conservative")]))
  expect_false(anyDuplicated(d$id) > 0)
  expect_true(all(d$rating_trump >= 0 & d$rating_trump <= 100))
  expect_setequal(unique(d$conservative), c(0, 1))
  expect_true(all(d$svy_weight > 0))
  expect_identical(gss$codebook$variable, names(d))
  expect_identical(names(gss$labels), names(d))
  expect_true(all(nzchar(gss$labels)))
  expect_identical(
    gss$codebook$role[
      gss$codebook$variable %in% c("rating_trump", "conservative")
    ],
    c("dependent", "independent")
  )
  # Every coded value has a category name, and no category name is unused by the
  # coding.
  for (v in names(gss$value_labels)) {
    observed <- unique(d[[v]][!is.na(d[[v]])])
    expect_true(
      all(as.character(observed) %in% names(gss$value_labels[[v]])),
      info = v
    )
  }
  expect_identical(
    unname(gss$value_labels$lkelyvot),
    c(
      "Not at all likely",
      "Not very likely",
      "Somewhat likely",
      "Very likely",
      "Extremely likely"
    )
  )
  expect_identical(names(gss$value_labels$attend)[1], "0")
  expect_match(
    gss$codebook$coding[gss$codebook$variable == "female"],
    "1 = female; 0 = male"
  )
  expect_match(
    gss$codebook$note[gss$codebook$variable == "lkelyvot"],
    "not U.S. citizens"
  )
  expect_identical(
    unname(gss$labels["raclive"]),
    "Neighbours of the other race (Black/white)"
  )
  expect_identical(
    unname(gss$value_labels$economy),
    c("Very bad", "Bad", "Neither bad nor good", "Good", "Very good")
  )
  expect_identical(
    unname(gss$value_labels$immigration)[c(1, 5)],
    c("Reduced a lot", "Increased a lot")
  )
  expect_identical(
    unname(gss$value_labels$class),
    c("Lower class", "Working class", "Middle class", "Upper class")
  )
  # Plain 0/1 flags show their codes, as in the data; meaningful names stay.
  expect_null(gss$value_labels$female)
  expect_null(gss$value_labels$married)
  expect_identical(
    unname(gss$value_labels$conservative),
    c("Liberal/moderate", "Conservative")
  )
  expect_identical(
    gss$effect_label,
    "Treatment effect on Trump rating (points)"
  )
  # Reading needs no download and gives the same result each time.
  expect_identical(load_gss2024(), gss)
})

test_that("the build recodes by label meaning, with missing codes as NA", {
  b <- builder()
  raw <- fake_gss()
  demo <- b$build_gss_demo(raw)
  d <- demo$data
  cb <- demo$codebook
  covariates <- cb$variable[cb$role == "characteristic"]
  others <- cb$variable[cb$role %in% c("other dependent", "other independent")]
  kept <- !is.na(raw$ratechall124) & !is.na(raw$polviews)
  expect_equal(nrow(d), sum(kept))
  expect_identical(
    names(d),
    c(
      "id",
      "rating_trump",
      "conservative",
      covariates,
      others,
      "svy_weight",
      "psu_cluster"
    )
  )
  expect_length(covariates, 39L)
  r <- raw[kept, ]
  expect_equal(d$conservative, as.numeric(r$polviews > 4))
  expect_equal(d$happy, 4 - as.numeric(r$happy))
  expect_equal(d$pray, 6 - as.numeric(r$pray))
  expect_equal(d$finalter, c(3, 1, 2)[as.numeric(r$finalter)])
  expect_equal(d$lgb, as.numeric(r$sexornt %in% 1:2))
  expect_equal(d$rural, as.numeric(r$res16 <= 3))
  expect_equal(d$hispanic, as.numeric(r$hispanic != 1))
  expect_true(all(is.na(d$age[r$age == 98])))
  expect_true(all(is.na(d$vetyears[r$vetyears == 4])))
  expect_equal(sum(is.na(d$vetyears)), sum(r$vetyears == 4))
  expect_equal(d$employed, as.numeric(r$wrkstat <= 3))
  expect_equal(d$no_religion, as.numeric(r$relig == 4))
  expect_equal(d$college, as.numeric(r$degree >= 3))
  expect_equal(d$economy, 6 - as.numeric(r$econstat))
  expect_equal(d$immigration, 6 - as.numeric(r$letin1a))
  expect_equal(d$class, ifelse(r$class == 5, NA, as.numeric(r$class)))
  expect_identical(
    sort(unique(d$region)),
    sort(c("Northeast", "Midwest", "South", "West")[sort(unique(as.numeric(
      r$region
    )))])
  )
  expect_identical(
    cb$values[cb$variable == "lkelyvot"],
    "1=Not at all likely|2=Not very likely|3=Somewhat likely|4=Very likely|5=Extremely likely"
  )
  expect_identical(cb$variable, names(d))
  expect_match(cb$coding[cb$variable == "female"], "1 = female; 0 = male")
  expect_identical(cb$label[cb$variable == "white"], "White")
})

test_that("the build follows the labels when codes move, and stops when labels change", {
  b <- builder()
  raw <- fake_gss()
  # The same meaning under different codes gives the same recoded values.
  shuffled <- raw
  shuffled$happy <- structure(
    unname(c(`1` = 2, `2` = 3, `3` = 1)[as.character(as.numeric(raw$happy))]),
    labels = c("not too happy" = 1, "very happy" = 2, "pretty happy" = 3)
  )
  expect_equal(
    b$build_gss_demo(shuffled)$data$happy,
    b$build_gss_demo(raw)$data$happy
  )
  renamed <- raw
  attr(renamed$health, "labels") <- c(
    outstanding = 1,
    good = 2,
    fair = 3,
    poor = 4
  )
  expect_error(b$build_gss_demo(renamed), "GSS item health")
  extra <- raw
  attr(extra$sexornt, "labels") <- c(
    attr(raw$sexornt, "labels"),
    "something else" = 4
  )
  expect_error(b$build_gss_demo(extra), "unexpected category 'something else'")
  expect_error(
    b$build_gss_demo(raw[setdiff(names(raw), "lkelyvot")]),
    "lacks: lkelyvot"
  )
  negative <- raw
  negative$educ[1:3] <- -99
  rebuilt <- b$build_gss_demo(negative)$data
  expect_true(all(is.na(rebuilt$educ[rebuilt$id %in% 1:3])))
})
