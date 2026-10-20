set.seed(3)
n <- 800
X <- data.frame(a = rnorm(n), b = rnorm(n), d = sample(0:1, n, TRUE))
W <- rbinom(n, 1, .5)
Y <- 2 * (X$a > 0) * W + X$b + rnorm(n)
report <- explore_heterogeneity(Y, W, X, W_hat = .5, num_trees = 300, seed = 1)

test_that("the report contains descriptive outputs and the planted pattern", {
  expect_s3_class(report, "heterogeneity_report")
  expect_named(report$ate, c("estimate", "status", "standardized", "sd_y", "sd_w"))
  expect_named(report$cates, c("id", "cate_estimate", "quintile"))
  expect_named(report$moderators, c("variable", "label", "importance", "rank"))
  expect_null(report$test)
  expect_equal(nrow(report$cates), n)
  expect_equal(report$moderators$variable[1], "a")
  expect_equal(sum(report$quintiles$n), n)
  expect_output(print(report, plots = FALSE), "Exploratory heterogeneity report")
  expect_equal(report$ate$estimate, unname(grf::average_treatment_effect(report$forest)["estimate"]))
  tab <- effect_by(report, "d")
  for (level in 0:1) {
    expected <- grf::average_treatment_effect(report$forest, subset = which(X$d == level))["estimate"]
    expect_equal(tab$estimate[tab$d == as.character(level)], unname(expected))
  }
})

test_that("all plots render without warnings, including unavailable means", {
  withr::local_pdf(NULL)
  plots <- list(plot(report), plot(report, "importance"), plot(report, "profiles"),
    plot(effect_by(report, "a")), plot(effect_by(report, "d")), plot(effect_by(report, "a", "d")),
    plot(effect_by(report, "a", min_n = n + 1)), plot(effect_by(report, "a", "d", min_n = n + 1)))
  for (p in plots) {
    expect_no_warning(ggplot2::ggplotGrob(p))
    layers <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    expect_false(any(layers %in% c("GeomVline", "GeomHline")))
  }
  expect_s3_class(effect_by(report, "a"), c("heterogeneity_groups", "data.frame"))
  expect_error(plot(effect_by(report, "a")[1:2, ]), "unmodified")
  tab <- effect_by(report, "a", "d", min_n = n + 1)
  expect_true(all(is.na(tab$estimate)))
  expect_true(all(tab$status == paste("fewer than", n + 1, "observations")))
})

test_that("missing covariates remain missing and original IDs survive filtering", {
  d <- data.frame(y = Y, w = W, a = X$a, b = X$b)
  d$a[1:5] <- NA; d$y[6:7] <- NA
  inp <- prepare_inputs(d, "y", "w", c("a", "b"))
  expect_equal(inp$n, n - 2)
  expect_equal(sum(is.na(inp$X$a)), 5)
  expect_identical(inp$ids, seq_len(n)[-c(6, 7)])
  expect_equal(inp$missingness$n_missing, c(5L, 0L))
  expect_false("missing" %in% c(names(formals(explore_heterogeneity)), names(formals(prepare_inputs))))
  d$a[8:80] <- NA
  r <- explore_heterogeneity(data = d, y = "y", w = "w", x = c("a", "b"),
    W_hat = seq(.3, .7, length.out = n), num_trees = 100)
  expect_equal(r$settings$W_hat, seq(.3, .7, length.out = n)[-c(6, 7)])
  expect_true(all(is.finite(r$cates$cate_estimate)))
  expect_equal(r$ate$estimate, unname(grf::average_treatment_effect(r$forest)["estimate"]))
  tab <- effect_by(r, "a")
  expect_equal(sum(tab$n), n - 2)
  expect_equal(tab$n[tab$a == "(Missing)"], 78)
  expect_true(is.finite(tab$estimate[tab$a == "(Missing)"]))
  withr::local_pdf(NULL)
  expect_no_warning(ggplot2::ggplotGrob(plot(r, "profiles")))
})

test_that("categorical variables retain missingness, labels and grouped importance", {
  d <- data.frame(y = Y, arm = ifelse(W == 1, "treated", "control"), a = X$a,
    region = rep(c("North", "South", "West", NA), n / 4), check.names = FALSE)
  d[["region=West"]] <- X$b
  r <- explore_heterogeneity(data = d, y = "y", w = "arm", x = c("a", "region", "region=West"),
    treated = "treated", W_hat = .5, num_trees = 100, labels = c(region = "Region"))
  expect_equal(anyDuplicated(names(r$X)), 0L)
  expect_true(all(is.na(r$X[is.na(d$region), r$feature_map$feature[r$feature_map$variable == "region"], drop = FALSE])))
  expect_equal(r$covariates$region, d$region)
  expect_setequal(r$moderators$variable, c("a", "region", "region=West"))
  imp <- as.numeric(grf::variable_importance(r$forest))
  expect_equal(r$moderators$importance[r$moderators$variable == "region"], sum(imp[r$feature_map$variable == "region"]))
  expect_equal(nrow(effect_by(r, "region")), 4L)
  expect_identical(r$labels[["region"]], "Region")
  expect_identical(r$moderators$label[r$moderators$variable == "region"], "Region")
  expect_equal(r$settings$w_labels, c(`0` = "control", `1` = "treated"))
  withr::local_pdf(NULL)
  expect_no_warning(ggplot2::ggplotGrob(plot(effect_by(r, "region"))))
  expect_no_warning(ggplot2::ggplotGrob(plot(r, "profiles")))
  # The two-way table keeps missing-value groups; the heatmap leaves them out.
  two <- effect_by(r, "region", "a")
  expect_true(any(two$region == "(Missing)"))
  heat <- plot(two)
  expect_false(any(heat$data$g1 == "(Missing)"))
  expect_equal(nrow(heat$data), sum(two$region != "(Missing)"))
  expect_no_warning(ggplot2::ggplotGrob(heat))
})

test_that("constant and entirely missing covariates are handled without requiring full rank", {
  d <- data.frame(y = Y, w = W, a = X$a, duplicate = X$a, same = "fixed", empty = NA_real_,
    empty_text = NA_character_, missing_only = rep(c("observed", NA), n / 2))
  inp <- prepare_inputs(d, "y", "w", setdiff(names(d), c("y", "w")))
  expect_false(any(c("same", "empty", "empty_text") %in% names(inp$covariates)))
  expect_true(all(c("a", "duplicate", "missing_only") %in% names(inp$covariates)))
  r <- explore_heterogeneity(data = d, y = "y", w = "w", x = c("a", "duplicate", "missing_only"), num_trees = 100)
  expect_true(all(is.finite(r$cates$cate_estimate)))
  expect_true(any(grepl("Highly correlated characteristics share importance (|r| >= 0.9): a & duplicate (r = 1.00)", r$notes, fixed = TRUE)))
  expect_false(any(grepl("Highly correlated", report$notes)))
  expect_error(prepare_inputs(d, "y", "w", c("empty", "same")), "No usable")
  many <- as.data.frame(matrix(rep(X$a[1:60], 80), nrow = 60))
  r <- explore_heterogeneity(Y[1:60], rep(0:1, 30), many, W_hat = .5, num_trees = 100)
  expect_equal(nrow(r$moderators), 80)
})

test_that("weights and clusters affect means consistently without blocking single-cluster groups", {
  set.seed(44)
  d <- data.frame(y = Y, w = W, a = X$a, z = rep(0:1, each = n / 2),
    cluster = c(rep(letters[1:10], each = 40), rep("z", n / 2)), weight = rep(c(1, 3), n / 2))
  r <- explore_heterogeneity(data = d, y = "y", w = "w", x = c("a", "z"),
    weights = "weight", clusters = "cluster", W_hat = .5, num_trees = 200)
  expect_true(r$settings$weighted)
  expect_true(r$settings$clustered)
  expect_equal(r$ate$estimate, unname(grf::average_treatment_effect(r$forest)["estimate"]))
  tab <- effect_by(r, "z")
  expect_true(all(is.finite(tab$estimate)))
  expect_equal(tab$estimate[tab$z == "1"], weighted.mean(grf::get_scores(r$forest)[d$z == 1], d$weight[d$z == 1]))
  for (k in r$quintiles$quintile) {
    rows <- r$cates$quintile == k
    expect_equal(r$quintiles$mean_prediction[r$quintiles$quintile == k], weighted.mean(r$cates$cate_estimate[rows], d$weight[rows]))
  }
  expect_true(all(grepl("fewer than 1000 positive-weight observations", effect_by(r, "z", min_n = 1000)$status)))
  profiles <- heterogeneity:::profile_summary(r, "a")
  first <- r$cates$quintile == profiles$quintile[1]
  expect_equal(profiles$mean[1], weighted.mean(d$a[first], d$weight[first]))
})

test_that("small and unsupported subgroups have clear statuses and truthful counts", {
  set.seed(27)
  d <- data.frame(z = rep(0:1, each = 200), a = rnorm(400))
  w <- c(rep(0:1, 100), rep(1, 200)); y <- w + d$a + rnorm(400)
  r <- explore_heterogeneity(y, w, d, W_hat = .5, num_trees = 100)
  tab <- effect_by(r, "z")
  expect_true(is.na(tab$estimate[tab$z == "1"]))
  expect_match(tab$status[tab$z == "1"], "one group of W")
  expect_equal(tab$n_control[tab$z == "1"], 0)
  r$weights[1:180] <- 0
  tab <- effect_by(r, "z")
  expect_equal(tab$n[1], 200)
  expect_equal(tab$n_positive[1], 20)
  expect_match(tab$status[1], "fewer than 30")
  r <- report; r$scores[1] <- Inf
  expect_match(effect_by(r, "d")$status[as.character(effect_by(r, "d")$d) == as.character(X$d[1])], "overlap")
  r$weights[1] <- 0
  expect_true(all(is.finite(effect_by(r, "d")$estimate)))
})

test_that("reserved names do not corrupt groups or plots", {
  withr::local_pdf(NULL)
  odd <- X; names(odd) <- c("g2", "g1", "estimate")
  r <- explore_heterogeneity(Y, W, odd, W_hat = .5, num_trees = 100)
  tab <- effect_by(r, "g2", "estimate", bins = 3)
  expect_equal(anyDuplicated(names(tab)), 0L)
  expect_equal(sum(tab$n), n)
  p <- plot(effect_by(r, "g2", "g1", bins = 3))
  expect_equal(nlevels(p$data$g1), 3L)
  expect_equal(nlevels(p$data$g2), 3L)
  expect_no_warning(ggplot2::ggplotGrob(p))
  expect_no_warning(ggplot2::ggplotGrob(plot(r, "profiles")))
  odd <- X; names(odd) <- c("n", "quintile", "a b")
  r <- explore_heterogeneity(Y, W, odd, W_hat = .5, num_trees = 100)
  expect_equal(sum(effect_by(r, "n")$n), n)
  expect_no_warning(ggplot2::ggplotGrob(plot(r, "profiles")))
})

test_that("binning handles one bin, ties, missing categories and close values", {
  expect_equal(nrow(effect_by(report, "a", bins = 1)), 1)
  groups <- heterogeneity:::bin_covariate(1 + seq_len(100) / 1e7, 5, 8)
  expect_equal(nlevels(groups), 5)
  expect_equal(as.numeric(table(groups)), rep(20, 5))
  groups <- heterogeneity:::bin_covariate(c("(Missing)", NA, "A"), 5, 8)
  expect_equal(nlevels(groups), 3L)
  expect_false(anyNA(groups))
  expect_equal(length(unique(heterogeneity:::prediction_groups(rep(1, 100)))), 1L)
  expect_equal(length(unique(heterogeneity:::prediction_groups(c(rep(0, 95), rep(1, 5))))), 2L)
  v <- rep(1:3, c(60, 30, 10)); g <- heterogeneity:::prediction_groups(v)
  expect_true(all(vapply(split(g, v), function(x) length(unique(x)) == 1L, logical(1))))
})

test_that("invalid inputs fail before fitting", {
  fit <- function(...) explore_heterogeneity(Y, W, X, num_trees = 100, ...)
  expect_error(fit(W_hat = 0), "strictly between")
  expect_error(fit(W_hat = c(.5, .6)), "original input row")
  expect_error(fit(weights = rep(0, n)), "positive")
  expect_error(fit(weights = rep(Inf, n)), "finite")
  expect_error(fit(clusters = rep("school", n)), "two clusters")
  expect_error(fit(seed = NA_real_), "whole number")
  expect_error(fit(labels = c(age = NA_character_)), "labels")
  expect_error(explore_heterogeneity(replace(Y, 1, Inf), W, X), "finite")
  expect_error(explore_heterogeneity(Y, rep(0, n), X), "independent variable must take at least two values ('W' has 1)", fixed = TRUE)
  expect_error(effect_by(report, "a", "a"), "different")
  expect_error(effect_by(report, "a", bins = 0), "whole number")
  expect_error(explore_heterogeneity(data = data.frame(y = Y, w = W, a = X$a), Y = Y), "either")
  d <- data.frame(y = Y, w = W, id = seq_len(n), text = paste0("person", seq_len(n)))
  expect_error(prepare_inputs(d, "y", "w", "text"), "at most 20")
  expect_error(prepare_inputs(d, "y", "w", "id", ids = "id"), "not the ID")
  d$bad <- I(rep(list(c(1, 2)), n))
  expect_error(prepare_inputs(d, "y", "w", "bad"), "Unsupported")
})

test_that("comparison coding, RNG state and actual tree counts are reproducible", {
  d <- data.frame(y = Y, w = W, X)
  a <- prepare_inputs(d, "y", "w", "a")
  b <- prepare_inputs(d, "y", "w", "a", treated = 0)
  expect_equal(a$W, 1 - b$W)
  set.seed(123); before <- .Random.seed
  r <- explore_heterogeneity(data = d, y = "y", w = "w", x = names(X), W_hat = .5, num_trees = 101,
    value_labels = list(w = c(`0` = "Control", `1` = "Programme")))
  expect_identical(.Random.seed, before)
  expect_equal(r$settings$num_trees, 102)
  expect_equal(r$settings$w_labels, c(`0` = "Control", `1` = "Programme"))
  ids <- paste0("person-", seq_len(n))
  r <- explore_heterogeneity(Y, W, X["a"], ids = ids, W_hat = .5, num_trees = 100)
  expect_identical(r$cates$id, ids)
})

test_that("the correlation helper uses available numeric pairs", {
  d <- data.frame(a = 1:100, b = c(NA, 2:100), c = rep(1, 100), text = "a")
  pairs <- highly_correlated(d)
  expect_equal(nrow(pairs), 1L)
  expect_equal(pairs$r, 1)
  expect_equal(nrow(highly_correlated(d["a"])), 0L)
})

test_that("heatmap colours cover every estimate and unavailable cells show only their count", {
  # Without the pad, rounding puts an extreme value outside the limits in
  # about one draw in eight, and ggplot2 then paints it as missing.
  set.seed(5)
  inside <- vapply(1:500, function(k) {
    values <- c(rnorm(6, sd = 20), NA)
    lim <- heterogeneity:::colour_limits(values, midpoint = rnorm(1, sd = 20))
    all(values[!is.na(values)] >= lim[1] & values[!is.na(values)] <= lim[2])
  }, logical(1))
  expect_true(all(inside))
  withr::local_pdf(NULL)
  cutoff <- floor(stats::median(effect_by(report, "a", "d", bins = 3)$n)) + 1
  p <- plot(effect_by(report, "a", "d", bins = 3, min_n = cutoff))
  built <- ggplot2::ggplot_build(p)$data[[1]]
  available <- is.finite(p$data$estimate)
  expect_true(any(!available) && any(available))
  expect_false(any(built$fill[available] == "grey85"))
  expect_true(all(built$fill[!available] == "grey85"))
  expect_true(all(grepl("^n = [0-9]+$", p$data$label[!available])))
})

test_that("printed reports wrap long notes to the console width", {
  withr::local_options(width = 50)
  r <- report
  r$notes <- c(r$notes, paste(rep("A long preparation note that must wrap.", 4), collapse = " "))
  out <- capture.output(print(r, plots = FALSE))
  expect_true(all(nchar(out) <= 50))
  expect_true(any(grepl("^  - A long preparation note", out)))
})

test_that("printing shows the three overview plots unless asked not to", {
  pages <- function(...) {
    file <- withr::local_tempfile(fileext = ".pdf")
    grDevices::pdf(file)
    capture.output(print(report, ...))
    grDevices::dev.off()
    sum(grepl("/Type /Page[^s]", readLines(file, warn = FALSE, skipNul = TRUE), useBytes = TRUE))
  }
  expect_equal(pages(), 3L)
  expect_equal(pages(plots = FALSE), 0L)
})

test_that("profile cells are average z-scores and group plots leave out missing values", {
  d <- profile_summary(report, "a")
  z <- (X$a - mean(X$a)) / sqrt(mean((X$a - mean(X$a))^2))
  expect_equal(d$z, as.numeric(tapply(z, report$cates$quintile, mean)))
  p <- plot(report, "profiles")
  expect_true(all(p$data$value_label == sprintf("%+.2f", p$data$z)))
  dd <- data.frame(y = Y, w = W, a = X$a, region = rep(c("North", "South", NA, "West"), n / 4))
  r <- explore_heterogeneity(data = dd, y = "y", w = "w", x = c("a", "region"), W_hat = .5, num_trees = 100)
  tab <- effect_by(r, "region")
  expect_true("(Missing)" %in% tab$region)
  dots <- plot(tab)
  expect_false(any(grepl("Missing", dots$data$x_label)))
  expect_equal(nrow(dots$data), 3L)
})

test_that("untidy text values are cleaned before fitting", {
  set.seed(8)
  d <- data.frame(y = as.character(round(rnorm(120), 3)), w = rep(c("a", "b"), 60),
                  age = sample(c(as.character(20:60), "N/A", " . "), 120, TRUE), region = sample(c("North", "South", "NA", ""), 120, TRUE))
  d$y[3] <- "n/a"
  inp <- prepare_inputs(d, "y", "w", c("age", "region"))
  expect_true(is.numeric(inp$Y) && is.numeric(inp$covariates$age))
  expect_equal(inp$n, 119)
  expect_true(all(is.na(inp$covariates$age[d$age[-3] %in% c("N/A", " . ")])))
  expect_setequal(unique(stats::na.omit(inp$covariates$region)), c("North", "South"))
  expect_true(any(grepl("read as numbers: y, age", inp$notes, fixed = TRUE)))
  expect_true(any(grepl("^1 row was dropped", inp$notes)))
  # Attributes such as variable labels do not reach grf.
  attr(d$y, "label") <- "An outcome"; attr(d$age, "format.spss") <- "F8.2"
  expect_null(attributes(prepare_inputs(d, "y", "w", c("age", "region"))$Y))
})

test_that("a numeric comparison gives slopes that recover a planted pattern", {
  set.seed(9)
  d <- data.frame(a = rnorm(n), b = rnorm(n), dose = runif(n, 0, 4))
  d$y <- d$dose * (1 + (d$a > 0)) + d$b + rnorm(n)
  r <- explore_heterogeneity(data = d, y = "y", w = "dose", x = c("a", "b"), num_trees = 300)
  expect_identical(r$settings$w_type, "continuous")
  expect_null(r$settings$w_labels)
  expect_identical(r$settings$effect_label, "Change in y per unit of dose")
  expect_equal(r$ate$estimate, unname(grf::average_treatment_effect(r$forest)["estimate"]))
  expect_equal(r$moderators$variable[1], "a")
  tab <- effect_by(r, "a")
  expect_false(any(c("n_control", "n_treated") %in% names(tab)))
  expect_gt(utils::tail(tab$estimate, 1), tab$estimate[1] + 0.5)
  expect_output(print(r, plots = FALSE), "Average adjusted slope (average partial effect)", fixed = TRUE)
  expect_output(print(r, plots = FALSE), "Effect of W: change per one-unit increase in dose")
  withr::local_pdf(NULL)
  expect_match(plot(r)$labels$title, "slopes")
  expect_match(plot(tab)$labels$caption, "no spread in W")
  expect_no_warning(ggplot2::ggplotGrob(plot(tab)))
  expect_no_warning(ggplot2::ggplotGrob(plot(effect_by(r, "a", "b"))))
  # A group in which the comparison does not vary is not estimated.
  flat <- r; flat$W[flat$covariates$a > 1] <- 2
  expect_true(any(effect_by(flat, "a", bins = 10)$status == "W does not vary in this group"))
  expect_error(prepare_inputs(data.frame(y = d$y, w = rep(c("a", "b", "c"), length.out = n), a = d$a), "y", "w", "a"), "must be numeric")
  expect_error(prepare_inputs(d, "y", "dose", "a", treated = 1), "only to an independent variable with two values")
  expect_error(explore_heterogeneity(data = d, y = "y", w = "dose", x = "a", W_hat = Inf, num_trees = 100), "finite")
})

test_that("the summary gives the range, candidate moderators, defaults and a data-driven histogram", {
  expect_equal(report$prediction_range, range(report$cates$cate_estimate))
  out <- capture.output(print(report, plots = FALSE))
  expect_true(any(grepl("Predicted differences range from", out)))
  expect_true(any(grepl("Average adjusted difference (average treatment effect)", out, fixed = TRUE)))
  expect_true(any(grepl("Effect of W: 1 minus 0", out, fixed = TRUE)))
  expect_true(any(grepl("Candidate moderators (most used in tree splits)", out, fixed = TRUE)))
  expect_false(any(grepl("Central 90|Mean prediction", out)))
  expect_equal(plot(report)$layers[[1]]$stat_params$bins, grDevices::nclass.FD(report$cates$cate_estimate))
  r <- explore_heterogeneity(data = data.frame(y = Y, w = W, a = X$a), y = "y", w = "w", x = "a", num_trees = 100)
  expect_equal(r$settings$seed, 21042020)
  expect_identical(r$settings$effect_label, "Difference in y")
  expect_identical(r$settings$w_type, "binary")
})

test_that("text characteristics get one indicator per category, or one for two categories", {
  set.seed(12)
  d <- data.frame(y = rnorm(200), w = rep(0:1, 100), sex = sample(c("female", "male"), 200, TRUE),
                  region = sample(c("North", "South", "West", NA), 200, TRUE), one = rep(c("yes", NA), 100))
  inp <- prepare_inputs(d, "y", "w", c("sex", "region", "one"))
  map <- inp$feature_map
  expect_equal(sum(map$variable == "sex"), 1L)
  expect_equal(map$level[map$variable == "sex"], names(sort(table(d$sex)))[1])
  expect_setequal(map$level[map$variable == "region"], c("North", "South", "West"))
  expect_equal(map$level[map$variable == "one"], "yes")
  observed <- !is.na(d$region)
  expect_true(all(rowSums(inp$X[observed, map$feature[map$variable == "region"]]) == 1))
  expect_true(all(is.na(inp$X[!observed, map$feature[map$variable == "region"]])))
  expect_true(any(grepl("one per category (a single one for two categories)", inp$notes, fixed = TRUE)))
  expect_false(any(grepl("reference", inp$notes)))
})

test_that("small samples run with a note instead of a minimum size", {
  set.seed(13)
  d <- data.frame(y = rnorm(12), w = rep(c("a", "b"), 6), age = rnorm(12))
  r <- explore_heterogeneity(data = d, y = "y", w = "w", x = "age", num_trees = 100)
  expect_true(is.finite(r$ate$estimate))
  expect_true(any(grepl("Only 6 rows have w = 'a' and 6 rows have w = 'b'", r$notes, fixed = TRUE)))
  slopes <- explore_heterogeneity(data = transform(d, w = seq_len(12)), y = "y", w = "w", x = "age", num_trees = 100)
  expect_true(any(grepl("Only 12 rows are used", slopes$notes)))
  expect_false(any(grepl("Only", report$notes)))
  expect_error(explore_heterogeneity(data = d[1:3, ], y = "y", w = "w", x = "age", num_trees = 100), "too few rows")
  expect_error(prepare_inputs(transform(d, y = ifelse(w == "a", NA, y)), "y", "w", "age"), "only one value in the usable rows")
  expect_error(prepare_inputs(transform(d, wt = ifelse(w == "a", 0, 1)), "y", "w", "age", weights = "wt"), "both values")
})

test_that("histogram colours deepen with distance from 0, and the other plots show every characteristic", {
  bars <- ggplot2::layer_data(plot(report))
  expect_false(any(bars$xmin < 0 & bars$xmax > 0))
  expect_false(anyNA(bars$fill))
  expect_gt(length(unique(bars$fill)), 2L)
  # Red minus blue falls steadily from left to right: deep red, light grey, deep teal.
  rgb <- grDevices::col2rgb(bars$fill)
  tint <- rgb["red", ] - rgb["blue", ]
  expect_true(all(diff(tint[order(bars$x)]) <= 0))
  expect_true(all(tint[bars$x > 0] < 0))
  # Equal distances from 0 get equally deep colours: the scale is symmetric.
  limits <- ggplot2::ggplot_build(plot(report))$plot$scales$get_scales("fill")$get_limits()
  expect_equal(limits[1], -limits[2])
  expect_gte(limits[2], max(abs(report$cates$cate_estimate)))
  expect_equal(nrow(plot(report, "importance")$data), nrow(report$moderators))
  expect_setequal(unique(plot(report, "profiles")$data$feature), report$feature_map$feature)
  expect_equal(nrow(plot(report, "importance", top_n = 2)$data), 2L)
})

test_that("the average is also given in standard deviations of a continuous Y", {
  # Two groups: divided by the pooled within-group SD of Y, before adjustment.
  pooled <- sqrt(((sum(W == 1) - 1) * var(Y[W == 1]) + (sum(W == 0) - 1) * var(Y[W == 0])) / (n - 2))
  expect_equal(report$ate$sd_y, pooled)
  expect_equal(report$ate$standardized, report$ate$estimate / pooled)
  expect_true(is.na(report$ate$sd_w))
  out <- capture.output(print(report, plots = FALSE))
  expect_true(any(grepl(sprintf("In standard deviations: %.2f (SD of Y within the two groups: %.2f)",
                                report$ate$standardized, pooled), out, fixed = TRUE)))
  # Equal weights change nothing; unequal weights use weighted spreads.
  same <- explore_heterogeneity(Y, W, X, weights = rep(2, n), W_hat = .5, num_trees = 100)
  expect_equal(same$ate$sd_y, pooled)
  # A numeric W: change in Y, in SDs, per SD of W.
  set.seed(31)
  d <- data.frame(a = rnorm(300), dose = runif(300, 0, 4))
  d$y <- d$dose * (1 + (d$a > 0)) + rnorm(300)
  slopes <- explore_heterogeneity(data = d, y = "y", w = "dose", x = "a", num_trees = 100)
  expect_equal(slopes$ate$standardized, slopes$ate$estimate * sd(d$dose) / sd(d$y))
  expect_output(print(slopes, plots = FALSE), "per SD of dose")
  # A 0/1 Y keeps its differences in proportions only.
  binary_y <- explore_heterogeneity(as.numeric(Y > 0), W, X, W_hat = .5, num_trees = 100)
  expect_true(is.na(binary_y$ate$standardized))
  expect_false(any(grepl("In standard deviations", capture.output(print(binary_y, plots = FALSE)))))
  # The spread helper matches base R for one group and the pooled formula for two.
  expect_equal(heterogeneity:::spread(Y, rep(1, n)), sd(Y))
  expect_equal(heterogeneity:::spread(Y, rep(1, n), W), pooled)
})
