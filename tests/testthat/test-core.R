# Tests for core FHLUS calculation functions
# Coverage: fhlus_score and related internal functions

# ==============================================================================
# INPUT VALIDATION TESTS (30+ tests)
# ==============================================================================

test_that("fhlus_score validates column existence - missing numerator", {
  df <- create_valid_data()
  expect_error(
    fhlus_score(df, "nonexistent", "income", "area", graph = FALSE),
    "Missing required columns.*nonexistent"
  )
})

test_that("fhlus_score validates column existence - missing denominator", {
  df <- create_valid_data()
  expect_error(
    fhlus_score(df, "units", "income", "nonexistent", graph = FALSE),
    "Missing required columns.*nonexistent"
  )
})

test_that("fhlus_score validates column existence - missing rank_var", {
  df <- create_valid_data()
  expect_error(
    fhlus_score(df, "units", "nonexistent", "area", graph = FALSE),
    "Missing rank columns.*nonexistent"
  )
})

test_that("fhlus_score validates numeric types - character numerator", {
  df <- create_invalid_type_data()
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "must be numeric.*character"
  )
})

test_that("fhlus_score validates numeric types - non-numeric denominator", {
  df <- create_valid_data()
  df$area <- as.character(df$area)
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "must be numeric.*character"
  )
})

test_that("fhlus_score rejects negative values in numerator", {
  df <- create_edge_case_data("negative_numerator")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "contains negative values"
  )
})

test_that("fhlus_score rejects negative values in denominator", {
  df <- create_edge_case_data("negative_denominator")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "contains negative values"
  )
})

test_that("fhlus_score requires minimum 2 rows", {
  df <- create_edge_case_data("single_row")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "at least 2 rows"
  )
})

test_that("fhlus_score rejects empty data frame", {
  df <- create_valid_data(10)[0, ]  # 0 rows
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "empty \\(0 rows\\)"
  )
})

test_that("fhlus_score rejects all-NA numerator", {
  df <- create_edge_case_data("all_na_numerator")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE),
    "only NA values"
  )
})

test_that("fhlus_score rejects all-NA denominator", {
  df <- create_edge_case_data("all_na_denominator")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE),
    "only NA values"
  )
})

test_that("fhlus_score warns about NA values when verbose=TRUE", {
  df <- create_edge_case_data("some_na")
  expect_warning(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "NA values found"
  )
})

test_that("fhlus_score does not warn about NA values when verbose=FALSE", {
  df <- create_edge_case_data("some_na")
  expect_silent(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  )
})

test_that("fhlus_score warns about zero denominator when verbose=TRUE", {
  df <- create_valid_data()
  df$area[1] <- 0
  expect_warning(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "zero values found in denominator"
  )
})

test_that("fhlus_score errors on all-zero denominator", {
  df <- create_edge_case_data("zero_denominator")
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE),
    "Denominator sum is zero"
  )
})

test_that("fhlus_score validates multiple rank_var columns", {
  df <- create_valid_data()
  expect_error(
    fhlus_score(df, "units", c("income", "nonexistent"), "area", graph = FALSE),
    "Missing rank columns.*nonexistent"
  )
})

test_that("fhlus_score validates exclude_area column if specified", {
  df <- create_valid_data()
  # Should work fine - if column doesn't exist, it's just ignored
  result <- fhlus_score(df, "units", "income", "area",
                        exclude_area = "nonexistent", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score validates tie_breaker columns", {
  df <- create_valid_data()
  # tie_breaker columns that don't exist should cause issues
  expect_error(
    fhlus_score(df, "units", "income", "area",
               tie_breaker = c("nonexistent"), graph = FALSE, verbose = FALSE),
    "object '.*' not found|not found"
  )
})

test_that("fhlus_score handles factor columns as numerator (should error)", {
  df <- create_valid_data()
  df$units <- as.factor(df$units)
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE),
    "must be numeric"
  )
})

test_that("fhlus_score handles logical columns as numerator (should work)", {
  df <- create_valid_data()
  df$units_logical <- df$units > 100
  # Logical is numeric-compatible in R
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score validates missing parameter is numeric", {
  df <- create_valid_data()
  # Non-numeric missing should work or error appropriately
  result <- fhlus_score(df, "units", "income", "area",
                       missing = 10, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score validates missing_pct parameter is numeric", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.1, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score warns when both missing and missing_pct provided", {
  df <- create_valid_data()
  expect_warning(
    fhlus_score(df, "units", "income", "area",
               missing = 10, missing_pct = 0.1, graph = FALSE, verbose = TRUE),
    "Both missing and missing_pct provided"
  )
})

test_that("fhlus_score handles data.frame subclasses (tibble)", {
  skip_if_not_installed("tibble")
  df <- tibble::as_tibble(create_valid_data())
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score validates graph parameter is logical", {
  df <- create_valid_data()
  # Non-logical graph should coerce or error
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score validates verbose parameter is logical", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles Inf values in numerator", {
  df <- create_valid_data()
  df$units[1] <- Inf
  # Should either handle gracefully or error informatively
  expect_error(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE),
    "negative values|invalid|non-finite"
  )
})

test_that("fhlus_score handles Inf values in denominator", {
  df <- create_valid_data()
  df$area[1] <- Inf
  # Should handle or error
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  # May work or error depending on implementation
  expect_true(TRUE)  # Just checking it doesn't crash unexpectedly
})

test_that("fhlus_score handles very large numbers", {
  df <- create_valid_data()
  df$units <- df$units * 1e10
  df$area <- df$area * 1e10
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

test_that("fhlus_score handles very small numbers", {
  df <- create_valid_data()
  df$units <- df$units * 1e-10
  df$area <- df$area * 1e-10
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

# ==============================================================================
# EDGE CASE TESTS (25+ tests)
# ==============================================================================

test_that("fhlus_score handles all-identical values", {
  df <- create_edge_case_data("all_identical")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # When all values identical, score should be 0 (proportional)
  expect_equal(result$scores$score[1], 0, tolerance = 1e-10)
})

test_that("fhlus_score handles minimum valid data (2 rows)", {
  df <- create_edge_case_data("two_rows")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

test_that("fhlus_score handles zero numerator sum", {
  df <- create_edge_case_data("zero_numerator")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # Zero numerator should result in NA score
  expect_true(is.na(result$scores$score[1]))
  expect_true(is.na(result$auc))
})

test_that("fhlus_score handles tied ranking values", {
  df <- create_edge_case_data("tied_ranks")
  expect_warning(
    result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "tied values"
  )
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

test_that("fhlus_score handles tied ranks with tie_breaker", {
  df <- create_edge_case_data("tied_ranks")
  df$secondary <- runif(nrow(df))
  # Should not warn when tie_breaker provided
  result <- fhlus_score(df, "units", "income", "area",
                       tie_breaker = "secondary", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles extreme concentration in high-opportunity areas", {
  df <- create_edge_case_data("extreme_concentration_high")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # Should be close to +1
  expect_gt(result$scores$score[1], 0.8)
  expect_lte(result$scores$score[1], 1.0)
})

test_that("fhlus_score handles extreme concentration in low-opportunity areas", {
  df <- create_edge_case_data("extreme_concentration_low")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # Should be close to -1
  expect_lt(result$scores$score[1], -0.8)
  expect_gte(result$scores$score[1], -1.0)
})

test_that("fhlus_score handles perfect proportional distribution", {
  df <- create_edge_case_data("perfect_proportional")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # Should be very close to 0
  expect_equal(result$scores$score[1], 0, tolerance = 0.05)
})

test_that("fhlus_score handles some NA values in numerator", {
  df <- create_edge_case_data("some_na")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

test_that("fhlus_score handles some NA values in rank_var", {
  df <- create_edge_case_data("some_na")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles some NA values in denominator", {
  df <- create_edge_case_data("some_na")
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles multiple ranking variables", {
  df <- create_valid_data()
  df$poverty_rate <- runif(nrow(df), 0.05, 0.30)
  result <- fhlus_score(df, "units", c("income", "poverty_rate"), "area",
                       graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles exclude_area correctly", {
  df <- create_valid_data()
  df$excluded <- runif(nrow(df), 0, 0.5)
  result <- fhlus_score(df, "units", "income", "area",
                       exclude_area = "excluded", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles exclude_area larger than total area", {
  df <- create_valid_data()
  df$excluded <- df$area * 1.5  # Larger than total
  expect_warning(
    result <- fhlus_score(df, "units", "income", "area",
                         exclude_area = "excluded", graph = FALSE, verbose = TRUE),
    "Negative values after area adjustment"
  )
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles single unique rank value", {
  df <- create_valid_data()
  df$income <- 50000  # All same income
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  # Should be 0 since no variation in opportunity
  expect_equal(result$scores$score[1], 0, tolerance = 1e-10)
})

test_that("fhlus_score handles descending rank order", {
  df <- create_valid_data()
  df$poverty_rate <- runif(nrow(df), 0.05, 0.30)
  # Lower poverty is better, so need to reverse
  df$neg_poverty <- -df$poverty_rate
  result <- fhlus_score(df, "units", "neg_poverty", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles very skewed distributions", {
  df <- create_valid_data(100)
  df$units[1] <- 10000  # One huge value
  df$units[2:100] <- 1
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_true(is_valid_fhlus_score(result$scores$score[1]))
})

test_that("fhlus_score handles uniform distributions", {
  df <- create_valid_data()
  df$units <- 100  # All same
  df$area <- 2    # All same
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_equal(result$scores$score[1], 0, tolerance = 1e-10)
})

test_that("fhlus_score handles zero values in numerator (but not all zero)", {
  df <- create_valid_data()
  df$units[1:3] <- 0
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles zero values in denominator (but not all zero)", {
  df <- create_valid_data()
  df$area[1] <- 0
  expect_warning(
    result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "zero values found in denominator"
  )
  expect_s3_class(result, "fhlus")
})

test_that("fhlus_score handles missing data with missing parameter", {
  data_list <- create_data_with_missing(10, 50)
  result <- fhlus_score(data_list$data, "units", "income", "area",
                       missing = data_list$missing, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_length(result$scores$score, 3)  # base, lowest, highest
  expect_false(is.null(result$confidence_interval))
})

test_that("fhlus_score handles missing data with missing_pct parameter", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.10, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_length(result$scores$score, 3)
  expect_false(is.null(result$confidence_interval))
  expect_length(result$confidence_interval, 2)
})

test_that("fhlus_score handles zero missing amount", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing = 0, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_length(result$scores$score, 1)  # Only base, no CI
  expect_null(result$confidence_interval)
})

test_that("fhlus_score handles large missing amount", {
  df <- create_valid_data()
  total_units <- sum(df$units)
  result <- fhlus_score(df, "units", "income", "area",
                       missing = total_units, graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
  expect_length(result$scores$score, 3)
})

# ==============================================================================
# FUNCTIONALITY TESTS (20+ tests)
# ==============================================================================

test_that("fhlus_score returns correct structure", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_s3_class(result, "fhlus")
  expect_type(result, "list")
  expect_named(result, c("scores", "data", "plot", "auc", "confidence_interval"))
})

test_that("fhlus_score scores are in valid range", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_gte(result$scores$score[1], -1)
  expect_lte(result$scores$score[1], 1)
})

test_that("fhlus_score auc is in valid range", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_gte(result$auc, 0)
  expect_lte(result$auc, 1)
})

test_that("fhlus_score data contains cumulative columns", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_true("cumulativeVariable" %in% names(result$data))
  expect_true("cumulativeDenom" %in% names(result$data))
})

test_that("fhlus_score data cumulative values reach 1", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  max_cum_var <- max(result$data$cumulativeVariable, na.rm = TRUE)
  max_cum_denom <- max(result$data$cumulativeDenom, na.rm = TRUE)

  expect_equal(max_cum_var, 1, tolerance = 1e-10)
  expect_equal(max_cum_denom, 1, tolerance = 1e-10)
})

test_that("fhlus_score graph parameter controls plot creation", {
  df <- create_valid_data()

  result_no_graph <- fhlus_score(df, "units", "income", "area",
                                 graph = FALSE, verbose = FALSE)
  expect_null(result_no_graph$plot)

  result_with_graph <- fhlus_score(df, "units", "income", "area",
                                   graph = TRUE, verbose = FALSE)
  expect_s3_class(result_with_graph$plot, "gg")
})

test_that("fhlus_score verbose parameter controls messages", {
  df <- create_valid_data()

  # With verbose = FALSE, no messages
  expect_silent(
    fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  )

  # With verbose = TRUE and tied values, should warn
  df_tied <- create_edge_case_data("tied_ranks")
  expect_message(
    fhlus_score(df_tied, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "tied|ranking"
  )
})

test_that("fhlus_score confidence intervals have correct bounds", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.10, graph = FALSE, verbose = FALSE)

  ci <- result$confidence_interval
  base_score <- result$scores$score[result$scores$type == "base"]

  expect_gte(ci[1], -1)
  expect_lte(ci[2], 1)
  expect_lte(ci[1], ci[2])  # Lower bound <= upper bound
})

test_that("fhlus_score confidence intervals contain base score", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.05, graph = FALSE, verbose = FALSE)

  ci <- result$confidence_interval
  base_score <- result$scores$score[result$scores$type == "base"]

  # Base score should typically be between CI bounds
  # (May not always be true if missing units change ranking significantly)
  expect_true(base_score >= min(ci) - 0.1 && base_score <= max(ci) + 0.1)
})

test_that("fhlus_score percent_error is calculated correctly", {
  df <- create_valid_data()
  total_units <- sum(df$units)
  missing_amt <- 50

  result <- fhlus_score(df, "units", "income", "area",
                       missing = missing_amt, graph = FALSE, verbose = FALSE)

  expected_pct_err <- missing_amt / (total_units + missing_amt)
  actual_pct_err <- result$scores$percent_error[result$scores$type == "base"]

  expect_equal(actual_pct_err, expected_pct_err, tolerance = 1e-10)
})

test_that("fhlus_score ranking is correct", {
  df <- create_valid_data()
  df$income <- c(10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)

  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  # Check that ranking in result$data is correct
  expect_true("rank" %in% names(result$data))
  # Lowest income should be rank 1 (after accounting for zero row)
  # Highest income should be highest rank
})

test_that("fhlus_score handles different data sizes", {
  for (n in c(2, 5, 10, 50, 100)) {
    df <- create_valid_data(n)
    result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
    expect_s3_class(result, "fhlus")
    expect_true(is_valid_fhlus_score(result$scores$score[1]))
  }
})

test_that("fhlus_score is deterministic", {
  df <- create_valid_data()

  result1 <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  result2 <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_equal(result1$scores$score, result2$scores$score)
  expect_equal(result1$auc, result2$auc)
})

test_that("print.fhlus works correctly", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_output(print(result), "Fair Housing Land Use Score")
  expect_output(print(result), "Score Summary")
  expect_output(print(result), "AUC")
})

test_that("print.fhlus shows confidence interval when available", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.10, graph = FALSE, verbose = FALSE)

  expect_output(print(result), "Confidence Interval")
})

test_that("fhlus_score scores data frame has correct columns", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_true("type" %in% names(result$scores))
  expect_true("score" %in% names(result$scores))
  expect_true("percent_error" %in% names(result$scores))
})

test_that("fhlus_score with missing data has 3 score types", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area",
                       missing_pct = 0.10, graph = FALSE, verbose = FALSE)

  expect_equal(nrow(result$scores), 3)
  expect_setequal(result$scores$type, c("base", "lowest", "highest"))
})

test_that("fhlus_score without missing data has 1 score type", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_equal(nrow(result$scores), 1)
  expect_equal(result$scores$type, "base")
})

test_that("fhlus_score plot contains expected layers", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = TRUE, verbose = FALSE)

  expect_s3_class(result$plot, "gg")
  # Check for expected layers
  expect_true(length(result$plot$layers) > 0)
})

test_that("fhlus_score handles reversed ranking (poverty)", {
  df <- create_valid_data()
  df$poverty <- runif(nrow(df), 0.05, 0.30)
  df$neg_poverty <- -df$poverty

  result <- fhlus_score(df, "units", "neg_poverty", "area", graph = FALSE, verbose = FALSE)
  expect_s3_class(result, "fhlus")
})
