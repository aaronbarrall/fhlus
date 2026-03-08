# Tests for grouped FHLUS calculation functions

test_that("grouped_fhlus validates group_var existence", {
  df <- create_grouped_data()
  expect_error(
    grouped_fhlus(df, "nonexistent", "units", "income", "area",
                 graph = FALSE, verbose = FALSE),
    "Group variable.*not found"
  )
})

test_that("grouped_fhlus works with valid grouped data", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  expect_s3_class(result, "grouped_fhlus")
  expect_s3_class(result, "data.frame")
})

test_that("grouped_fhlus handles insufficient data by group", {
  df <- create_mixed_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         verbose = FALSE, graph = FALSE)

  # Check that Insufficient_C has error_reason (wide format: no type column)
  insufficient_rows <- result[result$group == "Insufficient_C", ]
  expect_equal(insufficient_rows$error_reason[1], "insufficient_data")
  expect_true(is.na(insufficient_rows$score[1]))
})

test_that("grouped_fhlus handles zero numerator by group", {
  df <- create_mixed_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         verbose = FALSE, graph = FALSE)

  # Check that Zero_D has error_reason (wide format: no type column)
  zero_rows <- result[result$group == "Zero_D", ]
  expect_equal(zero_rows$error_reason[1], "zero_numerator")
  expect_true(is.na(zero_rows$score[1]))
})

test_that("grouped_fhlus returns correct number of groups", {
  df <- create_grouped_data(n_groups = 5)
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  n_unique_groups <- length(unique(result$group))
  expect_equal(n_unique_groups, 5)
})

test_that("grouped_fhlus has summary statistics", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  expect_false(is.null(attr(result, "summary")))
  expect_true("mean_score" %in% names(attr(result, "summary")))
})

test_that("print.grouped_fhlus works correctly", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  expect_output(print(result), "Grouped Fair Housing Land Use Score")
  expect_output(print(result), "Number of groups")
})

test_that("plot_grouped_fhlus creates a plot", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  p <- plot_grouped_fhlus(result, score_col = "score")
  expect_s3_class(p, "gg")
})

test_that("analyze_grouped_fhlus works without characteristics", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  analysis <- analyze_grouped_fhlus(result)
  expect_true("data" %in% names(analysis))
  expect_true("summary" %in% names(analysis))
})

# --- New tests for wide format and per-group error ---

test_that("grouped_fhlus returns wide format with score columns", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  # Wide format should have score, score_lowest, score_highest

  expect_true("score" %in% names(result))
  expect_true("score_lowest" %in% names(result))
  expect_true("score_highest" %in% names(result))
  # Should NOT have type column
  expect_false("type" %in% names(result))
})

test_that("grouped_fhlus returns one row per group", {
  df <- create_grouped_data(n_groups = 4)
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 4)
  expect_equal(length(unique(result$group)), 4)
})

test_that("grouped_fhlus one row per group with CI", {
  df <- create_grouped_data(n_groups = 3)
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         missing_pct = 0.10,
                         graph = FALSE, verbose = FALSE)

  # Still one row per group even with CI

  expect_equal(nrow(result), 3)
  # CI columns should be populated
  expect_true(all(!is.na(result$score_lowest)))
  expect_true(all(!is.na(result$score_highest)))
})

test_that("grouped_fhlus per-group missing_pct column", {
  df <- create_grouped_data_with_errors()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         missing_pct = "pct_error",
                         graph = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 3)
  # All groups should have CI bounds since they all have pct_error > 0
  expect_true(all(!is.na(result$score_lowest)))
  expect_true(all(!is.na(result$score_highest)))
})

test_that("grouped_fhlus per-group missing column", {
  df <- create_grouped_data_with_errors()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         missing = "abs_error",
                         graph = FALSE, verbose = FALSE)

  expect_equal(nrow(result), 3)
  # All groups should have CI bounds
  expect_true(all(!is.na(result$score_lowest)))
  expect_true(all(!is.na(result$score_highest)))
})

test_that("prepare_missing_by_group bug fix: string column name not treated as scalar", {
  df <- create_grouped_data_with_errors()

  # This should NOT treat "abs_error" as a literal missing value
  # It should look up per-group values from the column
  missing_vals <- fhlus:::prepare_missing_by_group(df, "group", "abs_error", NULL)

  # Each group should have a numeric value from the abs_error column
  expect_true(is.numeric(missing_vals[["Group_A"]]))
  expect_true(is.numeric(missing_vals[["Group_B"]]))
  expect_true(is.numeric(missing_vals[["Group_C"]]))

  # Values should differ (10, 50, 100)
  expect_false(missing_vals[["Group_A"]] == missing_vals[["Group_B"]])
})

test_that("plot_grouped_fhlus backward compat: type='base' maps to score_col='score'", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  # Old-style call should still work
  p <- plot_grouped_fhlus(result, score_col = "base")
  expect_s3_class(p, "gg")
})

test_that("grouped_fhlus stores plot objects when graph=TRUE", {
  df <- create_grouped_data(n_groups = 3)
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = TRUE, verbose = FALSE)

  plots <- attr(result, "plots")
  expect_true(!is.null(plots))
  expect_true(is.list(plots))
  expect_equal(length(plots), 3)
  # Each plot should be a ggplot object
  expect_s3_class(plots[[1]], "gg")
  # Keys should be group names
  expect_true(all(names(plots) %in% unique(df$group)))
})

test_that("grouped_fhlus no plots attribute when graph=FALSE", {
  df <- create_grouped_data()
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = FALSE, verbose = FALSE)

  expect_true(is.null(attr(result, "plots")))
})

test_that("grouped_fhlus graph_subset limits which plots are stored", {
  df <- create_grouped_data(n_groups = 3)
  target_group <- unique(df$group)[1]

  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = TRUE, graph_subset = target_group,
                         verbose = FALSE)

  plots <- attr(result, "plots")
  expect_equal(length(plots), 1)
  expect_equal(names(plots), target_group)
})

test_that("grouped_fhlus plots have group name as subtitle", {
  df <- create_grouped_data(n_groups = 2)
  result <- grouped_fhlus(df, "group", "units", "income", "area",
                         graph = TRUE, verbose = FALSE)

  plots <- attr(result, "plots")
  first_group <- names(plots)[1]
  # Extract subtitle from ggplot object
  plot_subtitle <- plots[[first_group]]$labels$subtitle
  expect_equal(plot_subtitle, first_group)
})
