# Tests for fhlus_to_list()

create_list_data <- function(n = 8) {
  set.seed(321)
  data.frame(
    GEOID = sprintf("06037%06d", seq_len(n)),
    units = sample(50:200, n, replace = TRUE),
    income = sample(30000:90000, n, replace = TRUE),
    area = runif(n, 1, 5),
    rent = sample(1000:2500, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("fhlus_to_list returns scores, meta and one segment per geography", {
  df <- create_list_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)
  x <- fhlus_to_list(result, tooltip_vars = c("Median rent" = "rent"))

  expect_equal(x$format_version, 1L)
  expect_equal(x$score, result$scores$score[1])
  expect_equal(x$auc, result$auc)
  expect_null(x$confidence_interval)
  expect_equal(x$id_col, "GEOID")
  expect_equal(x$tooltip_vars, list("Median rent" = "rent"))
  expect_equal(nrow(x$segments), nrow(df))
  expect_setequal(x$segments$id, df$GEOID)
  expect_type(x$segments$tooltip, "character")
  expect_true(all(c("units", "income", "area", "rent") %in% names(x$segments)))
})

test_that("fhlus_to_list includes CI curves and scores when available", {
  df <- create_list_data()
  result <- fhlus_score(df, "units", "income", "area", missing_pct = 0.1,
                        graph = FALSE, verbose = FALSE)
  x <- fhlus_to_list(result, tooltips = FALSE)

  expect_equal(as.numeric(x$confidence_interval), result$confidence_interval)
  expect_equal(x$scores$type, c("base", "lowest", "highest"))
  expect_true(all(c("cum_var_lowest", "cum_var_highest") %in% names(x$segments)))
  expect_false("tooltip" %in% names(x$segments))
})

test_that("fhlus_to_list serializes to JSON with arrays kept as arrays", {
  skip_if_not_installed("jsonlite")
  df <- create_list_data()
  result <- fhlus_score(df, "units", "income", "area", missing_pct = 0.1,
                        graph = FALSE, verbose = FALSE)
  json <- jsonlite::toJSON(fhlus_to_list(result), dataframe = "rows",
                           auto_unbox = TRUE, na = "null", null = "null",
                           digits = NA)
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_type(back$score, "double")
  expect_length(back$meta$rank_var, 1)
  expect_type(back$meta$rank_var, "list")
  expect_length(back$confidence_interval, 2)
  expect_null(back$meta$tie_breaker)
  expect_null(back$meta$exclude_area)
  expect_length(back$segments, nrow(df))
  expect_equal(back$segments[[1]]$rank, 1)
})
