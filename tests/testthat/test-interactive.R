# Tests for interactive FHLUS functions

create_geo_data <- function(n = 12) {
  set.seed(789)
  data.frame(
    GEOID = sprintf("06037%06d", seq_len(n)),
    units = sample(50:200, n, replace = TRUE),
    affordable = sample(0:40, n, replace = TRUE),
    income = sample(30000:90000, n, replace = TRUE),
    area = runif(n, 1, 5),
    rent = sample(1000:2500, n, replace = TRUE),
    neighborhood = paste("Area", LETTERS[seq_len(n)]),
    stringsAsFactors = FALSE
  )
}

score_quietly <- function(df, numerator = "units", ...) {
  fhlus_score(df, numerator, "income", "area", graph = FALSE, verbose = FALSE, ...)
}

# ---- meta ----------------------------------------------------------------

test_that("fhlus_score records the columns it used in meta", {
  result <- score_quietly(create_geo_data())
  expect_equal(result$meta$numerator, "units")
  expect_equal(result$meta$denom, "area")
  expect_equal(result$meta$rank_var, "income")
  expect_null(result$meta$exclude_area)
})

# ---- fhlus_segments ------------------------------------------------------

test_that("fhlus_segments returns one row per geography, without the zero row", {
  df <- create_geo_data()
  seg <- fhlus_segments(score_quietly(df))

  expect_s3_class(seg, "fhlus_segments")
  expect_equal(nrow(seg), nrow(df))
  expect_equal(seg$rank, seq_len(nrow(df)))
  expect_setequal(seg$id, df$GEOID)
})

test_that("segments are contiguous and end at 1", {
  seg <- fhlus_segments(score_quietly(create_geo_data()))
  n <- nrow(seg)

  expect_equal(seg$cum_denom_start[1], 0)
  expect_equal(seg$cum_var_start[1], 0)
  expect_equal(seg$cum_denom_start[-1], seg$cum_denom_end[-n])
  expect_equal(seg$cum_var_start[-1], seg$cum_var_end[-n])
  expect_equal(seg$cum_denom_end[n], 1)
  expect_equal(seg$cum_var_end[n], 1)
})

test_that("segment widths match shares and concentration is the slope", {
  seg <- fhlus_segments(score_quietly(create_geo_data()))

  expect_equal(seg$cum_denom_end - seg$cum_denom_start, seg$share_denom)
  expect_equal(seg$cum_var_end - seg$cum_var_start, seg$share_var)
  expect_equal(seg$concentration, seg$share_var / seg$share_denom)
})

test_that("segments follow the ranking order and carry raw values", {
  df <- create_geo_data()
  seg <- fhlus_segments(score_quietly(df))

  expect_false(is.unsorted(seg$income))
  expect_equal(seg$units, df$units[match(seg$id, df$GEOID)])
  expect_equal(seg$area, df$area[match(seg$id, df$GEOID)])
})

test_that("id is auto-detected, user-specified, or falls back to rank", {
  df <- create_geo_data()
  result <- score_quietly(df)

  expect_equal(attr(fhlus_segments(result), "id_col"), "GEOID")
  seg_nb <- fhlus_segments(result, id = "neighborhood")
  expect_equal(attr(seg_nb, "id_col"), "neighborhood")
  expect_setequal(seg_nb$id, df$neighborhood)

  df$GEOID <- NULL
  seg_rank <- fhlus_segments(score_quietly(df))
  expect_null(attr(seg_rank, "id_col"))
  expect_equal(seg_rank$id, as.character(seg_rank$rank))
})

test_that("invalid id and tooltip_vars give informative errors", {
  result <- score_quietly(create_geo_data())
  expect_error(fhlus_segments(result, id = "nope"), "ID column 'nope' not found")
  expect_error(fhlus_segments(result, tooltip_vars = c("rent", "nope")),
               "tooltip_vars not found.*nope")
  expect_error(fhlus_segments(list()), "class 'fhlus'")
})

test_that("duplicate ids warn", {
  df <- create_geo_data()
  df$dup <- rep(c("a", "b"), length.out = nrow(df))
  expect_warning(fhlus_segments(score_quietly(df), id = "dup"), "duplicate")
})

test_that("tooltip_vars are carried along with labels from names", {
  seg <- fhlus_segments(score_quietly(create_geo_data()),
                        tooltip_vars = c("Median rent" = "rent", "neighborhood"))
  expect_true(all(c("rent", "neighborhood") %in% names(seg)))
  expect_equal(attr(seg, "tooltip_vars"),
               c("Median rent" = "rent", neighborhood = "neighborhood"))
})

test_that("CI columns are added when the result has confidence intervals", {
  df <- create_geo_data()
  expect_false("cum_var_lowest" %in% names(fhlus_segments(score_quietly(df))))

  seg <- fhlus_segments(score_quietly(df, missing_pct = 0.1))
  expect_true(all(c("cum_var_lowest", "cum_var_highest") %in% names(seg)))
  expect_equal(nrow(seg), nrow(df))
})

test_that("zero-denominator geographies get NA concentration", {
  df <- create_geo_data()
  df$area[3] <- 0
  seg <- fhlus_segments(suppressWarnings(
    fhlus_score(df, "units", "income", "area", graph = FALSE)))
  zero <- seg[seg$area == 0, ]
  expect_true(is.na(zero$concentration))
  expect_equal(zero$cum_denom_start, zero$cum_denom_end)
})

test_that("exclude_area and multiple rank variables are recorded", {
  df <- create_geo_data()
  df$open_space <- df$area * 0.1
  df$pov <- runif(nrow(df))
  result <- fhlus_score(df, "units", c("income", "pov"), "area",
                        exclude_area = "open_space", graph = FALSE, verbose = FALSE)
  seg <- fhlus_segments(result)
  expect_true(all(c("income", "pov", "open_space") %in% names(seg)))
  expect_equal(attr(seg, "fhlus_meta")$exclude_area, "open_space")
})

test_that("results without meta need column names supplied", {
  result <- score_quietly(create_geo_data())
  result$meta <- NULL
  expect_error(fhlus_segments(result), "numerator, denom, rank_var")
  seg <- fhlus_segments(result, numerator = "units", denom = "area", rank_var = "income")
  expect_true("units" %in% names(seg))
})

# ---- tooltips ------------------------------------------------------------

test_that("tooltips contain id, values, percentages and extra variables", {
  seg <- fhlus_segments(score_quietly(create_geo_data()),
                        tooltip_vars = c("Median rent" = "rent"))
  tips <- build_tooltips(seg)

  expect_length(tips, nrow(seg))
  expect_match(tips[1], paste0("<b>GEOID ", seg$id[1], "</b>"), fixed = TRUE)
  expect_match(tips[1], "Rank 1 of 12 (1 = lowest income)", fixed = TRUE)
  expect_match(tips[1], "Median rent</span>", fixed = TRUE)
  expect_match(tips[1], "Concentration</span>", fixed = TRUE)
  expect_match(tips[1], "of total", fixed = TRUE)
  expect_false(grepl("CI ", tips[1], fixed = TRUE))

  seg_ci <- fhlus_segments(score_quietly(create_geo_data(), missing_pct = 0.1))
  expect_match(build_tooltips(seg_ci)[1], "CI ", fixed = TRUE)
})

test_that("comparison tooltips combine both policies with shared details once", {
  df <- create_geo_data()
  seg_a <- fhlus_segments(score_quietly(df), tooltip_vars = c("Median rent" = "rent"))
  seg_b <- fhlus_segments(score_quietly(df, "affordable"), tooltip_vars = c("Median rent" = "rent"))
  tips <- build_compare_tooltips(seg_a, seg_b, "All", "Affordable", "red", "blue")

  expect_length(tips, nrow(seg_a))
  count <- function(pattern, x) lengths(regmatches(x, gregexpr(pattern, x, fixed = TRUE)))
  # Shared rows appear once, per-policy rows once per policy
  expect_equal(count("GEOID ", tips[1]), 1)
  expect_equal(count("Median rent", tips[1]), 1)
  expect_equal(count(">area</span>", tips[1]), 1)
  expect_match(tips[1], "<b>All</b>", fixed = TRUE)
  expect_match(tips[1], "<b>Affordable</b>", fixed = TRUE)
  expect_match(tips[1], ">units</span>", fixed = TRUE)
  expect_match(tips[1], ">affordable</span>", fixed = TRUE)
  expect_match(tips[1], "Difference (All", fixed = TRUE)
  expect_match(tips[1], " pts", fixed = TRUE)
})

test_that("comparison tooltips handle geographies missing from one result", {
  df <- create_geo_data()
  seg_a <- fhlus_segments(score_quietly(df))
  seg_b <- fhlus_segments(score_quietly(df[-1, ], "affordable"))
  tips <- build_compare_tooltips(seg_a, seg_b, "All", "Affordable", "red", "blue")
  missing_row <- which(seg_a$id == df$GEOID[1])
  expect_match(tips[missing_row], "Not in this result", fixed = TRUE)
  expect_match(tips[missing_row], "n/a", fixed = TRUE)
})

test_that("comparison tooltips show per-policy denominators when they differ", {
  df <- create_geo_data()
  df$area2 <- df$area * runif(nrow(df), 0.5, 1.5)
  seg_a <- fhlus_segments(score_quietly(df))
  seg_b <- fhlus_segments(fhlus_score(df, "units", "income", "area2",
                                      graph = FALSE, verbose = FALSE))
  tips <- build_compare_tooltips(seg_a, seg_b, "A", "B", "red", "blue")
  expect_match(tips[1], ">area</span>", fixed = TRUE)
  expect_match(tips[1], ">area2</span>", fixed = TRUE)
})

test_that("tooltip text is HTML-escaped", {
  df <- create_geo_data()
  df$neighborhood[1] <- "<script>alert(1)</script>"
  seg <- fhlus_segments(score_quietly(df), tooltip_vars = "neighborhood")
  expect_false(any(grepl("<script>", build_tooltips(seg), fixed = TRUE)))
})

test_that("fmt_value formats numbers, text and NA", {
  expect_equal(fmt_value(c(1234567, 0.123456, NA)), c("1,234,567", "0.1235", "NA"))
  expect_equal(fmt_value(c("a", NA)), c("a", "NA"))
})

test_that("hover points stay inside their segment", {
  seg <- fhlus_segments(score_quietly(create_geo_data()))
  pts <- hover_points(seg)
  expect_true(all(seq_len(nrow(seg)) %in% pts$idx))
  expect_true(all(pts$x >= seg$cum_denom_start[pts$idx] &
                    pts$x <= seg$cum_denom_end[pts$idx]))
})

# ---- widgets -------------------------------------------------------------

test_that("fhlus_interactive returns a plotly widget keyed by geography", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")

  df <- create_geo_data()
  w <- fhlus_interactive(score_quietly(df), tooltip_vars = "rent")
  expect_s3_class(w, "plotly")
  expect_s3_class(w, "htmlwidget")

  built <- plotly::plotly_build(w)$x$data
  hover <- Filter(function(tr) !is.null(tr$customdata), built)
  expect_length(hover, 1)
  expect_setequal(unique(unlist(hover[[1]]$customdata)), df$GEOID)
  expect_false(is.null(w$jsHooks$render))
})

test_that("fhlus_interactive draws CI band only when requested and available", {
  skip_if_not_installed("plotly")
  df <- create_geo_data()
  has_fill <- function(w) {
    any(vapply(plotly::plotly_build(w)$x$data,
               function(tr) identical(tr$fill, "toself") || identical(tr$fill, "tonexty"),
               logical(1)))
  }

  expect_false(has_fill(fhlus_interactive(score_quietly(df, missing_pct = 0.1))))
  expect_true(has_fill(fhlus_interactive(score_quietly(df, missing_pct = 0.1),
                                         show_ci_bands = TRUE)))
  expect_warning(fhlus_interactive(score_quietly(df), show_ci_bands = TRUE),
                 "no CI data")
})

test_that("fhlus_interactive options are respected", {
  skip_if_not_installed("plotly")
  result <- score_quietly(create_geo_data())

  w <- fhlus_interactive(result, highlight = FALSE, boundary_label = NULL)
  expect_null(w$jsHooks$render)
  names_out <- vapply(plotly::plotly_build(w)$x$data,
                      function(tr) if (is.null(tr$name)) "" else tr$name, character(1))
  expect_false("Tract Boundary" %in% names_out)
})

test_that("large inputs switch the hover layer to WebGL", {
  skip_if_not_installed("plotly")
  big <- fhlus_interactive(score_quietly(create_geo_data(2100)), highlight = FALSE)
  types <- vapply(plotly::plotly_build(big)$x$data, function(tr) tr$type, character(1))
  expect_true("scattergl" %in% types)
})

test_that("fhlus_compare_interactive overlays two results", {
  skip_if_not_installed("plotly")
  df <- create_geo_data()
  r1 <- score_quietly(df)
  r2 <- score_quietly(df, numerator = "affordable", missing = 10)

  w <- expect_warning(
    fhlus_compare_interactive(r1, r2, label_a = "All", label_b = "Affordable",
                              show_ci = TRUE),
    "only one result has CI data"
  )
  built <- plotly::plotly_build(w)$x$data
  hover <- Filter(function(tr) !is.null(tr$customdata), built)
  # Same geographies and denominator: one combined hover layer
  expect_length(hover, 1)
  expect_match(hover[[1]]$text[[1]], "Difference (All", fixed = TRUE)
  expect_true(any(grepl("All (FHLUS =", vapply(built, function(tr)
    if (is.null(tr$name)) "" else tr$name, character(1)), fixed = TRUE)))

  expect_warning(
    fhlus_compare_interactive(r1, score_quietly(df, "affordable"), show_ci = TRUE),
    "neither result has CI data"
  )
  expect_error(fhlus_compare_interactive(r1, list()), "result_b must be")
})

test_that("fhlus_compare_interactive uses one hover layer per curve when x differs", {
  skip_if_not_installed("plotly")
  df <- create_geo_data()
  df$area2 <- df$area * runif(nrow(df), 0.5, 1.5)
  r1 <- score_quietly(df)
  r2 <- fhlus_score(df, "units", "income", "area2", graph = FALSE, verbose = FALSE)
  built <- plotly::plotly_build(fhlus_compare_interactive(r1, r2))$x
  expect_length(Filter(function(tr) !is.null(tr$customdata), built$data), 2)
  expect_equal(built$layout$hovermode, "closest")
})

# ---- saving --------------------------------------------------------------

test_that("fhlus_save_html writes a file from a widget or a result", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("htmlwidgets")
  result <- score_quietly(create_geo_data())

  f1 <- tempfile(fileext = ".html")
  out <- fhlus_save_html(fhlus_interactive(result), f1, selfcontained = FALSE)
  expect_true(file.exists(out))
  expect_match(paste(readLines(out, warn = FALSE), collapse = "\n"), "plotly")

  f2 <- tempfile(fileext = ".html")
  fhlus_save_html(result, f2, selfcontained = FALSE, tooltip_vars = "rent")
  expect_true(file.exists(f2))
})

test_that("fhlus_save_html can write a single self-contained file", {
  skip_if_not_installed("plotly")
  skip_if_not(fhlus:::pandoc_found(), "pandoc not available")
  f <- tempfile(fileext = ".html")
  fhlus_save_html(score_quietly(create_geo_data()), f)
  expect_true(file.exists(f))
  expect_false(dir.exists(sub("\\.html$", "_files", f)))
})

test_that("fhlus_save_html rejects unsupported inputs", {
  skip_if_not_installed("plotly")
  expect_error(fhlus_save_html(list(), tempfile()), "must be an htmlwidget")
  cmp <- structure(list(), class = c("fhlus_compare", "list"))
  expect_error(fhlus_save_html(cmp, tempfile()), "fhlus_compare_interactive")
  expect_error(fhlus_save_html(score_quietly(create_geo_data()),
                               file.path(tempdir(), "no_such_dir", "x.html")),
               "Directory does not exist")
})
