# Tests for missing rank values (rank_na) and the excluded-area denominator

# A 4 x 4 grid of 1 km squares (UTM 11N). Income rises from west to east, so
# a cell's neighbors bracket its own value.
make_rank_grid <- function() {
  sq <- function(x, y, s = 1000) {
    sf::st_polygon(list(rbind(c(x, y), c(x + s, y), c(x + s, y + s),
                              c(x, y + s), c(x, y))))
  }
  cells <- expand.grid(col = 0:3, row = 0:3)
  n <- nrow(cells)
  sf::st_sf(
    GEOID = sprintf("g%02d", seq_len(n)),
    units = rep(c(10, 20, 5, 15), length.out = n),
    income = 30000 + cells$col * 10000 + cells$row * 1000,
    area = 1,
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(k) sq(380000 + cells$col[k] * 1000,
                                        3770000 + cells$row[k] * 1000)),
      crs = 32611
    )
  )
}

score_quiet <- function(df, ...) {
  fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE, ...)
}

# ---- Missing rank values --------------------------------------------------

test_that("auto drops a missing-rank row with no numerator, with a message", {
  skip_if_not_installed("sf")
  geo <- make_rank_grid()
  geo$income[6] <- NA
  geo$units[6] <- 0

  expect_message(
    r <- fhlus_score(geo, "units", "income", "area", graph = FALSE, verbose = TRUE),
    "Dropped 1 neighborhood\\(s\\) with missing income and no units \\(GEOIDs: g06\\)"
  )
  expect_false("g06" %in% r$data$GEOID)
  expect_equal(r$meta$rank_na$dropped$id, "g06")
  expect_equal(r$meta$rank_na$dropped$numerator_value, 0)
  expect_equal(nrow(r$meta$rank_na$imputed), 0)
})

test_that("auto imputes a missing-rank row that has numerator values (sf input)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  geo$income[6] <- NA

  r <- score_quiet(geo)
  d <- sf::st_drop_geometry(r$data)
  d <- d[d$rank != 0, ]
  expect_true("g06" %in% d$GEOID)
  expect_equal(sum(d$units), sum(geo$units))          # nothing lost
  expect_equal(d$.rank_imputed, d$GEOID == "g06")
  imp <- r$meta$rank_na$imputed
  expect_equal(imp$id, "g06")
  expect_equal(imp$rank_var, "income")
  expect_equal(imp$method, "mean")
  expect_equal(imp$k, 5L)
  expect_equal(d$income[d$GEOID == "g06"], imp$imputed_value)
  expect_equal(nrow(r$meta$rank_na$dropped), 0)
})

test_that("imputed rows are ranked among the rows that bracket their value", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  geo$income[6] <- NA

  d <- sf::st_drop_geometry(score_quiet(geo)$data)
  d <- d[d$rank != 0, ]
  v <- d$income[d$GEOID == "g06"]
  rk <- d$rank[d$GEOID == "g06"]
  expect_true(all(d$rank[d$income < v] < rk))
  expect_true(all(d$rank[d$income > v] > rk))
})

test_that("auto stops for a plain data frame, naming the rows and units lost", {
  df <- sf::st_drop_geometry(make_rank_grid())
  df$income[c(3, 7)] <- NA
  expect_error(score_quiet(df),
               "missing for 2 neighborhood\\(s\\).*GEOIDs: g03, g07; 10 units.*no geometry")
})

test_that("auto stops when an sf row has no neighbors with a value", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  # Two cells far from the rest, each the other's only (k = 1) neighbor
  far <- geo[1:2, ]
  far$GEOID <- c("far1", "far2")
  far$income <- NA
  sf::st_geometry(far) <- sf::st_geometry(far) + c(50000, 50000)
  sf::st_crs(far) <- sf::st_crs(geo)
  geo <- rbind(geo, far)

  expect_error(score_quiet(geo, impute_k = 1),
               "far1, far2.*no neighbors with a value")
})

test_that("drop removes rows with numerator values, with a warning", {
  skip_if_not_installed("sf")
  geo <- make_rank_grid()
  geo$income[6] <- NA
  expect_warning(r <- score_quiet(geo, rank_na = "drop"),
                 "GEOIDs: g06; 20 units lost")
  expect_false("g06" %in% r$data$GEOID)
  expect_equal(r$meta$rank_na$dropped$numerator_value, 20)
})

test_that("impute fills rows without numerator values too", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  geo$income[6] <- NA
  geo$units[6] <- 0
  r <- score_quiet(geo, rank_na = "impute")
  expect_true("g06" %in% r$data$GEOID)
  expect_equal(r$meta$rank_na$imputed$id, "g06")
})

test_that("each rank variable is handled", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  geo$pov <- seq_len(nrow(geo)) / 100
  geo$income[6] <- NA
  geo$pov[9] <- NA
  r <- fhlus_score(geo, "units", c("income", "pov"), "area", graph = FALSE, verbose = FALSE)
  imp <- r$meta$rank_na$imputed
  expect_setequal(paste(imp$id, imp$rank_var), c("g06 income", "g09 pov"))
})

test_that("results without missing rank values record empty logs", {
  r <- score_quiet(sf::st_drop_geometry(make_rank_grid()))
  expect_equal(nrow(r$meta$rank_na$dropped), 0)
  expect_equal(nrow(r$meta$rank_na$imputed), 0)
  expect_false(any(r$data$.rank_imputed))
})

test_that("imputed rows are marked in the tooltips", {
  skip_if_not_installed("sf")
  skip_if_not_installed("spdep")
  geo <- make_rank_grid()
  geo$income[6] <- NA
  seg <- fhlus_segments(score_quiet(geo))
  tips <- build_tooltips(seg)
  expect_match(tips[seg$id == "g06"], "imputed from 5 nearest neighbors \\(mean\\)")
  expect_false(any(grepl("imputed", tips[seg$id != "g06"])))
})

test_that("grouped_fhlus applies rank_na within each group", {
  df <- sf::st_drop_geometry(make_rank_grid())
  df$city <- rep(c("A", "B"), each = 8)
  df$income[3] <- NA

  res <- grouped_fhlus(df, "city", "units", "income", "area", verbose = FALSE)
  expect_true(is.na(res$score[res$city == "A"]))
  expect_match(res$error_reason[res$city == "A"], "cannot be imputed")
  expect_false(is.na(res$score[res$city == "B"]))

  res <- suppressWarnings(grouped_fhlus(df, "city", "units", "income", "area",
                                        rank_na = "drop", verbose = FALSE))
  expect_equal(res$n_rank_dropped[res$city == "A"], 1)
  expect_equal(res$n_rank_dropped[res$city == "B"], 0)
  expect_false(is.na(res$score[res$city == "A"]))
})

test_that("meta$rank_na serializes in fhlus_to_list", {
  skip_if_not_installed("jsonlite")
  geo <- make_rank_grid()
  geo$income[6] <- NA
  geo$units[6] <- 0
  x <- fhlus_to_list(score_quiet(sf::st_drop_geometry(geo)))
  js <- jsonlite::toJSON(x, dataframe = "rows", auto_unbox = TRUE, na = "null",
                         null = "null", digits = NA)
  back <- jsonlite::fromJSON(js)
  expect_equal(back$meta$rank_na$dropped$id, "g06")
  expect_equal(back$meta$denom_used, "area")
})

# ---- Excluded area keeps the original denominator ------------------------

excl_data <- function() {
  data.frame(GEOID = sprintf("g%02d", 1:8), units = c(5, 0, 12, 30, 8, 0, 20, 14),
             income = c(30, 45, 38, 70, 52, 66, 41, 80) * 1000,
             total_area = c(10, 8, 12, 9, 15, 7, 11, 13),
             excluded_area = c(2, 0, 5, 1, 16, 0, 3, 4))
}

test_that("exclude_area keeps the denominator and adds <denom>_adjusted", {
  df <- excl_data()
  r <- suppressWarnings(fhlus_score(df, "units", "income", "total_area",
                                    exclude_area = "excluded_area", graph = FALSE,
                                    verbose = FALSE))
  d <- r$data[r$data$rank != 0, ]
  d <- d[match(df$GEOID, d$GEOID), ]
  expect_equal(d$total_area, df$total_area)
  expect_equal(d$total_area_adjusted, pmax(df$total_area - df$excluded_area, 0))
  expect_equal(d$share_denominator, d$total_area_adjusted / sum(d$total_area_adjusted))
  expect_equal(r$meta$denom_used, "total_area_adjusted")
})

test_that("the score with exclude_area is unchanged from fhlus 0.5.0", {
  r <- suppressWarnings(fhlus_score(excl_data(), "units", "income", "total_area",
                                    exclude_area = "excluded_area", graph = FALSE,
                                    verbose = FALSE))
  # Stored from the version that overwrote the denominator column
  expect_equal(r$auc, 0.43799795709908074, tolerance = 1e-12)
  expect_equal(r$scores$score[1], 0.1482657547630678, tolerance = 1e-12)
})

test_that("tooltips show total, excluded and available area", {
  df <- excl_data()
  df$total_area[1] <- 140954937
  df$excluded_area[1] <- 93050135
  r <- suppressWarnings(fhlus_score(df, "units", "income", "total_area",
                                    exclude_area = "excluded_area", graph = FALSE,
                                    verbose = FALSE))
  seg <- fhlus_segments(r)
  tip <- build_tooltips(seg)[seg$id == "g01"]
  expect_match(tip, "total_area</span> <b>140,954,937</b>")
  expect_match(tip, "Excluded \\(excluded_area\\)</span> <b>93,050,135</b>.*66\\.0% of total_area")
  expect_match(tip, "Available total_area</span> <b>47,904,802</b>")
  expect_match(tip, "used in the score")
  expect_true("total_area_adjusted" %in% names(seg))
})
