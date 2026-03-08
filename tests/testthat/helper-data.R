# Helper functions and fixtures for FHLUS tests

#' Create valid test data
#'
#' @param n Number of rows
#' @return Data frame with valid FHLUS input data
create_valid_data <- function(n = 10) {
  set.seed(123)  # For reproducibility
  data.frame(
    id = 1:n,
    units = sample(50:200, n, replace = TRUE),
    income = sample(30000:90000, n, replace = TRUE),
    area = runif(n, 1, 5),
    stringsAsFactors = FALSE
  )
}

#' Create grouped test data
#'
#' @param n_groups Number of groups
#' @param n_per_group Rows per group
#' @return Data frame with grouped FHLUS input data
create_grouped_data <- function(n_groups = 3, n_per_group = 5) {
  set.seed(456)
  total_n <- n_groups * n_per_group

  data.frame(
    group = rep(paste0("Group_", LETTERS[1:n_groups]), each = n_per_group),
    id = 1:total_n,
    units = sample(50:200, total_n, replace = TRUE),
    income = sample(30000:90000, total_n, replace = TRUE),
    area = runif(total_n, 1, 5),
    stringsAsFactors = FALSE
  )
}

#' Create edge case test data
#'
#' @param case Name of the edge case
#' @return Data frame with specific edge case
create_edge_case_data <- function(case = "all_identical") {
  switch(case,
    "all_identical" = data.frame(
      id = 1:5,
      units = rep(100, 5),
      income = rep(50000, 5),
      area = rep(2, 5)
    ),

    "single_row" = data.frame(
      id = 1,
      units = 100,
      income = 50000,
      area = 2
    ),

    "two_rows" = data.frame(
      id = 1:2,
      units = c(100, 150),
      income = c(40000, 60000),
      area = c(2, 3)
    ),

    "zero_numerator" = data.frame(
      id = 1:5,
      units = rep(0, 5),
      income = sample(30000:70000, 5),
      area = runif(5, 1, 3)
    ),

    "zero_denominator" = data.frame(
      id = 1:5,
      units = sample(50:150, 5),
      income = sample(30000:70000, 5),
      area = rep(0, 5)
    ),

    "all_na_numerator" = data.frame(
      id = 1:5,
      units = rep(NA_real_, 5),
      income = sample(30000:70000, 5),
      area = runif(5, 1, 3)
    ),

    "all_na_denominator" = data.frame(
      id = 1:5,
      units = sample(50:150, 5),
      income = sample(30000:70000, 5),
      area = rep(NA_real_, 5)
    ),

    "some_na" = data.frame(
      id = 1:10,
      units = c(100, NA, 150, 200, 120, 180, NA, 160, 140, 110),
      income = c(45000, 62000, NA, 71000, 55000, 82000, 48000, NA, 59000, 74000),
      area = c(2.5, 3.1, 1.8, NA, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8)
    ),

    "negative_numerator" = data.frame(
      id = 1:5,
      units = c(-10, 100, 150, 120, 180),
      income = sample(30000:70000, 5),
      area = runif(5, 1, 3)
    ),

    "negative_denominator" = data.frame(
      id = 1:5,
      units = sample(50:150, 5),
      income = sample(30000:70000, 5),
      area = c(2, -1, 3, 2.5, 2.8)
    ),

    "tied_ranks" = data.frame(
      id = 1:10,
      units = sample(50:200, 10, replace = TRUE),
      income = c(50000, 50000, 60000, 60000, 60000, 70000, 70000, 80000, 80000, 90000),
      area = runif(10, 1, 5)
    ),

    "extreme_concentration_high" = {
      df <- create_valid_data(10)
      df$units <- c(rep(1, 9), 1000)  # All units in highest-income area
      df$income <- 1:10 * 10000
      df
    },

    "extreme_concentration_low" = {
      df <- create_valid_data(10)
      df$units <- c(1000, rep(1, 9))  # All units in lowest-income area
      df$income <- 1:10 * 10000
      df
    },

    "perfect_proportional" = {
      # Units proportional to area
      data.frame(
        id = 1:10,
        units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
        income = 1:10 * 10000,
        area = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130) / 100
      )
    },

    stop(paste("Unknown edge case:", case))
  )
}

#' Create data with missing values for confidence interval testing
#'
#' @param base_n Number of base rows
#' @param missing_amount Amount of missing data
#' @return Data frame
create_data_with_missing <- function(base_n = 10, missing_amount = 50) {
  df <- create_valid_data(base_n)
  list(
    data = df,
    missing = missing_amount
  )
}

#' Check if FHLUS score is in valid range
#'
#' @param score Numeric FHLUS score
#' @return Logical
is_valid_fhlus_score <- function(score) {
  if (is.na(score)) return(TRUE)  # NA is valid for edge cases
  score >= -1 && score <= 1
}

#' Create character column data (should fail)
create_invalid_type_data <- function() {
  data.frame(
    id = 1:5,
    units = c("100", "150", "200", "120", "180"),  # Character instead of numeric
    income = sample(30000:70000, 5),
    area = runif(5, 1, 3),
    stringsAsFactors = FALSE
  )
}

#' Create data with mixed groups (some valid, some invalid)
#' Create spatial test data for exclude_layer tests
#'
#' Returns a list with sf objects for testing spatial exclusion.
#' Requires the sf package.
#' @return List with tracts, park, no_overlap, park_different_crs
create_spatial_test_data <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required for spatial test data")
  }

  # 4 square polygons in a 2x2 grid (CRS 3857, Web Mercator)
  # Each square is 100x100 units
  make_square <- function(xmin, ymin, size = 100) {
    sf::st_polygon(list(matrix(c(
      xmin,        ymin,
      xmin + size, ymin,
      xmin + size, ymin + size,
      xmin,        ymin + size,
      xmin,        ymin
    ), ncol = 2, byrow = TRUE)))
  }

  tracts <- sf::st_sf(
    id = 1:4,
    units = c(100, 150, 200, 120),
    income = c(40000, 60000, 50000, 80000),
    zone = c("Residential", "Commercial", "Residential", "Open space"),
    geometry = sf::st_sfc(
      make_square(0, 0),      # Tract 1: bottom-left
      make_square(100, 0),    # Tract 2: bottom-right
      make_square(0, 100),    # Tract 3: top-left
      make_square(100, 100)   # Tract 4: top-right
    ),
    crs = 3857
  )

  # Park polygon overlapping parts of tracts 1 and 3 (left half of tract 1 + left half of tract 3)
  park <- sf::st_sf(
    name = "Central Park",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        0,   0,
        50,  0,
        50,  200,
        0,   200,
        0,   0
      ), ncol = 2, byrow = TRUE)))
    ),
    crs = 3857
  )

  # Polygon completely outside all tracts
  no_overlap <- sf::st_sf(
    name = "Remote Park",
    geometry = sf::st_sfc(
      make_square(500, 500, 50)
    ),
    crs = 3857
  )

  # Same park but in CRS 4326 (geographic)
  park_different_crs <- sf::st_transform(park, 4326)

  list(
    tracts = tracts,
    park = park,
    no_overlap = no_overlap,
    park_different_crs = park_different_crs
  )
}

#' Create grouped data with per-group error columns
#'
#' @return Data frame with pct_error and abs_error columns varying by group
create_grouped_data_with_errors <- function() {
  set.seed(789)
  data.frame(
    group = rep(c("Group_A", "Group_B", "Group_C"), each = 5),
    id = 1:15,
    units = sample(50:200, 15, replace = TRUE),
    income = sample(30000:90000, 15, replace = TRUE),
    area = runif(15, 1, 5),
    pct_error = rep(c(0.05, 0.10, 0.20), each = 5),
    abs_error = rep(c(10, 50, 100), each = 5),
    stringsAsFactors = FALSE
  )
}

#' Create data with mixed groups (some valid, some invalid)
create_mixed_grouped_data <- function() {
  data.frame(
    group = c(
      rep("Valid_A", 10),
      rep("Valid_B", 8),
      rep("Insufficient_C", 1),  # Only 1 row
      rep("Zero_D", 5)
    ),
    id = 1:24,
    units = c(
      sample(50:200, 10, replace = TRUE),
      sample(50:200, 8, replace = TRUE),
      100,
      rep(0, 5)  # Zero numerator for group D
    ),
    income = sample(30000:90000, 24, replace = TRUE),
    area = runif(24, 1, 5),
    stringsAsFactors = FALSE
  )
}
