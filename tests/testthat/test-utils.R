# Tests for FHLUS utility functions

test_that("validate_fhlus_data identifies missing columns", {
  df <- data.frame(x = 1:5, y = 1:5)
  validation <- validate_fhlus_data(df, "units", "income", "area", verbose = FALSE)

  expect_false(validation$valid)
  expect_true("missing_columns" %in% names(validation$issues))
  expect_setequal(validation$issues$missing_columns, c("units", "income", "area"))
})

test_that("validate_fhlus_data passes with valid data", {
  df <- create_valid_data()
  validation <- validate_fhlus_data(df, "units", "income", "area", verbose = FALSE)

  expect_true(validation$valid)
  expect_length(validation$issues, 0)
})

test_that("validate_fhlus_data warns about NA values", {
  df <- create_edge_case_data("some_na")
  validation <- validate_fhlus_data(df, "units", "income", "area", verbose = FALSE)

  expect_true("na_values" %in% names(validation$warnings))
})

test_that("validate_fhlus_data catches insufficient data", {
  df <- create_edge_case_data("single_row")
  validation <- validate_fhlus_data(df, "units", "income", "area", verbose = FALSE)

  expect_false(validation$valid)
  expect_true("insufficient_data" %in% names(validation$issues))
})

test_that("validate_fhlus_data catches negative numerator", {
  df <- create_edge_case_data("negative_numerator")
  validation <- validate_fhlus_data(df, "units", "income", "area", verbose = FALSE)

  expect_false(validation$valid)
  expect_true("negative_numerator" %in% names(validation$issues))
})

test_that("export_fhlus handles CSV format", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  temp_file <- tempfile(fileext = "")
  export_fhlus(result, temp_file, format = "csv", include_plot = FALSE)

  expect_true(file.exists(paste0(temp_file, ".csv")))
  unlink(paste0(temp_file, ".csv"))
})

test_that("export_fhlus handles RDS format", {
  df <- create_valid_data()
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  temp_file <- tempfile(fileext = "")
  export_fhlus(result, temp_file, format = "rds", include_plot = FALSE)

  expect_true(file.exists(paste0(temp_file, ".rds")))
  unlink(paste0(temp_file, ".rds"))
})

test_that("calculate_multiple_indices works with multiple indices", {
  df <- create_valid_data()
  df$poverty_rate <- runif(nrow(df), 0.05, 0.30)

  indices <- list(
    income = list(
      numerator = "units",
      rank_var = "income",
      denom = "area"
    ),
    poverty = list(
      numerator = "units",
      rank_var = "poverty_rate",
      denom = "area",
      ascending = FALSE
    )
  )

  result <- calculate_multiple_indices(df, indices)

  expect_true("long" %in% names(result))
  expect_true("wide" %in% names(result))
  expect_true("index" %in% names(result$long))
})

# --- Spatial exclusion layer tests ---

test_that("spatial exclusion layer produces excluded_area > 0 for overlapping features", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  result <- prepare_spatial_fhlus(td$tracts, exclude_layer = td$park)

  expect_true("excluded_area" %in% names(result))
  expect_true("available_area" %in% names(result))
  # Park overlaps tracts 1 and 3

  expect_true(result$excluded_area[result$id == 1] > 0)
  expect_true(result$excluded_area[result$id == 3] > 0)
  # Tracts 2 and 4 should not be affected
  expect_equal(result$excluded_area[result$id == 2], 0)
  expect_equal(result$excluded_area[result$id == 4], 0)
  # available_area should be total_area - excluded_area
  expect_equal(result$available_area, result$total_area - result$excluded_area)
})

test_that("non-overlapping exclude_layer results in zero excluded_area", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  result <- prepare_spatial_fhlus(td$tracts, exclude_layer = td$no_overlap)

  expect_true(all(result$excluded_area == 0))
  expect_equal(result$available_area, result$total_area)
})

test_that("exclude_layer with different CRS is auto-transformed", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  # park_different_crs is in CRS 4326 while tracts are in 3857
  expect_message(
    result <- prepare_spatial_fhlus(td$tracts, exclude_layer = td$park_different_crs),
    "Transforming exclude_layer"
  )
  # Should still produce valid exclusions
  expect_true(any(result$excluded_area > 0))
})

test_that("non-sf exclude_layer is rejected", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  expect_error(
    prepare_spatial_fhlus(td$tracts, exclude_layer = data.frame(x = 1)),
    "exclude_layer must be an sf object"
  )
})

test_that("combined attribute + spatial exclusion is additive", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  # Attribute-only: tract 4 has zone "Open space"
  attr_result <- prepare_spatial_fhlus(
    td$tracts, exclude_zones = "Open space", zone_column = "zone"
  )

  # Spatial-only: park overlaps tracts 1 and 3

  spatial_result <- prepare_spatial_fhlus(
    td$tracts, exclude_layer = td$park
  )

  # Combined: both applied
  combined_result <- prepare_spatial_fhlus(
    td$tracts, exclude_zones = "Open space", zone_column = "zone",
    exclude_layer = td$park
  )

  # Tract 4 should have attribute exclusion in combined
  expect_equal(
    combined_result$excluded_area[combined_result$id == 4],
    attr_result$excluded_area[attr_result$id == 4]
  )
  # Tracts 1 and 3 should have spatial exclusion in combined
  expect_equal(
    combined_result$excluded_area[combined_result$id == 1],
    spatial_result$excluded_area[spatial_result$id == 1]
  )
})

test_that("excluded_area is capped at total_area", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  # Create a huge exclusion layer that covers everything and more
  huge_park <- sf::st_sf(
    name = "Huge",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(
        -100, -100,
        300,  -100,
        300,   300,
        -100,  300,
        -100, -100
      ), ncol = 2, byrow = TRUE)))
    ),
    crs = 3857
  )

  result <- prepare_spatial_fhlus(td$tracts, exclude_layer = huge_park)

  # excluded_area should not exceed total_area

  expect_true(all(result$excluded_area <= result$total_area))
  # available_area should be 0 (fully excluded)
  expect_true(all(result$available_area == 0))
})

test_that("backward compatibility: no exclude_layer behaves same as before", {
  skip_if_not_installed("sf")
  td <- create_spatial_test_data()

  # Without exclude_layer
  result_no_layer <- prepare_spatial_fhlus(td$tracts)

  expect_true(all(result_no_layer$excluded_area == 0))
  expect_equal(result_no_layer$available_area, result_no_layer$total_area)
})
