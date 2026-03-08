#' FHLUS Utility and Spatial Functions
#'
#' @description Helper functions for FHLUS calculations including spatial operations
#' @importFrom dplyr mutate arrange bind_rows case_when sym syms filter select group_by summarise left_join
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang sym syms :=

#' Prepare spatial data for FHLUS calculation
#'
#' @param spatial_data SF object with spatial data
#' @param area_crs CRS for area calculations (default: use existing CRS, but warn if geographic)
#' @param exclude_zones Vector of zone types to exclude (e.g., "Open space")
#' @param zone_column Column name containing zone information
#' @param exclude_layer Optional SF object with polygon geometries representing areas to exclude
#'   (e.g., parks, open space). The layer is dissolved and intersected with source features to
#'   calculate actual excluded area per feature. Can be used alone or combined with attribute-based
#'   exclusion (\code{exclude_zones}/\code{zone_column}), in which case exclusions are additive.
#'
#' @return SF object with calculated total_area, excluded_area, and available_area columns
#'
#' @examples
#' \donttest{
#' # Requires sf package
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   # Create sample spatial data
#'   pts <- matrix(runif(20), ncol = 2)
#'   polys <- sf::st_sfc(sf::st_polygon(list(pts)))
#'   spatial_data <- sf::st_sf(
#'     id = 1:5,
#'     units = c(100, 150, 200, 120, 180),
#'     geometry = polys
#'   )
#'
#'   # Prepare for FHLUS
#'   prepared <- prepare_spatial_fhlus(
#'     spatial_data,
#'     area_crs = 3857
#'   )
#'
#'   # With a spatial exclusion layer (e.g., parks shapefile)
#'   # park <- sf::st_read("parks.shp")
#'   # prepared <- prepare_spatial_fhlus(
#'   #   spatial_data,
#'   #   area_crs = 3857,
#'   #   exclude_layer = park
#'   # )
#' }
#' }
#'
#' @export
prepare_spatial_fhlus <- function(spatial_data,
                                  area_crs = NULL,
                                  exclude_zones = NULL,
                                  zone_column = NULL,
                                  exclude_layer = NULL) {

  # Check if sf package is available
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for spatial operations. Install with install.packages('sf').")
  }

  # Validate input
  if (!inherits(spatial_data, "sf")) {
    stop(sprintf(
      "spatial_data must be an sf object. Got class: %s. Use sf::st_as_sf() to convert.",
      paste(class(spatial_data), collapse = ", ")
    ))
  }

  # Validate exclude_layer if provided
  if (!is.null(exclude_layer)) {
    if (!inherits(exclude_layer, "sf")) {
      stop(sprintf(
        "exclude_layer must be an sf object. Got class: %s.",
        paste(class(exclude_layer), collapse = ", ")
      ))
    }
    geom_types <- unique(as.character(sf::st_geometry_type(exclude_layer)))
    valid_types <- c("POLYGON", "MULTIPOLYGON")
    if (!all(geom_types %in% valid_types)) {
      stop(sprintf(
        "exclude_layer must contain polygon geometries (POLYGON or MULTIPOLYGON). Got: %s.",
        paste(geom_types, collapse = ", ")
      ))
    }
  }

  # Transform to appropriate CRS for area calculation if needed
  if (!is.null(area_crs)) {
    spatial_data <- sf::st_transform(spatial_data, area_crs)
  } else if (sf::st_is_longlat(spatial_data)) {
    warning("Data is in geographic CRS (lat/lon). Areas may be inaccurate. Consider providing area_crs (e.g., a local UTM zone).")
  }

  # Calculate total area
  spatial_data$total_area <- as.numeric(sf::st_area(spatial_data))

  # Align exclude_layer CRS after spatial_data has been transformed
  if (!is.null(exclude_layer)) {
    if (sf::st_crs(exclude_layer) != sf::st_crs(spatial_data)) {
      message("Transforming exclude_layer to match spatial_data CRS")
      exclude_layer <- sf::st_transform(exclude_layer, sf::st_crs(spatial_data))
    }
  }

  # Determine which exclusion approaches apply
  has_attribute_exclusion <- !is.null(exclude_zones) && !is.null(zone_column)
  has_layer_exclusion <- !is.null(exclude_layer)

  if (has_attribute_exclusion || has_layer_exclusion) {
    # Initialize excluded_area to 0
    spatial_data$excluded_area <- 0

    # Apply attribute-based exclusion if applicable
    if (has_attribute_exclusion) {
      spatial_data <- calculate_excluded_areas(spatial_data, exclude_zones, zone_column)
    }

    # Apply spatial layer exclusion if applicable (additive)
    if (has_layer_exclusion) {
      spatial_data <- calculate_excluded_areas_spatial(spatial_data, exclude_layer)
    }

    # Recalculate available_area, capped at 0
    spatial_data$available_area <- pmax(spatial_data$total_area - spatial_data$excluded_area, 0)
  } else {
    # No exclusions
    spatial_data$excluded_area <- 0
    spatial_data$available_area <- spatial_data$total_area
  }

  return(spatial_data)
}

#' Calculate areas excluding specific zones (attribute-based)
#' @noRd
calculate_excluded_areas <- function(spatial_data, exclude_zones, zone_column) {

  if (!zone_column %in% names(spatial_data)) {
    warning(sprintf(
      "Zone column '%s' not found. Available columns: %s. Skipping exclusion.",
      zone_column,
      paste(names(spatial_data), collapse = ", ")
    ))
    return(spatial_data)
  }

  # Identify excluded areas
  excluded_mask <- spatial_data[[zone_column]] %in% exclude_zones

  if (any(excluded_mask)) {
    message(paste("Marking", sum(excluded_mask), "features as excluded zones"))
    spatial_data$excluded_area[excluded_mask] <-
      spatial_data$excluded_area[excluded_mask] + spatial_data$total_area[excluded_mask]
  }

  return(spatial_data)
}

#' Calculate excluded areas using a spatial exclusion layer
#'
#' Dissolves the exclusion layer, intersects with source features, and calculates
#' actual excluded area per feature. Adds to existing excluded_area (supports
#' combining with attribute-based exclusion).
#' @noRd
calculate_excluded_areas_spatial <- function(spatial_data, exclude_layer) {

  # Validate & repair geometries
  spatial_data <- sf::st_make_valid(spatial_data)
  exclude_layer <- sf::st_make_valid(exclude_layer)

  # Add temp row ID for reliable join-back
  spatial_data$.__fhlus_id <- seq_len(nrow(spatial_data))

  # Dissolve exclusion layer into a single geometry
  dissolved <- sf::st_union(exclude_layer)
  dissolved_sf <- sf::st_sf(.__exclude = 1L, geometry = dissolved)

  # Intersect source features with dissolved exclusion layer
  intersection <- tryCatch(
    suppressWarnings(sf::st_intersection(spatial_data, dissolved_sf)),
    error = function(e) {
      # Fallback: buffer by 0 to fix topology issues, then retry
      message("Retrying intersection after geometry repair...")
      spatial_data_fixed <- sf::st_buffer(spatial_data, 0)
      dissolved_fixed <- sf::st_buffer(dissolved_sf, 0)
      suppressWarnings(sf::st_intersection(spatial_data_fixed, dissolved_fixed))
    }
  )

  # Handle empty intersection (no overlap)
  if (nrow(intersection) == 0) {
    message("Exclusion layer does not overlap any source features. No spatial exclusion applied.")
    spatial_data$.__fhlus_id <- NULL
    return(spatial_data)
  }

  # Calculate intersected area
  intersection$.__intersect_area <- as.numeric(sf::st_area(intersection))

  # Aggregate by source feature ID
  intersect_summary <- intersection %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(.__fhlus_id) %>%
    dplyr::summarise(.__spatial_excluded = sum(.__intersect_area, na.rm = TRUE),
                     .groups = "drop")

  # Join back to source data
  spatial_data <- dplyr::left_join(
    spatial_data,
    intersect_summary,
    by = ".__fhlus_id"
  )
  spatial_data$.__spatial_excluded[is.na(spatial_data$.__spatial_excluded)] <- 0

  # Add spatial exclusion to excluded_area (additive with attribute-based)
  spatial_data$excluded_area <- spatial_data$excluded_area + spatial_data$.__spatial_excluded

  # Cap excluded_area at total_area
  spatial_data$excluded_area <- pmin(spatial_data$excluded_area, spatial_data$total_area)

  # Report
  n_affected <- sum(spatial_data$.__spatial_excluded > 0)
  total_excluded <- sum(spatial_data$.__spatial_excluded)
  message(sprintf(
    "Spatial exclusion: %d features affected, %.1f total area excluded",
    n_affected, total_excluded
  ))

  # Clean up temp columns
  spatial_data$.__fhlus_id <- NULL
  spatial_data$.__spatial_excluded <- NULL

  return(spatial_data)
}

#' Aggregate spatial units to neighborhoods
#'
#' @param units_data SF object with unit-level data (e.g., individual buildings)
#' @param neighborhoods SF object with neighborhood boundaries
#' @param unit_var Column name for units variable to aggregate
#' @param neighborhood_id Column name for neighborhood identifier
#'
#' @return SF object (neighborhoods) with aggregated units and site counts
#'
#' @examples
#' \donttest{
#' # Requires sf package
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   # This example would require actual spatial data
#'   # See package vignettes for complete examples
#' }
#' }
#'
#' @export
aggregate_to_neighborhoods <- function(units_data,
                                       neighborhoods,
                                       unit_var,
                                       neighborhood_id) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for spatial operations. Install with install.packages('sf').")
  }

  # Ensure both are sf objects
  if (!inherits(units_data, "sf") || !inherits(neighborhoods, "sf")) {
    stop("Both units_data and neighborhoods must be sf objects.")
  }

  # Ensure same CRS
  if (sf::st_crs(units_data) != sf::st_crs(neighborhoods)) {
    message("Transforming units_data to match neighborhoods CRS")
    units_data <- sf::st_transform(units_data, sf::st_crs(neighborhoods))
  }

  # Perform spatial join
  joined <- sf::st_join(units_data, neighborhoods[neighborhood_id])

  # Aggregate by neighborhood
  aggregated <- joined %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!rlang::sym(neighborhood_id)) %>%
    dplyr::summarise(
      total_units = sum(!!rlang::sym(unit_var), na.rm = TRUE),
      n_sites = dplyr::n(),
      .groups = "drop"
    )

  # Join back to neighborhoods to get all neighborhoods (even those with 0 units)
  result <- neighborhoods %>%
    dplyr::left_join(aggregated, by = neighborhood_id) %>%
    dplyr::mutate(
      total_units = tidyr::replace_na(total_units, 0),
      n_sites = tidyr::replace_na(n_sites, 0)
    )

  return(result)
}

#' Impute missing values using spatial neighbors
#'
#' @param spatial_data SF object with data
#' @param impute_var Variable to impute
#' @param method Method for imputation ("mean", "median", "idw")
#' @param k Number of neighbors to consider
#'
#' @return SF object with imputed values
#'
#' @examples
#' \donttest{
#' # Requires sf and spdep packages
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("spdep", quietly = TRUE)) {
#'   # This example would require actual spatial data with missing values
#'   # See package vignettes for complete examples
#' }
#' }
#'
#' @export
spatial_impute <- function(spatial_data,
                           impute_var,
                           method = "mean",
                           k = 5) {

  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' required for spatial imputation. Install with install.packages('spdep').")
  }

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for spatial operations. Install with install.packages('sf').")
  }

  # Validate impute_var exists
  if (!impute_var %in% names(spatial_data)) {
    stop(sprintf(
      "Variable '%s' not found in spatial_data. Available columns: %s",
      impute_var,
      paste(names(spatial_data), collapse = ", ")
    ))
  }

  # Identify missing values
  missing_idx <- which(is.na(spatial_data[[impute_var]]))

  if (length(missing_idx) == 0) {
    message("No missing values to impute")
    return(spatial_data)
  }

  message(paste("Imputing", length(missing_idx), "missing values using", method, "method"))

  # Create spatial weights matrix
  coords <- sf::st_coordinates(sf::st_centroid(spatial_data, of_largest_polygon = TRUE))

  if (method == "idw") {
    # Inverse distance weighting
    spatial_data[[impute_var]] <- idw_impute(spatial_data, impute_var, coords, missing_idx, k)
  } else {
    # Neighbor-based imputation
    nb <- spdep::knn2nb(spdep::knearneigh(coords, k = k))

    for (i in missing_idx) {
      neighbors <- nb[[i]]
      neighbor_values <- spatial_data[[impute_var]][neighbors]
      neighbor_values <- neighbor_values[!is.na(neighbor_values)]

      if (length(neighbor_values) > 0) {
        if (method == "mean") {
          spatial_data[[impute_var]][i] <- mean(neighbor_values)
        } else if (method == "median") {
          spatial_data[[impute_var]][i] <- stats::median(neighbor_values)
        }
      }
    }
  }

  # Report results
  still_missing <- sum(is.na(spatial_data[[impute_var]]))
  if (still_missing > 0) {
    warning(paste(still_missing, "values could not be imputed (isolated features with no valid neighbors)"))
  } else {
    message("All missing values successfully imputed")
  }

  return(spatial_data)
}

#' IDW imputation helper
#' @noRd
idw_impute <- function(data, var, coords, missing_idx, k) {

  for (i in missing_idx) {
    # Calculate distances to all other points
    distances <- sqrt((coords[, 1] - coords[i, 1])^2 +
                        (coords[, 2] - coords[i, 2])^2)

    # Exclude self and missing values
    valid_idx <- which(!is.na(data[[var]]) & distances > 0)

    if (length(valid_idx) >= 1) {
      # Get k nearest valid neighbors
      k_use <- min(k, length(valid_idx))
      nearest <- valid_idx[order(distances[valid_idx])[1:k_use]]

      # Calculate weights (inverse distance)
      weights <- 1 / distances[nearest]
      weights <- weights / sum(weights)

      # Impute value
      data[[var]][i] <- sum(data[[var]][nearest] * weights)
    }
  }

  return(data[[var]])
}

#' Calculate multiple opportunity indices
#'
#' @param data Data frame with variables
#' @param indices Named list of index specifications. Each element should be a list with:
#'   numerator, rank_var, denom, and optionally missing, missing_pct, exclude_area, tie_breaker, ascending
#' @param group_var Optional grouping variable for grouped FHLUS calculations
#'
#' @return List with long-format and wide-format results
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   tract_id = 1:10,
#'   housing_units = sample(50:200, 10, replace = TRUE),
#'   median_income = sample(30000:90000, 10, replace = TRUE),
#'   poverty_rate = runif(10, 0.05, 0.30),
#'   land_area = runif(10, 1, 5)
#' )
#'
#' # Define multiple indices
#' indices <- list(
#'   income = list(
#'     numerator = "housing_units",
#'     rank_var = "median_income",
#'     denom = "land_area"
#'   ),
#'   poverty = list(
#'     numerator = "housing_units",
#'     rank_var = "poverty_rate",
#'     denom = "land_area",
#'     ascending = FALSE  # Lower poverty is better
#'   )
#' )
#'
#' # Calculate multiple indices
#' results <- calculate_multiple_indices(sample_data, indices)
#' print(results$long)
#'
#' @export
calculate_multiple_indices <- function(data,
                                       indices,
                                       group_var = NULL) {

  if (!is.list(indices) || is.null(names(indices))) {
    stop("indices must be a named list of index specifications")
  }

  results <- list()

  for (index_name in names(indices)) {
    index_spec <- indices[[index_name]]

    message(paste("Calculating FHLUS for", index_name))

    # Extract parameters
    numerator <- index_spec$numerator
    rank_var <- index_spec$rank_var
    denom <- index_spec$denom

    # Validate required parameters
    if (is.null(numerator) || is.null(rank_var) || is.null(denom)) {
      stop(sprintf(
        "Index '%s' missing required parameters. Need: numerator, rank_var, denom",
        index_name
      ))
    }

    # Additional parameters with defaults
    missing <- index_spec$missing %||% NULL
    missing_pct <- index_spec$missing_pct %||% NULL
    exclude_area <- index_spec$exclude_area %||% NULL
    tie_breaker <- index_spec$tie_breaker %||% NULL
    ascending <- index_spec$ascending %||% TRUE

    # Reverse ranking if needed (lower is better)
    if (!ascending) {
      data <- dplyr::mutate(
        data,
        !!paste0(rank_var, "_reversed") := -!!rlang::sym(rank_var)
      )
      rank_var <- paste0(rank_var, "_reversed")
    }

    # Calculate FHLUS
    if (!is.null(group_var)) {
      result <- grouped_fhlus(
        df = data,
        group_var = group_var,
        numerator = numerator,
        rank_var = rank_var,
        denom = denom,
        missing = missing,
        missing_pct = missing_pct,
        exclude_area = exclude_area,
        tie_breaker = tie_breaker,
        graph = FALSE,
        verbose = FALSE
      )
    } else {
      result <- fhlus_score(
        df = data,
        numerator = numerator,
        rank_var = rank_var,
        denom = denom,
        missing = missing,
        missing_pct = missing_pct,
        exclude_area = exclude_area,
        tie_breaker = tie_breaker,
        graph = FALSE,
        verbose = FALSE
      )
      result <- result$scores
    }

    # Add index name
    result$index <- index_name
    results[[index_name]] <- result
  }

  # Combine results
  combined <- dplyr::bind_rows(results)

  # Reshape if requested
  if (length(indices) > 1) {
    reshaped <- reshape_multiple_indices(combined, group_var)
    return(reshaped)
  }

  return(list(long = combined, wide = combined))
}

#' Reshape multiple indices results
#' @noRd
reshape_multiple_indices <- function(combined, group_var) {

  # Create wide format for easier comparison
  if (!is.null(group_var)) {
    # Grouped input is already wide (score, score_lowest, score_highest per row)
    # Pivot the score columns across indices
    score_cols <- intersect(c("score", "score_lowest", "score_highest"), names(combined))

    wide_result <- combined %>%
      dplyr::select(!!rlang::sym(group_var), index, dplyr::all_of(score_cols)) %>%
      tidyr::pivot_wider(
        names_from = index,
        values_from = dplyr::all_of(score_cols),
        names_glue = "{.value}_{index}"
      )

  } else {
    # Non-grouped: still long format from fhlus_score (type column)
    wide_result <- combined %>%
      tidyr::pivot_wider(names_from = c(index, type),
                  values_from = score,
                  names_prefix = "score_",
                  names_sep = "_")
  }

  return(list(
    long = combined,
    wide = wide_result
  ))
}

#' Default null operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Validate FHLUS data requirements
#'
#' @param data Data frame to validate
#' @param numerator Numerator column name
#' @param rank_var Ranking variable(s)
#' @param denom Denominator column name
#' @param verbose Print detailed validation messages
#'
#' @return List with validation results including:
#'   \item{valid}{Logical, whether data passes validation}
#'   \item{issues}{List of critical issues that prevent calculation}
#'   \item{warnings}{List of warnings about data quality}
#'   \item{n_rows}{Number of rows in data}
#'   \item{n_cols}{Number of columns in data}
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   id = 1:5,
#'   units = c(100, 150, 200, NA, 180),
#'   income = c(45000, 62000, 38000, 71000, 55000),
#'   area = c(2.5, 3.1, 1.8, 2.9, 3.5)
#' )
#'
#' # Validate data
#' validation <- validate_fhlus_data(
#'   sample_data,
#'   numerator = "units",
#'   rank_var = "income",
#'   denom = "area"
#' )
#'
#' if (!validation$valid) {
#'   print(validation$issues)
#' }
#'
#' @export
validate_fhlus_data <- function(data,
                                numerator,
                                rank_var,
                                denom,
                                verbose = TRUE) {

  issues <- list()
  warnings <- list()

  # Check if data is a data frame
  if (!is.data.frame(data)) {
    issues$not_dataframe <- paste("Input is not a data frame. Got:", class(data)[1])
    # Early return if not a data frame
    return(list(
      valid = FALSE,
      issues = issues,
      warnings = warnings,
      n_rows = 0,
      n_cols = 0
    ))
  }

  # Check column existence
  required_cols <- c(numerator, denom, rank_var)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    issues$missing_columns <- missing_cols
  }

  # Check for NA values
  for (col in intersect(required_cols, names(data))) {
    na_count <- sum(is.na(data[[col]]))
    if (na_count > 0) {
      warnings$na_values[[col]] <- na_count
    }
  }

  # Check for zero or negative denominators
  if (denom %in% names(data)) {
    zero_denom <- sum(data[[denom]] <= 0, na.rm = TRUE)
    if (zero_denom > 0) {
      warnings$zero_or_negative_denominator <- zero_denom
    }

    # Check for negative values
    neg_denom <- sum(data[[denom]] < 0, na.rm = TRUE)
    if (neg_denom > 0) {
      warnings$negative_denominator <- neg_denom
    }
  }

  # Check for negative numerator
  if (numerator %in% names(data)) {
    neg_num <- sum(data[[numerator]] < 0, na.rm = TRUE)
    if (neg_num > 0) {
      issues$negative_numerator <- neg_num
    }
  }

  # Check for ties in ranking variable
  if (all(rank_var %in% names(data))) {
    if (length(rank_var) == 1) {
      n_unique <- length(unique(data[[rank_var]]))
      n_total <- nrow(data)
      if (n_unique < n_total) {
        warnings$ties_in_ranking <- n_total - n_unique
      }
    }
  }

  # Check data size
  if (nrow(data) < 2) {
    issues$insufficient_data <- paste("Only", nrow(data), "rows. FHLUS requires at least 2 rows.")
  }

  # Compile validation result
  is_valid <- length(issues) == 0

  result <- list(
    valid = is_valid,
    issues = issues,
    warnings = warnings,
    n_rows = nrow(data),
    n_cols = ncol(data)
  )

  if (verbose) {
    print_validation_results(result)
  }

  return(result)
}

#' Print validation results
#' @noRd
print_validation_results <- function(result) {
  if (result$valid) {
    cat("\u2713 Data validation passed\n")
  } else {
    cat("\u2717 Data validation failed\n")
    cat("\nIssues found:\n")
    for (issue in names(result$issues)) {
      if (issue == "missing_columns") {
        cat("  - Missing columns:", paste(result$issues[[issue]], collapse = ", "), "\n")
      } else {
        cat("  -", issue, ":", result$issues[[issue]], "\n")
      }
    }
  }

  if (length(result$warnings) > 0) {
    cat("\nWarnings:\n")
    for (warning in names(result$warnings)) {
      if (warning == "na_values") {
        for (col in names(result$warnings[[warning]])) {
          cat("  -", col, "has", result$warnings[[warning]][[col]], "NA values\n")
        }
      } else {
        cat("  -", warning, ":", result$warnings[[warning]], "\n")
      }
    }
  }

  cat("\nData dimensions:", result$n_rows, "rows x", result$n_cols, "columns\n")
}

#' Export FHLUS results to various formats
#'
#' @param results FHLUS results object (from fhlus_score or grouped_fhlus)
#' @param filename Output filename (without extension)
#' @param format Output format ("csv", "xlsx", "rds", "json")
#' @param include_plot Whether to save plot as PNG as well
#'
#' @examples
#' \dontrun{
#' # Calculate FHLUS
#' result <- fhlus_score(
#'   df = sample_data,
#'   numerator = "units",
#'   rank_var = "income",
#'   denom = "area",
#'   graph = FALSE
#' )
#'
#' # Export to CSV
#' export_fhlus(result, "my_fhlus_results", format = "csv")
#'
#' # Export to Excel with plot
#' export_fhlus(result, "my_fhlus_results", format = "xlsx", include_plot = TRUE)
#' }
#'
#' @export
export_fhlus <- function(results,
                         filename,
                         format = "csv",
                         include_plot = TRUE) {

  # Prepare data for export
  if (inherits(results, "fhlus")) {
    export_data <- results$scores
    plot_obj <- results$plot
  } else if (inherits(results, "grouped_fhlus")) {
    export_data <- results
    plot_obj <- NULL
  } else if (is.data.frame(results)) {
    export_data <- results
    plot_obj <- NULL
  } else {
    stop(sprintf(
      "results must be a fhlus object, grouped_fhlus object, or data frame. Got: %s",
      paste(class(results), collapse = ", ")
    ))
  }

  # Export data
  if (format == "csv") {
    utils::write.csv(export_data, paste0(filename, ".csv"), row.names = FALSE)
    message(paste("Data exported to", paste0(filename, ".csv")))

  } else if (format == "xlsx") {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("Package 'openxlsx' required for Excel export. Install with install.packages('openxlsx').")
    }
    openxlsx::write.xlsx(export_data, paste0(filename, ".xlsx"))
    message(paste("Data exported to", paste0(filename, ".xlsx")))

  } else if (format == "rds") {
    saveRDS(results, paste0(filename, ".rds"))
    message(paste("Results exported to", paste0(filename, ".rds")))

  } else if (format == "json") {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package 'jsonlite' required for JSON export. Install with install.packages('jsonlite').")
    }
    jsonlite::write_json(export_data, paste0(filename, ".json"), pretty = TRUE)
    message(paste("Data exported to", paste0(filename, ".json")))

  } else {
    stop(sprintf(
      "Unknown format: '%s'. Supported formats: csv, xlsx, rds, json",
      format
    ))
  }

  # Export plot if available and requested
  if (include_plot && !is.null(plot_obj)) {
    ggplot2::ggsave(paste0(filename, "_plot.png"),
                    plot = plot_obj,
                    width = 8,
                    height = 6,
                    dpi = 300)
    message(paste("Plot exported to", paste0(filename, "_plot.png")))
  }

  invisible(NULL)
}
