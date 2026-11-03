#' FHLUS Utility and Spatial Functions
#'
#' @description Helper functions for FHLUS calculations including spatial operations
#' @importFrom dplyr mutate arrange bind_rows case_when sym syms filter select group_by summarise left_join
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang sym syms :=

#' Prepare spatial data for FHLUS calculation
#'
#' Runs the spatial steps needed before \code{fhlus_score()} in a fixed order,
#' so the denominator and numerator are built consistently:
#' \enumerate{
#'   \item reproject everything to \code{area_crs};
#'   \item clip the neighborhoods to a jurisdictional \code{boundary} (optional);
#'   \item calculate \code{total_area}, \code{excluded_area} and \code{available_area};
#'   \item sum a spatial \code{numerator} layer (points or polygons) into the
#'     neighborhoods (optional).
#' }
#' The neighborhoods are always the destination of the numerator join: every
#' neighborhood is kept, with 0 where no numerator features fall.
#'
#' @param spatial_data SF object with the neighborhood polygons (e.g., tracts)
#'   and their attributes (rank variables etc.)
#' @param area_crs CRS for area calculations (default: use existing CRS, but warn if geographic)
#' @param exclude_zones Vector of zone types to exclude (e.g., "Open space")
#' @param zone_column Column name containing zone information
#' @param exclude_layer Optional SF object with polygon geometries representing areas to exclude
#'   (e.g., parks, open space). The layer is dissolved and intersected with source features to
#'   calculate actual excluded area per feature. Can be used alone or combined with attribute-based
#'   exclusion (\code{exclude_zones}/\code{zone_column}), in which case exclusions are additive.
#' @param boundary Optional SF polygon layer (e.g., a city boundary). Neighborhoods
#'   are clipped to it with \code{\link{clip_to_boundary}()}, so areas only count
#'   the part inside the boundary. See \code{boundary_id} for several jurisdictions.
#' @param boundary_id Optional column in \code{boundary} naming each jurisdiction.
#'   When given, each jurisdiction is clipped separately and the result is stacked,
#'   with this column identifying the jurisdiction (a neighborhood crossing two
#'   jurisdictions appears once in each). Pass it as \code{group_var} to
#'   \code{\link{grouped_fhlus}()}.
#' @param min_share Neighborhood pieces covering less than this share of the
#'   original neighborhood's area are dropped as slivers (default 0.01 = 1\%).
#'   Use 0 to keep every piece.
#' @param numerator Optional SF layer of numerator features (points, or polygons
#'   such as parcels) to sum into the neighborhoods with
#'   \code{\link{assign_to_neighborhoods}()}.
#' @param numerator_vars Columns of \code{numerator} to sum. A named vector
#'   renames the output (e.g., \code{c(total_units = "units")}). If \code{NULL},
#'   only the feature count is added.
#' @param numerator_method How polygon numerators are assigned:
#'   \code{"point_on_surface"} (default) or \code{"area_weighted"}. See
#'   \code{\link{assign_to_neighborhoods}()}.
#' @param count_col Name of the column holding the number of numerator features
#'   per neighborhood (default \code{"n_features"}).
#' @param outside What to do when numerator features inside the boundary fall
#'   outside every neighborhood: \code{"warn"} (default), \code{"error"} or
#'   \code{"ignore"}. Features outside the \code{boundary} itself are expected
#'   and only reported in a message.
#' @param verbose Print progress messages for each step (default \code{TRUE}).
#'
#' @return SF object with calculated total_area, excluded_area, and available_area
#'   columns. With a \code{boundary}, also \code{original_area} and
#'   \code{share_in_boundary} (and the \code{boundary_id} column). With a
#'   \code{numerator}, also the summed \code{numerator_vars} and \code{count_col}.
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
#'
#'   # Clip tracts to a city and sum project points into them
#'   # prepared <- prepare_spatial_fhlus(
#'   #   tracts,
#'   #   area_crs = 2229,
#'   #   boundary = city,
#'   #   numerator = projects,
#'   #   numerator_vars = c(units = "net_units")
#'   # )
#'   # fhlus_score(sf::st_drop_geometry(prepared), "units", "median_income",
#'   #             "available_area")
#' }
#' }
#'
#' @export
prepare_spatial_fhlus <- function(spatial_data,
                                  area_crs = NULL,
                                  exclude_zones = NULL,
                                  zone_column = NULL,
                                  exclude_layer = NULL,
                                  boundary = NULL,
                                  boundary_id = NULL,
                                  min_share = 0.01,
                                  numerator = NULL,
                                  numerator_vars = NULL,
                                  numerator_method = c("point_on_surface", "area_weighted"),
                                  count_col = "n_features",
                                  outside = c("warn", "error", "ignore"),
                                  verbose = TRUE) {

  # Check if sf package is available
  check_sf()

  # Validate input
  if (!inherits(spatial_data, "sf")) {
    stop(sprintf(
      "spatial_data must be an sf object. Got class: %s. Use sf::st_as_sf() to convert.",
      paste(class(spatial_data), collapse = ", ")
    ))
  }

  # Validate exclude_layer if provided
  if (!is.null(exclude_layer)) {
    check_polygon_layer(exclude_layer, "exclude_layer")
  }
  if (!is.null(boundary)) {
    check_polygon_layer(boundary, "boundary")
  } else if (!is.null(boundary_id)) {
    stop("boundary_id was given without a boundary.")
  }
  if (!is.null(numerator)) {
    numerator_method <- match.arg(numerator_method)
    outside <- match.arg(outside)
    check_numerator_layer(numerator, numerator_vars)
  }

  # Messages from the helpers follow verbose
  quietly <- function(expr) if (verbose) expr else suppressMessages(expr)

  n_steps <- 2L + (!is.null(boundary)) + (!is.null(numerator))
  progress <- spatial_progress(n_steps, verbose)

  warn_spatial_workload(spatial_data, area_crs, boundary, exclude_layer,
                        numerator, numerator_method)

  # Transform to appropriate CRS for area calculation if needed
  progress$step(sprintf("Preparing %s neighborhoods%s",
                        format_count(nrow(spatial_data)),
                        if (!is.null(area_crs)) " (transforming CRS)" else ""))
  if (!is.null(area_crs)) {
    spatial_data <- sf::st_transform(spatial_data, area_crs)
  } else if (sf::st_is_longlat(spatial_data)) {
    warning("Data is in geographic CRS (lat/lon). Areas may be inaccurate. Consider providing area_crs (e.g., a local UTM zone).")
  }

  # Clip to the jurisdictional boundary
  if (!is.null(boundary)) {
    progress$step(sprintf(
      "Clipping %s neighborhoods to the boundary%s",
      format_count(nrow(spatial_data)),
      if (!is.null(boundary_id)) {
        sprintf(" (%d jurisdictions)", length(unique(boundary[[boundary_id]])))
      } else ""
    ))
    spatial_data <- quietly(clip_to_boundary(
      spatial_data, boundary,
      boundary_id = boundary_id, min_share = min_share, verbose = verbose
    ))
  }

  # Calculate total area
  progress$step("Calculating areas and exclusions")
  spatial_data$total_area <- as.numeric(sf::st_area(spatial_data))

  # Align exclude_layer CRS after spatial_data has been transformed
  if (!is.null(exclude_layer)) {
    if (sf::st_crs(exclude_layer) != sf::st_crs(spatial_data)) {
      quietly(message("Transforming exclude_layer to match spatial_data CRS"))
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
      spatial_data <- quietly(calculate_excluded_areas(spatial_data, exclude_zones, zone_column))
    }

    # Apply spatial layer exclusion if applicable (additive)
    if (has_layer_exclusion) {
      spatial_data <- quietly(calculate_excluded_areas_spatial(spatial_data, exclude_layer))
    }

    # Recalculate available_area, capped at 0
    spatial_data$available_area <- pmax(spatial_data$total_area - spatial_data$excluded_area, 0)
  } else {
    # No exclusions
    spatial_data$excluded_area <- 0
    spatial_data$available_area <- spatial_data$total_area
  }

  # Sum the numerator into the (clipped) neighborhoods
  if (!is.null(numerator)) {
    progress$step(sprintf("Assigning %s numerator features to neighborhoods",
                          format_count(nrow(numerator))))

    if (!is.null(boundary)) {
      # Features outside the boundary are expected; drop them up front so
      # only genuine gaps trigger the outside warning
      numerator <- quietly(drop_outside_boundary(numerator, boundary,
                                                 sf::st_crs(spatial_data),
                                                 numerator_method))
    }

    spatial_data <- quietly(assign_to_neighborhoods(
      numerator, spatial_data,
      vars = numerator_vars, method = numerator_method,
      count_col = count_col, outside = outside, verbose = verbose
    ))
  }

  progress$done()

  return(spatial_data)
}

#' Clip neighborhoods to a jurisdictional boundary
#'
#' Intersects neighborhood polygons (e.g., tracts) with a boundary (e.g., a city)
#' and keeps only the part of each neighborhood inside it, so areas calculated
#' afterwards count only land within the jurisdiction. Attributes such as rank
#' variables are kept at their original (whole-neighborhood) values.
#'
#' @param neighborhoods SF polygon layer (e.g., tracts).
#' @param boundary SF polygon layer. Without \code{boundary_id} it is dissolved
#'   into a single area.
#' @param boundary_id Optional column in \code{boundary} naming each
#'   jurisdiction. Each jurisdiction is dissolved and clipped separately, and the
#'   pieces are stacked with this column added; a neighborhood that crosses two
#'   jurisdictions appears once in each.
#' @param min_share Pieces covering less than this share of the original
#'   neighborhood's area are dropped as slivers (default 0.01). Use 0 to keep all.
#' @param verbose Print a summary of what was clipped (default \code{TRUE}).
#'
#' @return SF object with one row per neighborhood piece inside the boundary,
#'   and new columns \code{original_area} (area before clipping) and
#'   \code{share_in_boundary} (clipped area / original area), plus
#'   \code{boundary_id} when given. Areas are in the units of the CRS, so use a
#'   projected CRS.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   sq <- function(x, y) sf::st_polygon(list(rbind(
#'     c(x, y), c(x + 1, y), c(x + 1, y + 1), c(x, y + 1), c(x, y))))
#'   tracts <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sq(0, 0), sq(1, 0)),
#'                       crs = 3857)
#'   city <- sf::st_sf(name = "City",
#'                     geometry = sf::st_sfc(sf::st_buffer(sf::st_point(c(1, 0.5)), 0.6)),
#'                     crs = 3857)
#'   clip_to_boundary(tracts, city)
#' }
#' }
#'
#' @export
clip_to_boundary <- function(neighborhoods,
                             boundary,
                             boundary_id = NULL,
                             min_share = 0.01,
                             verbose = TRUE) {

  check_sf()
  check_polygon_layer(neighborhoods, "neighborhoods")
  check_polygon_layer(boundary, "boundary")

  if (!is.numeric(min_share) || length(min_share) != 1 || is.na(min_share) ||
      min_share < 0 || min_share >= 1) {
    stop("min_share must be a single number between 0 and 1 (e.g., 0.01).")
  }

  new_cols <- c("original_area", "share_in_boundary", boundary_id)
  clash <- intersect(new_cols, names(neighborhoods))
  if (length(clash) > 0) {
    stop(sprintf(
      "neighborhoods already has column(s) %s, which clip_to_boundary() would add. Rename or drop them first.",
      paste0("'", clash, "'", collapse = ", ")
    ))
  }
  if (!is.null(boundary_id) && !boundary_id %in% names(boundary)) {
    stop(sprintf(
      "boundary_id '%s' not found in boundary. Available columns: %s",
      boundary_id, paste(setdiff(names(boundary), attr(boundary, "sf_column")), collapse = ", ")
    ))
  }

  if (sf::st_crs(boundary) != sf::st_crs(neighborhoods)) {
    message("Transforming boundary to match neighborhoods CRS")
    boundary <- sf::st_transform(boundary, sf::st_crs(neighborhoods))
  }
  if (isTRUE(sf::st_is_longlat(neighborhoods))) {
    warning("neighborhoods are in a geographic CRS (lat/lon); clipped areas use spherical geometry and may be slow. Consider a projected CRS.",
            call. = FALSE)
  }

  neighborhoods <- sf::st_make_valid(neighborhoods)
  boundary <- sf::st_make_valid(boundary)

  neighborhoods$.__fhlus_id <- seq_len(nrow(neighborhoods))
  neighborhoods$original_area <- as.numeric(sf::st_area(neighborhoods))

  # Dissolve the boundary (per jurisdiction if boundary_id is given)
  dissolved <- dissolve_boundary(boundary, boundary_id)

  pieces <- suppressWarnings(sf::st_intersection(neighborhoods, dissolved))
  pieces <- keep_polygon_pieces(pieces, c(".__fhlus_id", boundary_id))

  if (nrow(pieces) > 0) {
    pieces$share_in_boundary <- pmin(
      as.numeric(sf::st_area(pieces)) / pieces$original_area, 1
    )
    pieces$share_in_boundary[!is.finite(pieces$share_in_boundary)] <- 0
  } else {
    pieces$share_in_boundary <- numeric(0)
  }

  # Drop slivers
  slivers <- pieces$share_in_boundary < min_share
  n_slivers <- sum(slivers)
  pieces <- pieces[!slivers, ]

  if (nrow(pieces) == 0) {
    warning("No neighborhoods remain inside the boundary. Check that both layers cover the same place and CRS.",
            call. = FALSE)
  }

  if (verbose) {
    n_kept <- length(unique(pieces$.__fhlus_id))
    n_partial <- sum(pieces$share_in_boundary < 1 - 1e-6)
    message(sprintf(
      "Clipped to boundary: %s of %s neighborhoods kept as %s piece(s), %s of them clipped at the boundary%s",
      format_count(n_kept), format_count(nrow(neighborhoods)), format_count(nrow(pieces)),
      format_count(n_partial),
      if (n_slivers > 0) {
        sprintf("; dropped %s sliver(s) under %s of their area", format_count(n_slivers),
                paste0(format(min_share * 100), "%"))
      } else ""
    ))
    if (!is.null(boundary_id)) {
      n_split <- sum(table(pieces$.__fhlus_id) > 1)
      if (n_split > 0) {
        message(sprintf("%s neighborhood(s) are split between jurisdictions",
                        format_count(n_split)))
      }
    }
  }

  # Original order, temp columns removed, multipolygon geometry
  pieces <- pieces[order(pieces$.__fhlus_id), ]
  pieces$.__fhlus_id <- NULL
  pieces$.__fhlus_bnd <- NULL
  pieces <- sf::st_cast(pieces, "MULTIPOLYGON", warn = FALSE)
  rownames(pieces) <- NULL

  geometry_last(pieces)
}

#' Sum numerator features (points or polygons) into neighborhoods
#'
#' Assigns each numerator feature (e.g., a housing project point or a parcel) to
#' the neighborhood it falls in and sums the chosen variables per neighborhood.
#' The neighborhoods are the destination: every neighborhood is returned, with 0
#' where no features fall, so none are lost (the common mistake is joining
#' neighborhoods onto the points, which drops neighborhoods with no features).
#'
#' \strong{Points} are assigned to exactly one neighborhood. A point on a shared
#' edge goes to the first matching neighborhood, so it is never counted twice.
#'
#' \strong{Polygons} are assigned with \code{method}:
#' \describe{
#'   \item{\code{"point_on_surface"}}{(default) each polygon goes wholly to the
#'     neighborhood containing \code{sf::st_point_on_surface()} of it. Best when
#'     the polygons (e.g., parcels) are much smaller than the neighborhoods.}
#'   \item{\code{"area_weighted"}}{each polygon's values are split between the
#'     neighborhoods it overlaps, in proportion to the overlapping area. Use it
#'     for large sites or zones that cross neighborhood lines. Values are assumed
#'     to be spread evenly over each polygon.}
#' }
#'
#' Neighborhoods should not overlap each other. The result reports how much of
#' each variable was assigned, and features that fall outside every
#' neighborhood are handled according to \code{outside}.
#'
#' @param numerator SF layer of POINT/MULTIPOINT or POLYGON/MULTIPOLYGON features.
#' @param neighborhoods SF polygon layer (e.g., tracts, possibly clipped with
#'   \code{\link{clip_to_boundary}()}).
#' @param vars Numeric columns of \code{numerator} to sum. A named vector
#'   renames the output (e.g., \code{c(total_units = "units")}). If \code{NULL},
#'   only the feature count is added. NA values count as 0 (with a warning).
#' @param method \code{"point_on_surface"} or \code{"area_weighted"}; only
#'   affects polygon numerators.
#' @param count_col Name of the output column counting features per
#'   neighborhood (default \code{"n_features"}). With \code{"area_weighted"} it
#'   counts the features overlapping each neighborhood. Use \code{NULL} to skip.
#' @param outside What to do when features fall outside every neighborhood:
#'   \code{"warn"} (default), \code{"error"} or \code{"ignore"}. They are dropped
#'   in every case.
#' @param verbose Print a summary of the assignment (default \code{TRUE}).
#'
#' @return \code{neighborhoods} (same rows, same order) with the summed
#'   \code{vars} and \code{count_col} added. The attribute
#'   \code{"assignment_summary"} is a data frame with, for each variable, the
#'   \code{total} in \code{numerator}, the amount \code{assigned} and the amount
#'   that fell \code{outside}.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   sq <- function(x, y) sf::st_polygon(list(rbind(
#'     c(x, y), c(x + 1, y), c(x + 1, y + 1), c(x, y + 1), c(x, y))))
#'   tracts <- sf::st_sf(id = 1:3,
#'                       geometry = sf::st_sfc(sq(0, 0), sq(1, 0), sq(2, 0)),
#'                       crs = 3857)
#'   projects <- sf::st_sf(units = c(10, 20, 5),
#'                         geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)),
#'                                               sf::st_point(c(0.2, 0.8)),
#'                                               sf::st_point(c(1.5, 0.5))),
#'                         crs = 3857)
#'   # Tract 3 has no projects and is kept with 0 units
#'   assign_to_neighborhoods(projects, tracts, vars = "units")
#' }
#' }
#'
#' @export
assign_to_neighborhoods <- function(numerator,
                                    neighborhoods,
                                    vars = NULL,
                                    method = c("point_on_surface", "area_weighted"),
                                    count_col = "n_features",
                                    outside = c("warn", "error", "ignore"),
                                    verbose = TRUE) {

  check_sf()
  method <- match.arg(method)
  outside <- match.arg(outside)
  check_polygon_layer(neighborhoods, "neighborhoods")
  kind <- check_numerator_layer(numerator, vars)

  out_names <- if (is.null(vars)) character(0) else names(vars)
  if (length(vars) > 0 && is.null(out_names)) out_names <- rep("", length(vars))
  out_names[out_names == ""] <- vars[out_names == ""]
  vars <- unname(vars)

  new_cols <- c(out_names, count_col)
  if (anyDuplicated(new_cols)) {
    stop("Output column names must be unique: ", paste(new_cols, collapse = ", "))
  }
  clash <- intersect(new_cols, names(neighborhoods))
  if (length(clash) > 0) {
    stop(sprintf(
      "neighborhoods already has column(s) %s. Name the output differently, e.g. vars = c(%s_new = \"%s\").",
      paste0("'", clash, "'", collapse = ", "), clash[1], clash[1]
    ))
  }

  if (sf::st_crs(numerator) != sf::st_crs(neighborhoods)) {
    message("Transforming numerator to match neighborhoods CRS")
    numerator <- sf::st_transform(numerator, sf::st_crs(neighborhoods))
  }

  # Drop empty geometries
  empty <- sf::st_is_empty(numerator)
  if (any(empty)) {
    warning(sprintf("%s numerator feature(s) have empty geometry and were dropped.",
                    format_count(sum(empty))), call. = FALSE)
    numerator <- numerator[!empty, ]
  }

  # Values to sum (n features x k vars), NA counted as 0
  values <- vapply(vars, function(v) as.numeric(numerator[[v]]),
                   numeric(nrow(numerator)))
  values <- matrix(values, nrow = nrow(numerator), ncol = length(vars))
  colnames(values) <- out_names
  if (anyNA(values)) {
    na_counts <- colSums(is.na(values))
    warning(sprintf(
      "NA values in numerator counted as 0: %s",
      paste(sprintf("%s (%s)", vars[na_counts > 0], format_count(na_counts[na_counts > 0])),
            collapse = ", ")
    ), call. = FALSE)
    values[is.na(values)] <- 0
  }

  n_nb <- nrow(neighborhoods)
  nb_geom <- sf::st_geometry(neighborhoods)

  if (kind == "polygon" && method == "area_weighted") {
    weights <- area_weights(sf::st_geometry(numerator), nb_geom)
    # weights: data frame of feature, neighborhood, share
    sums <- matrix(0, nrow = n_nb, ncol = ncol(values), dimnames = list(NULL, out_names))
    if (nrow(weights) > 0 && ncol(values) > 0) {
      contrib <- values[weights$feature, , drop = FALSE] * weights$share
      agg <- rowsum(contrib, weights$neighborhood)
      sums[as.integer(rownames(agg)), ] <- agg
    }
    counts <- tabulate(unique(weights[c("feature", "neighborhood")])$neighborhood,
                       nbins = n_nb)
    assigned_share <- tabulate_weights(weights, nrow(numerator))
    n_outside <- sum(assigned_share < 1e-9)
  } else {
    pts <- representative_points(numerator, kind)
    hits <- sf::st_intersects(pts, nb_geom)
    # First match only, so points on shared edges count once
    nb_idx <- vapply(hits, function(h) if (length(h) > 0) h[1] else NA_integer_,
                     integer(1))
    ok <- !is.na(nb_idx)

    sums <- matrix(0, nrow = n_nb, ncol = ncol(values), dimnames = list(NULL, out_names))
    if (any(ok) && ncol(values) > 0) {
      agg <- rowsum(values[ok, , drop = FALSE], nb_idx[ok])
      sums[as.integer(rownames(agg)), ] <- agg
    }
    counts <- tabulate(nb_idx[ok], nbins = n_nb)
    assigned_share <- as.numeric(ok)
    n_outside <- sum(!ok)
  }

  # Conservation check
  totals <- colSums(values)
  assigned <- colSums(sums)
  summary_df <- data.frame(
    variable = out_names,
    total = unname(totals),
    assigned = unname(assigned),
    stringsAsFactors = FALSE
  )
  if (!is.null(count_col)) {
    summary_df <- rbind(summary_df, data.frame(
      variable = count_col, total = nrow(numerator), assigned = sum(assigned_share),
      stringsAsFactors = FALSE
    ))
  }
  summary_df$outside <- summary_df$total - summary_df$assigned
  rownames(summary_df) <- NULL

  if (verbose) {
    message(sprintf(
      "Assigned %s of %s numerator features (%s method) to %s of %s neighborhoods%s",
      format_count(round(sum(assigned_share), 2)), format_count(nrow(numerator)),
      if (kind == "point") "point" else method,
      format_count(sum(counts > 0)), format_count(n_nb),
      if (length(out_names) > 0) {
        paste0("; ", paste(sprintf("%s: %s of %s", out_names,
                                   format_count(round(assigned, 2)),
                                   format_count(round(totals, 2))),
                           collapse = ", "))
      } else ""
    ))
  }

  lost <- n_outside > 0 || any(abs(totals - assigned) > 1e-6 * pmax(abs(totals), 1))
  if (lost) {
    msg <- sprintf(
      "%s numerator feature(s)%s fell outside every neighborhood and were dropped%s. Check the CRS and coverage of both layers.",
      format_count(n_outside),
      if (kind == "polygon" && method == "area_weighted") " (or parts of features)" else "",
      if (length(out_names) > 0) {
        paste0(" (", paste(sprintf("%s: %s", out_names, format_count(round(totals - assigned, 2))),
                           collapse = ", "), ")")
      } else ""
    )
    switch(outside,
      warn = warning(msg, call. = FALSE),
      error = stop(msg, call. = FALSE),
      ignore = if (verbose) message(msg)
    )
  }

  # Neighborhoods are the destination: add the sums to every row
  for (nm in out_names) neighborhoods[[nm]] <- sums[, nm]
  if (!is.null(count_col)) neighborhoods[[count_col]] <- counts
  neighborhoods <- geometry_last(neighborhoods)

  attr(neighborhoods, "assignment_summary") <- summary_df
  neighborhoods
}

#' Aggregate spatial units to neighborhoods
#'
#' Sums a unit variable from point (or polygon) features into neighborhoods.
#' This is a wrapper around \code{\link{assign_to_neighborhoods}()} kept for
#' backward compatibility; new code should use that function or the
#' \code{numerator} argument of \code{\link{prepare_spatial_fhlus}()}.
#'
#' @param units_data SF object with unit-level data (e.g., individual buildings)
#' @param neighborhoods SF object with neighborhood boundaries
#' @param unit_var Column name for units variable to aggregate
#' @param neighborhood_id Column name for neighborhood identifier
#' @param method How polygon features are assigned; see
#'   \code{\link{assign_to_neighborhoods}()}.
#'
#' @return SF object (neighborhoods) with aggregated units (\code{total_units})
#'   and site counts (\code{n_sites}); neighborhoods without units get 0.
#'
#' @examples
#' \donttest{
#' # Requires sf package
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   # This example would require actual spatial data
#'   # See ?assign_to_neighborhoods for a complete example
#' }
#' }
#'
#' @export
aggregate_to_neighborhoods <- function(units_data,
                                       neighborhoods,
                                       unit_var,
                                       neighborhood_id,
                                       method = c("point_on_surface", "area_weighted")) {

  check_sf()

  # Ensure both are sf objects
  if (!inherits(units_data, "sf") || !inherits(neighborhoods, "sf")) {
    stop("Both units_data and neighborhoods must be sf objects.")
  }
  if (!neighborhood_id %in% names(neighborhoods)) {
    stop(sprintf("neighborhood_id '%s' not found in neighborhoods.", neighborhood_id))
  }

  assign_to_neighborhoods(
    units_data, neighborhoods,
    vars = c(total_units = unit_var),
    method = match.arg(method),
    count_col = "n_sites"
  )
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

# ---- Internal spatial helpers ----------------------------------------------

#' Stop if sf is not installed
#' @noRd
check_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' required for spatial operations. Install with install.packages('sf').")
  }
}

#' Check that an argument is an sf layer of polygons
#' @noRd
check_polygon_layer <- function(x, arg) {
  if (!inherits(x, "sf")) {
    stop(sprintf(
      "%s must be an sf object. Got class: %s.",
      arg, paste(class(x), collapse = ", ")
    ))
  }
  geom_types <- unique(as.character(sf::st_geometry_type(x)))
  valid_types <- c("POLYGON", "MULTIPOLYGON")
  if (!all(geom_types %in% valid_types)) {
    stop(sprintf(
      "%s must contain polygon geometries (POLYGON or MULTIPOLYGON). Got: %s.",
      arg, paste(geom_types, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

#' Check a numerator layer; returns "point" or "polygon"
#' @noRd
check_numerator_layer <- function(numerator, vars) {
  if (!inherits(numerator, "sf")) {
    stop(sprintf(
      "numerator must be an sf object. Got class: %s. Use sf::st_as_sf() to convert (e.g., sf::st_as_sf(df, coords = c(\"lon\", \"lat\"), crs = 4326)).",
      paste(class(numerator), collapse = ", ")
    ))
  }
  geom_types <- unique(as.character(sf::st_geometry_type(numerator)))
  geom_types <- setdiff(geom_types, "GEOMETRYCOLLECTION")
  is_point <- all(geom_types %in% c("POINT", "MULTIPOINT"))
  is_polygon <- all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
  if (length(geom_types) == 0 || !(is_point || is_polygon)) {
    stop(sprintf(
      "numerator must contain only points (POINT/MULTIPOINT) or only polygons (POLYGON/MULTIPOLYGON). Got: %s.",
      paste(unique(as.character(sf::st_geometry_type(numerator))), collapse = ", ")
    ))
  }

  vars <- unname(vars)
  missing_vars <- setdiff(vars, names(numerator))
  if (length(missing_vars) > 0) {
    stop(sprintf(
      "numerator_vars not found in numerator: %s. Available columns: %s",
      paste(missing_vars, collapse = ", "),
      paste(setdiff(names(numerator), attr(numerator, "sf_column")), collapse = ", ")
    ))
  }
  not_numeric <- vars[!vapply(vars, function(v) is.numeric(numerator[[v]]), logical(1))]
  if (length(not_numeric) > 0) {
    stop(sprintf("numerator_vars must be numeric: %s", paste(not_numeric, collapse = ", ")))
  }

  if (is_point) "point" else "polygon"
}

#' One point per feature used for point-in-neighborhood assignment
#' @noRd
representative_points <- function(x, kind) {
  geom <- sf::st_geometry(x)
  if (kind == "point") {
    # MULTIPOINTs are placed at their first point
    if (any(sf::st_geometry_type(geom) == "MULTIPOINT")) {
      geom <- sf::st_sfc(lapply(geom, function(g) {
        if (inherits(g, "MULTIPOINT")) sf::st_point(unclass(g)[1, ]) else g
      }), crs = sf::st_crs(geom))
    }
    return(geom)
  }
  suppressWarnings(sf::st_point_on_surface(sf::st_make_valid(geom)))
}

#' Area-weighted overlap shares between features and neighborhoods
#' @return data frame: feature, neighborhood, share (of the feature's area)
#' @noRd
area_weights <- function(features, nb_geom) {
  features <- sf::st_make_valid(features)
  nb_geom <- sf::st_make_valid(nb_geom)
  feat_area <- as.numeric(sf::st_area(features))

  # sf-on-sf intersection uses a spatial index and returns one row per
  # overlapping (feature, neighborhood) pair
  overlap <- suppressWarnings(sf::st_intersection(
    sf::st_sf(feature = seq_along(features), geometry = features),
    sf::st_sf(neighborhood = seq_along(nb_geom), geometry = nb_geom)
  ))
  if (nrow(overlap) == 0) {
    return(data.frame(feature = integer(0), neighborhood = integer(0), share = numeric(0)))
  }

  pairs <- data.frame(feature = overlap$feature, neighborhood = overlap$neighborhood)
  pairs$share <- ifelse(feat_area[pairs$feature] > 0,
                        as.numeric(sf::st_area(overlap)) / feat_area[pairs$feature], 0)
  pairs[pairs$share > 0, ]
}

#' Total assigned share per feature (area-weighted)
#' @noRd
tabulate_weights <- function(weights, n) {
  out <- numeric(n)
  if (nrow(weights) > 0) {
    s <- rowsum(weights$share, weights$feature)
    out[as.integer(rownames(s))] <- pmin(s[, 1], 1)
  }
  out
}

#' Dissolve a boundary layer, optionally by an ID column
#' @noRd
dissolve_boundary <- function(boundary, boundary_id = NULL) {
  geom <- sf::st_geometry(boundary)
  if (is.null(boundary_id)) {
    return(sf::st_sf(.__fhlus_bnd = 1L, geometry = sf::st_union(geom)))
  }
  ids <- boundary[[boundary_id]]
  keep <- !is.na(ids)
  if (any(!keep)) {
    warning(sprintf("%d boundary feature(s) have NA %s and were ignored.",
                    sum(!keep), boundary_id), call. = FALSE)
  }
  groups <- unique(ids[keep])
  merged <- lapply(groups, function(g) sf::st_union(geom[keep & ids == g])[[1]])
  out <- sf::st_sf(id = groups, geometry = sf::st_sfc(merged, crs = sf::st_crs(boundary)))
  names(out)[1] <- boundary_id
  out
}

#' Keep polygon parts of an intersection and merge pieces sharing a key
#' @noRd
keep_polygon_pieces <- function(pieces, key_cols) {
  if (nrow(pieces) == 0) return(pieces)

  types <- as.character(sf::st_geometry_type(pieces))
  if (any(types == "GEOMETRYCOLLECTION")) {
    gc <- types == "GEOMETRYCOLLECTION"
    extracted <- sf::st_collection_extract(pieces[gc, ], "POLYGON", warn = FALSE)
    pieces <- rbind(pieces[!gc, ], extracted)
    types <- as.character(sf::st_geometry_type(pieces))
  }
  # Edges and corners touching the boundary produce lines/points; drop them
  pieces <- pieces[types %in% c("POLYGON", "MULTIPOLYGON"), ]

  key <- do.call(paste, c(lapply(key_cols, function(k) pieces[[k]]), sep = "\r"))
  if (anyDuplicated(key)) {
    geoms <- sf::st_geometry(pieces)
    groups <- split(seq_along(key), factor(key, levels = unique(key)))
    merged <- lapply(groups, function(i) {
      if (length(i) == 1) geoms[[i]] else sf::st_union(geoms[i])[[1]]
    })
    pieces <- pieces[!duplicated(key), ]
    sf::st_geometry(pieces) <- sf::st_sfc(merged, crs = sf::st_crs(geoms))
  }
  pieces
}

#' Drop numerator features outside the boundary (reported, not warned)
#' @noRd
drop_outside_boundary <- function(numerator, boundary, crs, method) {
  if (sf::st_crs(numerator) != crs) numerator <- sf::st_transform(numerator, crs)
  if (sf::st_crs(boundary) != crs) boundary <- sf::st_transform(boundary, crs)

  kind <- check_numerator_layer(numerator, NULL)
  test_geom <- if (kind == "polygon" && method == "area_weighted") {
    sf::st_geometry(numerator)
  } else {
    representative_points(numerator, kind)
  }
  area <- sf::st_union(sf::st_make_valid(sf::st_geometry(boundary)))
  inside <- lengths(sf::st_intersects(test_geom, area)) > 0

  if (any(!inside)) {
    message(sprintf("%s of %s numerator features are outside the boundary and were dropped",
                    format_count(sum(!inside)), format_count(length(inside))))
  }
  numerator[inside, ]
}

#' Number of vertices in a layer (for workload estimates)
#' @noRd
count_vertices <- function(x) {
  if (is.null(x)) return(0)
  nrow(sf::st_coordinates(sf::st_geometry(x)))
}

#' Warn when the spatial steps are likely to be slow
#' @noRd
warn_spatial_workload <- function(spatial_data, area_crs, boundary, exclude_layer,
                                  numerator, numerator_method) {
  reasons <- character(0)

  if (is.null(area_crs) && isTRUE(sf::st_is_longlat(spatial_data)) &&
      (!is.null(boundary) || !is.null(exclude_layer) || !is.null(numerator))) {
    reasons <- c(reasons, "data are in lat/lon, so intersections use slower spherical geometry (set area_crs to a projected CRS)")
  }

  heavy_vertices <- count_vertices(boundary) + count_vertices(exclude_layer)
  if (heavy_vertices > 500000) {
    reasons <- c(reasons, sprintf(
      "the boundary/exclusion layers have %s vertices (consider sf::st_simplify() or rmapshaper::ms_simplify())",
      format_count(heavy_vertices)))
  }

  nb_vertices <- count_vertices(spatial_data)
  if (nb_vertices > 2000000) {
    reasons <- c(reasons, sprintf("the neighborhoods have %s vertices", format_count(nb_vertices)))
  }

  if (!is.null(numerator)) {
    poly <- any(as.character(sf::st_geometry_type(numerator)) %in% c("POLYGON", "MULTIPOLYGON"))
    limit <- if (poly && numerator_method == "area_weighted") 50000 else 500000
    if (nrow(numerator) > limit) {
      reasons <- c(reasons, sprintf(
        "the numerator has %s %s features%s", format_count(nrow(numerator)),
        if (poly) "polygon" else "point",
        if (poly && numerator_method == "area_weighted") " with area weighting" else ""))
    }
  }

  if (length(reasons) > 0) {
    warning(paste0(
      "The spatial steps may take a while: ", paste(reasons, collapse = "; "), "."
    ), call. = FALSE)
  }
  invisible(reasons)
}

#' Step-by-step progress messages with elapsed time
#' @noRd
spatial_progress <- function(n_steps, verbose) {
  i <- 0L
  start <- Sys.time()
  elapsed <- function() {
    sprintf("%.1fs", as.numeric(difftime(Sys.time(), start, units = "secs")))
  }
  list(
    step = function(text) {
      i <<- i + 1L
      if (verbose) message(sprintf("[%d/%d] %s... (%s)", i, n_steps, text, elapsed()))
    },
    done = function() {
      if (verbose) message(sprintf("Spatial preparation finished in %s", elapsed()))
    }
  )
}

#' Move the geometry column to the end
#' @noRd
geometry_last <- function(x) {
  geom_col <- attr(x, "sf_column")
  x[, c(setdiff(names(x), geom_col), geom_col)]
}

#' Format a count with thousands separators
#' @noRd
format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
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
