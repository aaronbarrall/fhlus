# Tests for boundary clipping and numerator assignment

# 2x2 grid of 100x100 tracts (A B / C D), the two cities split at x = 120,
# and a set of project points
spatial_join_fixture <- function() {
  sq <- function(x, y, w = 100, h = 100) {
    sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + h), c(x, y + h), c(x, y))))
  }
  tracts <- sf::st_sf(
    GEOID = c("A", "B", "C", "D"),
    income = c(40000, 60000, 50000, 80000),
    geometry = sf::st_sfc(sq(0, 0), sq(100, 0), sq(0, 100), sq(100, 100)),
    crs = 3857
  )
  cities <- sf::st_sf(
    city = c("West", "East"),
    geometry = sf::st_sfc(sq(0, 0, 120, 200), sq(120, 0, 80, 200)),
    crs = 3857
  )
  points <- sf::st_sf(
    units = c(10, 20, 5, 7),
    geometry = sf::st_sfc(
      sf::st_point(c(50, 50)),    # A
      sf::st_point(c(100, 50)),   # on the A|B edge
      sf::st_point(c(150, 50)),   # B (East)
      sf::st_point(c(50, 150))    # C
    ),
    crs = 3857
  )
  far_point <- sf::st_sf(
    units = 100, geometry = sf::st_sfc(sf::st_point(c(900, 900))), crs = 3857
  )
  # Parcel straddling A and B, half in each
  parcel <- sf::st_sf(units = 100, geometry = sf::st_sfc(sq(50, 0, 100, 50)), crs = 3857)
  list(tracts = tracts, cities = cities, points = points,
       far_point = far_point, parcel = parcel)
}

test_that("assign_to_neighborhoods keeps every neighborhood and fills zeros", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  r <- assign_to_neighborhoods(fx$points, fx$tracts, vars = "units", verbose = FALSE)

  expect_s3_class(r, "sf")
  expect_equal(nrow(r), 4)
  expect_equal(r$GEOID, fx$tracts$GEOID)
  expect_equal(r$units[r$GEOID == "D"], 0)
  expect_equal(r$n_features[r$GEOID == "D"], 0)
})

test_that("a point on a shared edge is counted once", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  r <- assign_to_neighborhoods(fx$points, fx$tracts, vars = "units", verbose = FALSE)

  expect_equal(sum(r$units), sum(fx$points$units))
  expect_equal(sum(r$n_features), nrow(fx$points))
})

test_that("features outside all neighborhoods warn, error or are ignored", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  pts <- rbind(fx$points, fx$far_point)

  expect_warning(
    r <- assign_to_neighborhoods(pts, fx$tracts, vars = "units", verbose = FALSE),
    "fell outside every neighborhood"
  )
  s <- attr(r, "assignment_summary")
  expect_equal(s$outside[s$variable == "units"], 100)
  expect_equal(s$outside[s$variable == "n_features"], 1)

  expect_error(
    assign_to_neighborhoods(pts, fx$tracts, vars = "units", outside = "error", verbose = FALSE),
    "fell outside every neighborhood"
  )
  expect_silent(
    assign_to_neighborhoods(pts, fx$tracts, vars = "units", outside = "ignore", verbose = FALSE)
  )
})

test_that("polygon numerators use point_on_surface or area weighting", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  pos <- assign_to_neighborhoods(fx$parcel, fx$tracts, vars = "units", verbose = FALSE)
  expect_equal(sum(pos$units), 100)
  expect_equal(sum(pos$units > 0), 1)

  aw <- assign_to_neighborhoods(fx$parcel, fx$tracts, vars = "units",
                                method = "area_weighted", verbose = FALSE)
  expect_equal(aw$units, c(50, 50, 0, 0))
  expect_equal(aw$n_features, c(1, 1, 0, 0))
})

test_that("named vars rename the output and clashes are rejected", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  r <- assign_to_neighborhoods(fx$points, fx$tracts, vars = c(total_units = "units"),
                               count_col = "n_sites", verbose = FALSE)
  expect_true(all(c("total_units", "n_sites") %in% names(r)))

  expect_error(
    assign_to_neighborhoods(fx$points, fx$tracts, vars = c(income = "units"), verbose = FALSE),
    "already has column"
  )
})

test_that("assign_to_neighborhoods validates its inputs", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  expect_error(assign_to_neighborhoods(data.frame(units = 1), fx$tracts, vars = "units"),
               "numerator must be an sf object")
  expect_error(assign_to_neighborhoods(fx$points, fx$tracts, vars = "nope"),
               "not found in numerator")
  line <- sf::st_sf(units = 1, geometry = sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(1, 1)))),
                    crs = 3857)
  expect_error(assign_to_neighborhoods(line, fx$tracts, vars = "units"),
               "only points")
})

test_that("assign_to_neighborhoods transforms a numerator in another CRS", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  pts <- sf::st_transform(fx$points, 4326)

  expect_message(
    r <- assign_to_neighborhoods(pts, fx$tracts, vars = "units", verbose = FALSE),
    "Transforming numerator"
  )
  expect_equal(sum(r$units), sum(fx$points$units))
})

test_that("aggregate_to_neighborhoods keeps its old output columns", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  r <- suppressMessages(aggregate_to_neighborhoods(fx$points, fx$tracts, "units", "GEOID"))
  expect_equal(nrow(r), 4)
  expect_equal(sum(r$total_units), sum(fx$points$units))
  expect_equal(r$n_sites[r$GEOID == "D"], 0)
})

test_that("clip_to_boundary clips areas and records the share inside", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  west <- clip_to_boundary(fx$tracts, fx$cities[1, ], verbose = FALSE)

  expect_equal(nrow(west), 4)
  expect_equal(west$original_area, rep(10000, 4))
  expect_equal(west$share_in_boundary, c(1, 0.2, 1, 0.2))
  expect_equal(as.numeric(sf::st_area(west)), c(10000, 2000, 10000, 2000))
  # Rank variables are kept unchanged
  expect_equal(west$income, fx$tracts$income)
})

test_that("clip_to_boundary drops slivers below min_share", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  west <- clip_to_boundary(fx$tracts, fx$cities[1, ], min_share = 0.25, verbose = FALSE)
  expect_equal(west$GEOID, c("A", "C"))

  expect_error(clip_to_boundary(fx$tracts, fx$cities, min_share = 2), "min_share")
})

test_that("clip_to_boundary with boundary_id splits tracts between jurisdictions", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  both <- clip_to_boundary(fx$tracts, fx$cities, boundary_id = "city", verbose = FALSE)

  expect_equal(nrow(both), 6)
  expect_equal(sort(both$city[both$GEOID == "B"]), c("East", "West"))
  expect_equal(sum(sf::st_area(both)), sum(sf::st_area(fx$tracts)))
  expect_error(clip_to_boundary(fx$tracts, fx$cities, boundary_id = "nope"), "not found")
})

test_that("clip_to_boundary ignores pieces that only touch the boundary", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  # Boundary exactly covering A touches B and C along edges and D at a corner
  a_only <- fx$tracts[1, "GEOID"]
  names(a_only)[1] <- "name"

  r <- clip_to_boundary(fx$tracts, a_only, min_share = 0, verbose = FALSE)
  expect_equal(r$GEOID, "A")
  expect_true(all(sf::st_geometry_type(r) == "MULTIPOLYGON"))
})

test_that("prepare_spatial_fhlus clips, measures and assigns in one call", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  pts <- rbind(fx$points, fx$far_point)

  prep <- prepare_spatial_fhlus(fx$tracts, boundary = fx$cities[1, ],
                                numerator = pts, numerator_vars = "units",
                                verbose = FALSE)

  expect_equal(prep$total_area, c(10000, 2000, 10000, 2000))
  expect_equal(prep$available_area, prep$total_area)
  # Point in East's part of B and the far point are outside West and dropped
  # without a warning (they are outside the boundary)
  expect_equal(prep$units, c(30, 0, 7, 0))

  score <- fhlus_score(sf::st_drop_geometry(prep), "units", "income", "available_area",
                       graph = FALSE, verbose = FALSE)
  expect_true(is_valid_fhlus_score(score$scores$score[1]))
})

test_that("prepare_spatial_fhlus prints numbered progress steps", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  msgs <- character(0)
  withCallingHandlers(
    prepare_spatial_fhlus(fx$tracts, boundary = fx$cities[1, ],
                          numerator = fx$points, numerator_vars = "units"),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("^\\[1/4\\]", msgs)))
  expect_true(any(grepl("^\\[4/4\\] Assigning", msgs)))
  expect_false(any(grepl("\\[5/", msgs)))
})

test_that("prepare_spatial_fhlus output with boundary_id feeds grouped_fhlus", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  prep <- prepare_spatial_fhlus(fx$tracts, boundary = fx$cities, boundary_id = "city",
                                numerator = fx$points, numerator_vars = "units",
                                verbose = FALSE)
  expect_equal(sum(prep$units), sum(fx$points$units))

  g <- grouped_fhlus(sf::st_drop_geometry(prep), group_var = "city",
                     numerator = "units", rank_var = "income",
                     denom = "available_area", verbose = FALSE)
  expect_setequal(g$city, c("West", "East"))
})

test_that("prepare_spatial_fhlus works inside lapply over jurisdictions", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  per_city <- lapply(split(fx$cities, fx$cities$city), function(b) {
    prepare_spatial_fhlus(fx$tracts, boundary = b, numerator = fx$points,
                          numerator_vars = "units", verbose = FALSE)
  })
  expect_equal(sum(vapply(per_city, function(x) sum(x$units), numeric(1))),
               sum(fx$points$units))
})

test_that("prepare_spatial_fhlus rejects boundary_id without boundary", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  expect_error(prepare_spatial_fhlus(fx$tracts, boundary_id = "city"), "without a boundary")
})

test_that("large jobs warn that they may be slow", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  # ~570k vertices along the boundary edge
  heavy <- sf::st_segmentize(fx$cities, dfMaxLength = 0.001)

  expect_warning(
    warn_spatial_workload(fx$tracts, heavy, NULL, NULL, "point_on_surface"),
    "may take a while"
  )
  expect_silent(
    warn_spatial_workload(fx$tracts, fx$cities, NULL, fx$points, "point_on_surface")
  )
})

# Append one feature with empty geometry (like water-only Census tracts)
with_empty <- function(x, ...) {
  extra <- sf::st_sf(..., geometry = sf::st_sfc(sf::st_polygon(), crs = sf::st_crs(x)))
  rbind(x, extra)
}

test_that("count_vertices skips empty geometries", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  expect_equal(count_vertices(with_empty(fx$tracts, GEOID = "E", income = 1)),
               count_vertices(fx$tracts))
  expect_equal(count_vertices(fx$tracts[1, ][0, ]), 0)
  all_empty <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sf::st_polygon(), sf::st_polygon(),
                                                         crs = 3857))
  expect_equal(count_vertices(all_empty), 0)
})

test_that("prepare_spatial_fhlus drops empty geometries from every layer", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  park <- sf::st_sf(id = 1, geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(50, 0), c(50, 50), c(0, 50), c(0, 0)))),
    crs = 3857))
  empty_point <- sf::st_sf(units = 99, geometry = sf::st_sfc(sf::st_point(), crs = 3857))

  clean <- prepare_spatial_fhlus(fx$tracts, boundary = fx$cities, exclude_layer = park,
                                 numerator = fx$points, numerator_vars = "units",
                                 verbose = FALSE)

  expect_warning(
    expect_message(
      dirty <- prepare_spatial_fhlus(
        with_empty(fx$tracts, GEOID = "E", income = 1),
        boundary = with_empty(fx$cities, city = "Nowhere"),
        exclude_layer = with_empty(park, id = 2),
        numerator = rbind(fx$points, empty_point), numerator_vars = "units",
        verbose = TRUE
      ),
      "Dropped 1 feature with empty geometry from `spatial_data`"
    ),
    "Dropped 1 feature with empty geometry from `numerator`"
  )

  expect_false("E" %in% dirty$GEOID)
  expect_equal(sf::st_drop_geometry(dirty), sf::st_drop_geometry(clean))
})

test_that("prepare_spatial_fhlus reports empty boundary/exclusion features when verbose", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()

  expect_message(
    prepare_spatial_fhlus(fx$tracts, boundary = with_empty(fx$cities, city = "Nowhere"),
                          verbose = TRUE),
    "from `boundary`"
  )
  expect_no_message(
    prepare_spatial_fhlus(fx$tracts, boundary = with_empty(fx$cities, city = "Nowhere"),
                          verbose = FALSE)
  )
})

test_that("prepare_spatial_fhlus errors when every geometry is empty", {
  skip_if_not_installed("sf")
  all_empty <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sf::st_polygon(), sf::st_polygon(),
                                                         crs = 3857))
  expect_error(prepare_spatial_fhlus(all_empty, verbose = FALSE),
               "All 2 geometries in `spatial_data` are empty")

  fx <- spatial_join_fixture()
  expect_error(prepare_spatial_fhlus(fx$tracts, boundary = all_empty, verbose = FALSE),
               "All 2 geometries in `boundary` are empty")
})

test_that("count_vertices handles mixed geometry types", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  # POLYGON + MULTIPOLYGON gives sfc_GEOMETRY, as st_make_valid() can return
  mixed <- fx$tracts
  sf::st_geometry(mixed) <- sf::st_sfc(
    c(list(sf::st_multipolygon(list(sf::st_geometry(fx$tracts)[[1]]))),
      as.list(sf::st_geometry(fx$tracts)[-1])),
    crs = sf::st_crs(fx$tracts)
  )
  expect_s3_class(sf::st_geometry(mixed), "sfc_GEOMETRY")
  expect_equal(count_vertices(mixed), 20)
  expect_equal(count_vertices(with_empty(mixed, GEOID = "E", income = 1)), 20)
  expect_no_error(prepare_spatial_fhlus(fx$tracts, exclude_layer = mixed, verbose = FALSE))
})

test_that("spatial exclusion only uses exclusion features near the neighborhoods", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  sq <- function(x, y, w) {
    sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
  }
  # One park inside tract A, one far away
  parks <- sf::st_sf(id = 1:2, geometry = sf::st_sfc(sq(10, 10, 20), sq(5000, 5000, 20)),
                     crs = 3857)

  expect_equal(nrow(crop_to_bbox(parks, fx$tracts)), 1)
  r <- prepare_spatial_fhlus(fx$tracts, exclude_layer = parks, verbose = FALSE)
  expect_equal(r$excluded_area, c(400, 0, 0, 0))

  expect_message(
    r2 <- calculate_excluded_areas_spatial(
      transform(fx$tracts, total_area = 1e4, excluded_area = 0), parks[2, ]),
    "does not overlap"
  )
  expect_equal(r2$excluded_area, rep(0, 4))
})

test_that("lat/lon spatial_data is projected to the local UTM zone", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  # Move the fixture to Burbank, CA (UTM 11N) and switch to lat/lon
  shift <- function(x) {
    g <- sf::st_geometry(x) + c(-13161000, 4040000)
    sf::st_geometry(x) <- sf::st_sfc(g, crs = 3857)
    sf::st_transform(x, 4269)
  }
  tracts <- shift(fx$tracts)
  cities <- shift(fx$cities)

  expect_equal(auto_area_crs(tracts)$crs, 32611L)

  expect_message(
    r <- prepare_spatial_fhlus(tracts, boundary = cities, verbose = FALSE),
    "UTM zone 11N \\(EPSG:32611\\)"
  )
  expect_equal(sf::st_crs(r)$epsg, 32611L)
  # Same areas as a direct planar run in that CRS
  direct <- prepare_spatial_fhlus(tracts, boundary = cities, area_crs = 32611,
                                  verbose = FALSE)
  expect_equal(r$total_area, direct$total_area)

  # Projected input is left alone
  expect_no_message(prepare_spatial_fhlus(fx$tracts, verbose = FALSE))
})

test_that("auto_area_crs uses equal-area for extents too wide for one UTM zone", {
  skip_if_not_installed("sf")
  box <- function(xmin, ymin, xmax, ymax) {
    sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmax, ymin), c(xmax, ymax), c(xmin, ymax), c(xmin, ymin)))),
      crs = 4326))
  }
  # Roughly California: spans UTM zones 10 and 11
  ca <- auto_area_crs(box(-124.4, 32.5, -114.1, 42.0))
  expect_match(ca$crs, "+proj=laea", fixed = TRUE)
  expect_match(ca$label, "equal-area")
  # Southern hemisphere, small extent
  expect_equal(auto_area_crs(box(18.4, -34.0, 18.6, -33.8))$crs, 32734L)

  # Areas in the equal-area CRS match s2's ellipsoidal areas closely
  b <- box(-124.4, 32.5, -114.1, 42.0)
  expect_equal(as.numeric(sf::st_area(sf::st_transform(sf::st_segmentize(b, 10000), ca$crs))),
               as.numeric(sf::st_area(sf::st_segmentize(b, 10000))), tolerance = 0.005)
})

test_that("a large exclusion layer far from the neighborhoods is cropped up front", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  sq <- function(x, y, w) {
    sf::st_polygon(list(rbind(c(x, y), c(x + w, y), c(x + w, y + w), c(x, y + w), c(x, y))))
  }
  # ~800k vertices, all far from the tracts, plus one small park in tract A
  far <- sf::st_segmentize(sf::st_sfc(sq(5000, 5000, 100), crs = 3857), 0.0005)
  parks <- sf::st_sf(id = 1:2, geometry = c(sf::st_sfc(sq(10, 10, 20), crs = 3857), far))
  expect_gt(count_vertices(parks), 500000)

  expect_no_warning(
    r <- prepare_spatial_fhlus(fx$tracts, exclude_layer = parks, verbose = FALSE)
  )
  expect_equal(r$excluded_area, c(400, 0, 0, 0))
})
