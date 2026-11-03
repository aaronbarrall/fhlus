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

test_that("large or lat/lon jobs warn that they may be slow", {
  skip_if_not_installed("sf")
  fx <- spatial_join_fixture()
  ll <- sf::st_transform(fx$tracts, 4326)

  expect_warning(
    warn_spatial_workload(ll, NULL, sf::st_transform(fx$cities, 4326), NULL, NULL,
                          "point_on_surface"),
    "may take a while"
  )
  expect_silent(
    warn_spatial_workload(fx$tracts, NULL, fx$cities, NULL, fx$points, "point_on_surface")
  )
})
