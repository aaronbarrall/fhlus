# Tests for the linked plot + map widget

skip_if_not_installed("sf")
skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")
skip_if_not_installed("jsonlite")

# A 4 x 3 grid of square "tracts" in a projected CRS (UTM 11N, near LA)
make_linked_sf <- function(nx = 4, ny = 3) {
  sq <- function(x, y, s = 1000) {
    sf::st_polygon(list(rbind(c(x, y), c(x + s, y), c(x + s, y + s),
                              c(x, y + s), c(x, y))))
  }
  cells <- expand.grid(i = seq_len(nx) - 1, j = seq_len(ny) - 1)
  n <- nrow(cells)
  set.seed(42)
  sf::st_sf(
    GEOID = sprintf("060370%05d", seq_len(n)),
    units = c(0, sample(10:200, n - 1, replace = TRUE)),
    income = sample(30000:90000, n),
    rent = sample(1000:2500, n),
    geometry = sf::st_sfc(
      lapply(seq_len(n), function(k) sq(380000 + cells$i[k] * 1000,
                                        3770000 + cells$j[k] * 1000)),
      crs = 32611
    )
  )
}

linked_result <- function(geo = make_linked_sf()) {
  geo$area <- as.numeric(sf::st_area(geo))
  fhlus_score(geo, "units", "income", "area", graph = FALSE, verbose = FALSE)
}

parse_geojson <- function(w) {
  jsonlite::fromJSON(as.character(w$x$geojson), simplifyVector = FALSE)
}

test_that("fhlus_score accepts sf input with verbose checks on", {
  geo <- make_linked_sf()
  geo$area <- as.numeric(sf::st_area(geo))
  expect_no_error(fhlus_score(geo, "units", "income", "area", graph = FALSE))
})

test_that("fhlus_linked builds a widget from an sf-based result", {
  result <- linked_result()
  w <- fhlus_linked(result, tooltip_vars = c("Rent" = "rent"))

  expect_s3_class(w, "fhlus_linked")
  expect_s3_class(w, "htmlwidget")
  n <- nrow(fhlus_segments(result))
  expect_length(w$x$ids, n)
  expect_length(w$x$tips, n)
  expect_length(w$x$x0, n)
  expect_true(all(grepl("Rent", w$x$tips)))
  expect_equal(w$x$n_unmapped, 0)
})

test_that("GeoJSON features are in WGS84 and indexed by segment row", {
  result <- linked_result()
  w <- fhlus_linked(result)
  gj <- parse_geojson(w)
  seg <- fhlus_segments(result)

  idx <- vapply(gj$features, function(f) f$properties$i, numeric(1))
  expect_setequal(idx, seq_len(nrow(seg)) - 1)

  b <- w$x$bounds
  expect_true(b[1] > -119 && b[3] < -117 && b[2] > 33 && b[4] < 35)

  # Feature i has the polygon of the segment in row i + 1
  geo <- sf::st_transform(make_linked_sf(), 4326)
  k <- which(idx == 0)
  first_id <- seg$id[1]
  coords <- unlist(gj$features[[k]]$geometry$coordinates)
  expected <- sf::st_bbox(geo[geo$GEOID == first_id, ])
  expect_equal(min(coords[c(TRUE, FALSE)]), unname(expected["xmin"]), tolerance = 1e-5)
})

test_that("plot data keeps tooltips out of the hover layer and drops attrs", {
  w <- fhlus_linked(linked_result())
  fig <- jsonlite::fromJSON(as.character(w$x$fig), simplifyVector = FALSE)

  expect_setequal(names(fig), c("data", "layout", "config"))
  hover <- Filter(function(tr) !is.null(tr$customdata), fig$data)
  expect_length(hover, 1)
  expect_null(hover[[1]]$text)
})

test_that("color layers give one color per geography and a legend", {
  result <- linked_result()
  w <- fhlus_linked(result)
  n <- nrow(fhlus_segments(result))

  keys <- vapply(w$x$layers, `[[`, "", "key")
  expect_equal(keys, c("rank", "numerator"))
  for (L in w$x$layers) {
    expect_length(L$colors, n)
    expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", L$colors)))
  }

  # Rank colors run light to dark with rank
  rank_cols <- w$x$layers[[1]]$colors
  lum <- colSums(grDevices::col2rgb(rank_cols) * c(0.299, 0.587, 0.114))
  expect_true(lum[1] > lum[n])

  # Zero numerator is grey, and listed first in the legend
  seg <- fhlus_segments(result)
  num <- w$x$layers[[2]]
  expect_equal(unique(num$colors[seg$units == 0]), LINKED_ZERO_COLOR)
  expect_equal(num$legend$items[[1]]$label, "0")
})

test_that("geometry can be supplied separately and is matched by ID", {
  geo <- make_linked_sf()
  df <- sf::st_drop_geometry(geo)
  df$area <- 1e6
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  shuffled <- geo[rev(seq_len(nrow(geo))), ]
  w <- fhlus_linked(result, geometry = shuffled)
  gj <- parse_geojson(w)
  seg <- fhlus_segments(result)

  # Feature for segment row 1 is that geography's polygon
  k <- which(vapply(gj$features, function(f) f$properties$i, numeric(1)) == 0)
  coords <- unlist(gj$features[[k]]$geometry$coordinates)
  expected <- sf::st_bbox(sf::st_transform(geo[geo$GEOID == seg$id[1], ], 4326))
  expect_equal(max(coords[c(FALSE, TRUE)]), unname(expected["ymax"]), tolerance = 1e-5)
})

test_that("missing and extra polygons are reported", {
  geo <- make_linked_sf()
  df <- sf::st_drop_geometry(geo)
  df$area <- 1e6
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  partial <- geo[-(1:2), ]
  expect_warning(w <- fhlus_linked(result, geometry = partial),
                 "2 of 12 geographies")
  expect_equal(w$x$n_unmapped, 2)
  expect_length(parse_geojson(w)$features, 10)

  extra <- rbind(geo, transform(geo[1, ], GEOID = "999"))
  expect_message(fhlus_linked(result, geometry = extra), "1 polygon")
})

test_that("bad geometry inputs give clear errors", {
  geo <- make_linked_sf()
  df <- sf::st_drop_geometry(geo)
  df$area <- 1e6
  result <- fhlus_score(df, "units", "income", "area", graph = FALSE, verbose = FALSE)

  expect_error(fhlus_linked(result), "has no geometry")
  expect_error(fhlus_linked(result, geometry = df), "must be an sf object")
  renamed <- geo
  renamed$GEOID <- paste0("x", renamed$GEOID)
  expect_error(suppressWarnings(fhlus_linked(result, geometry = renamed)), "No IDs")

  dup <- rbind(geo, geo[1, ])
  expect_error(fhlus_linked(result, geometry = dup), "duplicated ID")

  pts <- sf::st_sf(GEOID = geo$GEOID, geometry = sf::st_centroid(sf::st_geometry(geo)))
  expect_error(fhlus_linked(result, geometry = pts), "polygon geometry")

  no_crs <- sf::st_set_crs(geo, NA)
  expect_error(fhlus_linked(result, geometry = no_crs), "no coordinate reference")
})

test_that("max_features and argument checks are enforced", {
  result <- linked_result()
  expect_error(fhlus_linked(result, max_features = 5), "more than max_features")
  expect_error(fhlus_linked(result, fill_by = "density"))
  expect_error(fhlus_linked(result, fill_opacity = 2), "fill_opacity")

  w <- fhlus_linked(result, fill_by = "numerator", basemap = "aerial",
                    layout = "stacked", fill_opacity = 0.4)
  expect_equal(w$x$fill_by, "numerator")
  expect_equal(w$x$basemap, "aerial")
  expect_equal(w$x$layout, "stacked")
  expect_equal(w$x$fill_opacity, 0.4)
})

test_that("plot options pass through to fhlus_interactive", {
  w <- fhlus_linked(linked_result(), title = "My title", color_var = "darkgreen")
  fig <- jsonlite::fromJSON(as.character(w$x$fig), simplifyVector = FALSE)
  expect_match(fig$layout$title$text, "My title")
  expect_equal(toupper(w$x$color_var), "#006400")
})

test_that("simplify reduces vertices and FALSE leaves them", {
  # A finely subdivided circle
  centers <- sf::st_sfc(sf::st_point(c(380000, 3770000)),
                        sf::st_point(c(400000, 3770000)), crs = 32611)
  geo <- sf::st_sf(GEOID = c("a", "b"),
                   units = c(10, 20), income = c(1, 2), area = c(1, 1),
                   geometry = sf::st_buffer(centers, 5000, nQuadSegs = 400))
  result <- fhlus_score(geo, "units", "income", "area", graph = FALSE, verbose = FALSE)

  n_coords <- function(w) length(unlist(lapply(parse_geojson(w)$features,
                                              function(f) f$geometry$coordinates)))
  full <- n_coords(fhlus_linked(result, simplify = FALSE))
  simple <- n_coords(fhlus_linked(result, simplify = 200))
  expect_lt(simple, full / 4)
})

test_that("fhlus_save_html writes a linked widget", {
  w <- fhlus_linked(linked_result())
  f <- tempfile(fileext = ".html")
  on.exit(unlink(f))
  fhlus_save_html(w, f, selfcontained = FALSE)
  expect_true(file.exists(f))
  html <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(html, "fhlus_linked")
  expect_match(html, "maplibre-gl")
  unlink(paste0(tools::file_path_sans_ext(f), "_files"), recursive = TRUE)
})

test_that("Shiny bindings are available", {
  skip_if_not_installed("shiny")
  out <- fhlus_linkedOutput("linked", height = "500px")
  expect_match(as.character(out), "fhlus_linked")
  expect_true(is.function(renderFhlus_linked(NULL)))
})
