# Linked FHLUS Plot + Map
#
# fhlus_linked() builds a single htmlwidget with two panels: the interactive
# FHLUS curve (plotly.js) and a map of the same geographies (MapLibre GL).
# Hovering a segment highlights its polygon and hovering a polygon highlights
# its segment; clicking either pins the geography. Everything that does not
# change after rendering (tooltips, colors, geometry) is computed in R; the
# JavaScript in inst/htmlwidgets/fhlus_linked.js only switches styles, so
# interaction stays smooth for thousands of polygons.

#' Linked Interactive FHLUS Plot and Map
#'
#' Shows the interactive FHLUS curve next to a map of the same geographies,
#' linked in both directions: hovering a segment of the curve outlines its
#' neighborhood on the map, and hovering a neighborhood highlights its segment
#' on the curve. Clicking either one pins the geography (click again, click an
#' empty part of the map, or press Esc to unpin).
#'
#' The map can be colored by opportunity rank or by the numerator, or shown as
#' outlines only, and the basemap can be switched between a light map, a
#' street map and aerial imagery. These are toggles in the widget itself, so
#' they also work in a saved HTML file. Basemap tiles come from Esri and need
#' no API key; they are streamed when the page is viewed, so viewing
#' the map needs an internet connection (everything else is embedded).
#'
#' The usual workflow is to prepare the geographies with
#' \code{prepare_spatial_fhlus()}, score the resulting sf object with
#' \code{fhlus_score()}, then pass the result here. The result then carries
#' its own geometry. Alternatively, score a plain data frame and pass the
#' polygons separately via \code{geometry}, matched on the ID column.
#'
#' Requires the \pkg{plotly}, \pkg{htmlwidgets}, \pkg{sf} and \pkg{jsonlite}
#' packages.
#'
#' @inheritParams fhlus_segments
#' @param geometry Optional sf object with polygon geometry and the ID column.
#'   If NULL, the geometry of \code{result$data} is used (the result must then
#'   come from sf data).
#' @param fill_by Initial map coloring: "rank" (opportunity rank),
#'   "numerator" (the numerator, e.g. housing units) or "none" (outlines
#'   only). All three can be switched in the widget.
#' @param basemap Initial basemap: "light", "streets", "aerial" or "none".
#' @param fill_opacity Initial opacity of the polygon fill (0-1).
#' @param layout "side" (plot left, map right) or "stacked" (plot above map).
#'   Narrow containers always stack.
#' @param simplify Polygon simplification tolerance in meters. NULL (default)
#'   simplifies only large inputs (over 100,000 vertices); FALSE never
#'   simplifies.
#' @param max_features Maximum number of geographies. Larger inputs stop with
#'   an error, since the page would become slow and very large.
#' @param width,height Widget size (CSS units or pixels), or NULL to fill the
#'   container.
#' @param elementId Optional element ID for the widget. In Shiny, use the
#'   output ID instead (see \code{fhlus_linkedOutput()}).
#' @param ... Passed to \code{fhlus_interactive()} for the plot panel (e.g.
#'   \code{title}, \code{subtitle}, \code{show_ci_bands}, \code{color_var},
#'   \code{boundary_label}).
#'
#' @return An htmlwidget of class "fhlus_linked". Print it to view it, save it
#'   with \code{fhlus_save_html()}, or render it in Shiny with
#'   \code{renderFhlus_linked()}.
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   nc$area_km2 <- as.numeric(sf::st_area(nc)) / 1e6
#'   nc$nonwhite_share <- nc$NWBIR74 / nc$BIR74
#'   result <- fhlus_score(nc, numerator = "BIR74", rank_var = "nonwhite_share",
#'                         denom = "area_km2", graph = FALSE)
#'   fhlus_linked(result, id = "NAME", tooltip_vars = c("Births" = "BIR74"))
#' }
#'
#' @export
fhlus_linked <- function(result,
                         geometry = NULL,
                         id = NULL,
                         tooltip_vars = NULL,
                         fill_by = c("rank", "numerator", "none"),
                         basemap = c("light", "streets", "aerial", "none"),
                         fill_opacity = 0.7,
                         layout = c("side", "stacked"),
                         simplify = NULL,
                         max_features = 5000,
                         width = NULL,
                         height = NULL,
                         elementId = NULL,
                         ...) {
  check_linked_deps()
  fill_by <- match.arg(fill_by)
  basemap <- match.arg(basemap)
  layout <- match.arg(layout)
  if (!is.numeric(fill_opacity) || length(fill_opacity) != 1 ||
      fill_opacity < 0 || fill_opacity > 1) {
    stop("fill_opacity must be a single number between 0 and 1.")
  }

  seg <- fhlus_segments(result, id = id, tooltip_vars = tooltip_vars)
  meta <- attr(seg, "fhlus_meta")
  if (nrow(seg) > max_features) {
    stop(sprintf(paste0(
      "This result has %s geographies, more than max_features = %s. ",
      "Linked maps stay smooth up to a few thousand polygons; aggregate to ",
      "larger geographies, or raise max_features if you accept a slower, ",
      "larger page."),
      format(nrow(seg), big.mark = ","), format(max_features, big.mark = ",")))
  }

  geom <- linked_geometry(result, seg, geometry, id)
  geom <- clean_linked_geometry(geom, simplify)

  plot_args <- list(...)
  plot_args$result <- result
  plot_args["id"] <- list(id)
  plot_args["tooltip_vars"] <- list(tooltip_vars)
  plot_args$highlight <- FALSE
  # Thousands of boundary dots merge into a solid line that hides the
  # highlighted segment, so drop them for large inputs unless asked for
  if (!"boundary_label" %in% names(plot_args) && nrow(seg) > 500) {
    plot_args["boundary_label"] <- list(NULL)
  }
  p <- do.call(fhlus_interactive, plot_args)
  p <- plotly::layout(
    p,
    legend = list(orientation = "h", x = 0, xanchor = "left",
                  y = -0.2, yanchor = "top"),
    margin = list(t = 80, r = 20, b = 120)
  )
  plot_deps <- plotly_js_dependencies(p)

  color_var <- if (!is.null(list(...)$color_var)) list(...)$color_var else "red"

  # The hover layer repeats each tooltip at every hover point; drop those
  # copies and let the widget refill them from `tips` (by customdata ID)
  built <- plotly::plotly_build(p)
  built$x$data <- lapply(built$x$data, function(tr) {
    if (!is.null(tr$customdata)) tr$text <- NULL
    tr
  })

  # Keep only what plotly.js needs; `attrs` would carry a second copy of the
  # data (including every tooltip)
  fig <- jsonlite::fromJSON(plotly::plotly_json(built, jsonedit = FALSE, pretty = FALSE),
                            simplifyVector = FALSE)
  fig <- jsonlite::toJSON(fig[intersect(c("data", "layout", "config"), names(fig))],
                          auto_unbox = TRUE, null = "null", na = "null", digits = NA)

  x <- list(
    fig = fig,
    geojson = geometry_to_geojson(geom),
    bounds = I(unname(as.numeric(sf::st_bbox(geom$geometry)))),
    ids = I(seg$id),
    tips = I(build_tooltips(seg)),
    x0 = I(seg$cum_denom_start), x1 = I(seg$cum_denom_end),
    y0 = I(seg$cum_var_start), y1 = I(seg$cum_var_end),
    layers = linked_color_layers(seg, meta),
    fill_by = fill_by,
    basemap = basemap,
    fill_opacity = fill_opacity,
    layout = layout,
    color_var = grDevices::rgb(t(grDevices::col2rgb(color_var)), maxColorValue = 255),
    n_unmapped = sum(!geom$mapped)
  )

  w <- htmlwidgets::createWidget(
    name = "fhlus_linked",
    x = x,
    width = width,
    height = height,
    package = "fhlus",
    elementId = elementId,
    dependencies = plot_deps,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = 640,
      padding = 0,
      browser.fill = TRUE,
      browser.padding = 0,
      viewer.fill = TRUE,
      viewer.padding = 0,
      knitr.figure = FALSE,
      knitr.defaultWidth = "100%",
      knitr.defaultHeight = 640
    )
  )
  attr(w, "segments") <- seg
  w
}

#' Shiny Bindings for fhlus_linked
#'
#' Output and render functions for using \code{fhlus_linked()} in Shiny. The
#' widget reports the hovered and pinned geography IDs as inputs:
#' \code{input$<outputId>_hover} and \code{input$<outputId>_selected} (NULL
#' when nothing is hovered or pinned).
#'
#' @param outputId Output variable to read from.
#' @param width,height Must be a valid CSS unit (like "100\%", "640px") or a
#'   number, which will be coerced to a string and have "px" appended.
#' @param expr An expression that generates an \code{fhlus_linked()} widget.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})?
#'
#' @return \code{fhlus_linkedOutput()} returns a Shiny output element;
#'   \code{renderFhlus_linked()} returns a Shiny render function.
#'
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   nc$area_km2 <- as.numeric(sf::st_area(nc)) / 1e6
#'   nc$nonwhite_share <- nc$NWBIR74 / nc$BIR74
#'   result <- fhlus_score(nc, "BIR74", "nonwhite_share", "area_km2", graph = FALSE)
#'
#'   ui <- shiny::fluidPage(
#'     fhlus_linkedOutput("linked", height = "640px"),
#'     shiny::verbatimTextOutput("picked")
#'   )
#'   server <- function(input, output, session) {
#'     output$linked <- renderFhlus_linked(fhlus_linked(result, id = "NAME"))
#'     output$picked <- shiny::renderPrint(
#'       list(hover = input$linked_hover, pinned = input$linked_selected)
#'     )
#'   }
#'   shiny::shinyApp(ui, server)
#' }
#'
#' @name fhlus_linked-shiny
#' @export
fhlus_linkedOutput <- function(outputId, width = "100%", height = "640px") {
  htmlwidgets::shinyWidgetOutput(outputId, "fhlus_linked", width, height,
                                 package = "fhlus")
}

#' @rdname fhlus_linked-shiny
#' @export
renderFhlus_linked <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, fhlus_linkedOutput, env, quoted = TRUE)
}

# ---- Internal helpers: geometry ------------------------------------------

#' Stop with an informative message if linked-map dependencies are missing
#' @noRd
check_linked_deps <- function() {
  rlang::check_installed(c("plotly", "htmlwidgets", "sf", "jsonlite"),
                         reason = "for linked FHLUS maps.")
}

#' Polygon geometry aligned with the segment rows
#'
#' Returns an sf object with one row per segment (same order), a `mapped`
#' column, and empty geometry where a segment has no polygon.
#' @noRd
linked_geometry <- function(result, seg, geometry, id) {
  if (is.null(geometry)) {
    d <- result$data
    if (!inherits(d, "sf")) {
      stop("result$data has no geometry. Either pass sf data to fhlus_score() ",
           "(e.g. the output of prepare_spatial_fhlus()), or supply `geometry =` ",
           "an sf object with polygons and the ID column.")
    }
    d <- d[d$rank != 0, , drop = FALSE]
    d <- d[order(d$rank), , drop = FALSE]
    out <- sf::st_sf(mapped = rep(TRUE, nrow(d)), geometry = sf::st_geometry(d))
    return(out)
  }

  if (!inherits(geometry, "sf")) {
    stop("geometry must be an sf object with polygons and the ID column.")
  }
  key <- if (!is.null(id)) id else attr(seg, "id_col")
  if (is.null(key)) {
    stop("To match `geometry` to the result, the result needs an ID column. ",
         "Set `id` to a column present in both (e.g. \"GEOID\").")
  }
  if (!key %in% names(geometry)) {
    stop(sprintf("ID column '%s' not found in geometry. Available columns: %s",
                 key, paste(setdiff(names(geometry), attr(geometry, "sf_column")),
                            collapse = ", ")))
  }
  geo_ids <- as.character(geometry[[key]])
  if (anyDuplicated(geo_ids)) {
    dups <- unique(geo_ids[duplicated(geo_ids)])
    stop(sprintf(paste0(
      "geometry has %d duplicated ID(s) in '%s' (e.g. %s). Combine multi-part ",
      "features first, e.g. dplyr::summarise(dplyr::group_by(geometry, %s))."),
      length(dups), key, paste(utils::head(dups, 3), collapse = ", "), key))
  }

  m <- match(seg$id, geo_ids)
  n_missing <- sum(is.na(m))
  if (n_missing > 0) {
    hint <- ""
    if (all(grepl("^[0-9]+$", c(seg$id, geo_ids))) &&
        !identical(range(nchar(seg$id)), range(nchar(geo_ids)))) {
      hint <- " The IDs differ in length; check for lost leading zeros."
    }
    warning(sprintf("%d of %d geographies in the result have no polygon in geometry; ",
                    n_missing, nrow(seg)),
            "they appear on the curve but not on the map.", hint, call. = FALSE)
  }
  n_extra <- length(setdiff(geo_ids, seg$id))
  if (n_extra > 0) {
    message(sprintf("%d polygon(s) in geometry are not in the result and are not mapped.",
                    n_extra))
  }
  if (all(is.na(m))) {
    stop("No IDs in the result match geometry$", key, ".")
  }

  g <- sf::st_geometry(geometry)
  empty <- sf::st_sfc(sf::st_polygon(), crs = sf::st_crs(g))
  geoms <- lapply(seq_along(m), function(i) if (is.na(m[i])) empty[[1]] else g[[m[i]]])
  sf::st_sf(mapped = !is.na(m),
            geometry = sf::st_sfc(geoms, crs = sf::st_crs(g)))
}

#' Validate, simplify and project geometry to WGS84
#' @noRd
clean_linked_geometry <- function(geom, simplify) {
  if (is.na(sf::st_crs(geom))) {
    stop("geometry has no coordinate reference system. Set one with sf::st_set_crs().")
  }
  geom$mapped <- geom$mapped & !sf::st_is_empty(geom)

  types <- as.character(sf::st_geometry_type(geom[geom$mapped, ]))
  if (!all(types %in% c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) {
    stop("Linked maps need polygon geometry. Found: ",
         paste(setdiff(unique(types), c("POLYGON", "MULTIPOLYGON")), collapse = ", "))
  }

  geom <- sf::st_transform(geom, 4326)
  idx <- which(geom$mapped)
  g <- sf::st_geometry(geom)
  valid <- sf::st_make_valid(g[idx])
  valid <- suppressWarnings(sf::st_collection_extract(valid, "POLYGON"))
  g[idx] <- sf::st_cast(valid, "MULTIPOLYGON")

  if (!isFALSE(simplify)) {
    tol <- simplify
    if (is.null(tol)) {
      n_vertices <- nrow(sf::st_coordinates(g[idx]))
      if (n_vertices > 1e5) {
        typical <- sqrt(stats::median(as.numeric(sf::st_area(g[idx]))))
        tol <- typical * 0.02
      }
    }
    if (!is.null(tol)) {
      if (!is.numeric(tol) || length(tol) != 1 || tol < 0) {
        stop("simplify must be NULL, FALSE, or a tolerance in meters.")
      }
      simple <- sf::st_simplify(g[idx], preserveTopology = TRUE, dTolerance = tol)
      keep <- !sf::st_is_empty(simple)
      g[idx[keep]] <- sf::st_cast(simple[keep], "MULTIPOLYGON")
    }
  }

  sf::st_geometry(geom) <- g
  geom$mapped <- geom$mapped & !sf::st_is_empty(geom)
  geom
}

#' GeoJSON text for the mapped polygons
#'
#' Each feature carries only `i`, its zero-based row in the segment table; the
#' widget looks up everything else by that index.
#' @noRd
geometry_to_geojson <- function(geom) {
  idx <- which(geom$mapped)
  out <- sf::st_sf(i = idx - 1L, geometry = sf::st_geometry(geom)[idx])
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(out, tmp, driver = "GeoJSON", quiet = TRUE,
               layer_options = c("COORDINATE_PRECISION=6", "RFC7946=YES"))
  # Class "json" makes htmlwidgets embed it verbatim instead of as a string
  structure(paste(readLines(tmp, warn = FALSE), collapse = ""), class = "json")
}

# ---- Internal helpers: colors --------------------------------------------

# Single-hue ramps, light to dark (blue for rank, orange for the numerator)
LINKED_RANK_RAMP <- c("#cce3fd", "#9dc8f7", "#6aabf1", "#338ced",
                      "#2370cd", "#1757ac", "#0e3d7e")
LINKED_NUM_RAMP <- c("#fde2cf", "#f9b48a", "#f47230", "#c4531a", "#8a3510")
LINKED_ZERO_COLOR <- "#e9e9e9"

#' Color layers for the map: one color per segment plus a legend
#' @noRd
linked_color_layers <- function(seg, meta) {
  list(
    rank_color_layer(seg, meta),
    numerator_color_layer(seg, meta)
  )
}

#' Rank layer: continuous ramp over rank position
#' @noRd
rank_color_layer <- function(seg, meta) {
  n <- nrow(seg)
  pos <- if (n > 1) (seg$rank - min(seg$rank)) / (max(seg$rank) - min(seg$rank)) else 0.5
  ramp <- grDevices::colorRamp(LINKED_RANK_RAMP, space = "Lab")
  cols <- grDevices::rgb(ramp(pos), maxColorValue = 255)

  rank_name <- paste(meta$rank_var, collapse = " + ")
  first <- meta$rank_var[1]
  ends <- c("Lower", "Higher")
  if (first %in% names(seg) && is.numeric(seg[[first]])) {
    lo <- seg[[first]][which.min(seg$rank)]
    hi <- seg[[first]][which.max(seg$rank)]
    ends <- c(paste0("Lower (", fmt_value(lo), ")"),
              paste0("Higher (", fmt_value(hi), ")"))
  }
  list(
    key = "rank",
    label = "Rank",
    colors = I(cols),
    legend = list(type = "gradient", title = html_escape(paste("Rank:", rank_name)),
                  colors = I(LINKED_RANK_RAMP), labels = I(html_escape(ends)))
  )
}

#' Numerator layer: quantile classes of the nonzero values, zero in grey
#' @noRd
numerator_color_layer <- function(seg, meta) {
  v <- seg[[meta$numerator]]
  cols <- rep(LINKED_ZERO_COLOR, length(v))
  items <- list()
  if (any(v == 0)) {
    items[[1]] <- list(color = LINKED_ZERO_COLOR, label = "0")
  }

  pos <- v[v > 0]
  if (length(pos) > 0) {
    k <- min(length(LINKED_NUM_RAMP), length(unique(pos)))
    breaks <- unique(stats::quantile(pos, probs = seq(0, 1, length.out = k + 1),
                                     names = FALSE, type = 7))
    if (length(breaks) < 2) breaks <- c(breaks, breaks)
    cls <- findInterval(v, breaks, rightmost.closed = TRUE, left.open = TRUE)
    cls[v > 0 & cls < 1] <- 1
    n_cls <- length(breaks) - 1
    pal <- LINKED_NUM_RAMP[unique(round(seq(1, length(LINKED_NUM_RAMP),
                                            length.out = n_cls)))]
    cols[v > 0] <- pal[pmin(cls[v > 0], length(pal))]
    for (j in seq_len(n_cls)) {
      lo <- if (j == 1) min(pos) else breaks[j]
      label <- if (fmt_value(lo) == fmt_value(breaks[j + 1])) {
        fmt_value(lo)
      } else {
        paste0(fmt_value(lo), " \u2013 ", fmt_value(breaks[j + 1]))
      }
      items[[length(items) + 1]] <- list(color = pal[j], label = label)
    }
  }
  list(
    key = "numerator",
    label = "Numerator",
    colors = I(cols),
    legend = list(type = "classes", title = html_escape(meta$numerator),
                  items = items)
  )
}

# ---- Internal helpers: dependencies --------------------------------------

#' plotly.js dependencies, taken from a plotly widget so the version matches
#' the installed plotly package
#' @noRd
plotly_js_dependencies <- function(p) {
  deps <- htmltools::findDependencies(htmltools::as.tags(p))
  keep <- vapply(deps, function(d) d$name %in% c("plotly-main", "plotly-htmlwidgets-css"),
                 logical(1))
  deps[keep]
}
