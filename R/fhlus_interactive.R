# Interactive FHLUS Visualization
#
# Per-geography segment data and interactive (plotly) versions of the FHLUS
# cumulative distribution plots, plus a helper to save them as standalone HTML.
#
# Layering:
#   fhlus_segments()            -> tidy table, one row per geography (no extra deps)
#   fhlus_interactive()         -> plotly htmlwidget for a single result
#   fhlus_compare_interactive() -> plotly htmlwidget overlaying two results
#   fhlus_save_html()           -> write any of the above to an HTML file
#
# Every hover point carries the geography ID in `customdata`, which is the key
# used to link the plot with other widgets (e.g. a map) in Shiny or crosstalk.

#' Per-Geography Segment Data for an FHLUS Result
#'
#' Converts an FHLUS result into one row per geography (tract, block group,
#' neighborhood, ...). On the cumulative distribution plot each geography is a
#' line segment: it starts where the previous geography ends and spans that
#' geography's share of the denominator (x) and of the numerator (y).
#'
#' The slope of a segment is its \code{concentration}: the geography's share of
#' the numerator divided by its share of the denominator. A value of 2 means
#' the geography holds twice its proportional share (e.g. twice as many units
#' as its share of land area would imply); 0.5 means half.
#'
#' @param result An object of class "fhlus" (from \code{fhlus_score()}).
#' @param id Column name identifying each geography (e.g. "GEOID"). If NULL,
#'   the first of "GEOID", "geoid", "GEOID20", "GEOID10" found in the data is
#'   used; failing that, the geography's rank position is used as its ID.
#' @param tooltip_vars Character vector of additional columns to carry along
#'   (and to show in interactive tooltips). These can be any column of the data
#'   passed to \code{fhlus_score()}, including variables not used for ranking.
#'   Name the elements to set display labels, e.g.
#'   \code{c("Median rent" = "med_rent", "Poverty rate" = "pov_rate")}.
#' @param numerator,denom,rank_var Column names used in the calculation. Only
#'   needed for results created before fhlus 0.3.0; newer results record them
#'   in \code{result$meta}.
#'
#' @return A tibble of class "fhlus_segments", ordered from lowest to highest
#'   rank, with columns:
#'   \item{id}{Geography identifier (character)}
#'   \item{rank}{Rank position (1 = lowest opportunity)}
#'   \item{cum_denom_start, cum_denom_end}{Cumulative share of the denominator
#'     at the start and end of the segment (x-axis)}
#'   \item{cum_var_start, cum_var_end}{Cumulative share of the numerator at
#'     the start and end of the segment (y-axis)}
#'   \item{share_denom, share_var}{The geography's own share of the
#'     denominator and numerator}
#'   \item{concentration}{\code{share_var / share_denom}; NA when the
#'     denominator share is zero}
#'   \item{cum_var_lowest, cum_var_highest}{Cumulative numerator share at the
#'     end of the segment under the lowest/highest CI scenarios (only when the
#'     result has confidence intervals)}
#'   Followed by the raw values of the rank variable(s), numerator, denominator,
#'   excluded area and tie breakers (when used), and any \code{tooltip_vars}.
#'
#' @examples
#' sample_data <- data.frame(
#'   GEOID = sprintf("06037%06d", 1:10),
#'   housing_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
#'   median_income = c(45000, 62000, 38000, 71000, 55000,
#'                     82000, 48000, 66000, 59000, 74000),
#'   land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8),
#'   median_rent = c(1400, 1750, 1200, 2100, 1600, 2500, 1350, 1900, 1700, 2200)
#' )
#' result <- fhlus_score(sample_data, numerator = "housing_units",
#'                       rank_var = "median_income", denom = "land_area",
#'                       graph = FALSE)
#' fhlus_segments(result, tooltip_vars = c("Median rent" = "median_rent"))
#'
#' @export
fhlus_segments <- function(result, id = NULL, tooltip_vars = NULL,
                           numerator = NULL, denom = NULL, rank_var = NULL) {
  if (!inherits(result, "fhlus")) {
    stop("result must be an object of class 'fhlus' (from fhlus_score()). Got: ",
         paste(class(result), collapse = ", "))
  }

  meta <- resolve_fhlus_meta(result, numerator, denom, rank_var)
  d <- strip_zero_row(result$data)
  id_col <- resolve_id_col(d, id)
  tooltip_vars <- resolve_tooltip_vars(d, tooltip_vars)

  n <- nrow(d)
  x1 <- d$cumulativeDenom
  y1 <- d$cumulativeVariable

  seg <- tibble::tibble(
    id = if (is.null(id_col)) as.character(d$rank) else as.character(d[[id_col]]),
    rank = d$rank,
    cum_denom_start = c(0, x1[-n]),
    cum_denom_end = x1,
    cum_var_start = c(0, y1[-n]),
    cum_var_end = y1,
    share_denom = d$share_denominator,
    share_var = d$share_variable,
    concentration = ifelse(d$share_denominator > 0,
                           d$share_variable / d$share_denominator,
                           NA_real_)
  )

  if (!is.null(result$data_lowest) && !is.null(result$data_highest)) {
    seg$cum_var_lowest <- strip_zero_row(result$data_lowest)$cumulativeVariable
    seg$cum_var_highest <- strip_zero_row(result$data_highest)$cumulativeVariable
  }

  # Raw values: variables used in the calculation, then requested extras
  raw_cols <- unique(c(meta$rank_var, meta$numerator, meta$denom,
                       meta$exclude_area, meta$tie_breaker,
                       unname(tooltip_vars)))
  for (col in setdiff(raw_cols, names(seg))) {
    if (col %in% names(d)) seg[[col]] <- d[[col]]
  }

  attr(seg, "fhlus_meta") <- meta
  attr(seg, "id_col") <- id_col
  attr(seg, "tooltip_vars") <- tooltip_vars
  class(seg) <- c("fhlus_segments", class(seg))
  seg
}

#' Interactive FHLUS Plot
#'
#' Builds an interactive version of the FHLUS cumulative distribution plot.
#' Hovering over the curve shows the geography under the cursor: its ID, rank,
#' raw rank-variable, numerator and denominator values, its shares and
#' concentration, the cumulative percentages at the start and end of its
#' segment, and any extra \code{tooltip_vars}. The hovered segment is
#' highlighted on the curve and its slice of the x-axis is shaded.
#'
#' The result is an htmlwidget: it prints in the RStudio/Positron viewer, embeds
#' in R Markdown/Quarto documents and Shiny apps, and can be saved as a
#' standalone HTML file with \code{fhlus_save_html()}.
#'
#' Requires the \pkg{plotly} and \pkg{htmlwidgets} packages.
#'
#' @inheritParams fhlus_segments
#' @param title Plot title.
#' @param subtitle Plot subtitle, or NULL for "Distribution of <numerator>".
#' @param show_ci_bands Logical, whether to draw the CI ribbon band (requires a
#'   result computed with \code{missing} or \code{missing_pct}).
#' @param ci_alpha Transparency of the CI ribbon band (0-1).
#' @param color_var,color_denom Colors of the cumulative numerator and
#'   cumulative denominator lines.
#' @param boundary_label Legend label for the geography boundary points, or
#'   NULL to hide them.
#' @param annotation Score annotation: TRUE shows "FHLUS = X", FALSE/NULL hides
#'   it, a character string is shown as-is.
#' @param annotation_x,annotation_y Position of the score annotation (0-1).
#' @param highlight Logical, whether to highlight the hovered segment.
#' @param width,height Widget size in pixels, or NULL to fill the container.
#' @param source Plotly event source ID, used to read hover/click events in
#'   Shiny via \code{plotly::event_data(source = ...)}.
#'
#' @return A plotly htmlwidget.
#'
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   sample_data <- data.frame(
#'     GEOID = sprintf("06037%06d", 1:10),
#'     housing_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
#'     median_income = c(45000, 62000, 38000, 71000, 55000,
#'                       82000, 48000, 66000, 59000, 74000),
#'     land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8),
#'     median_rent = c(1400, 1750, 1200, 2100, 1600, 2500, 1350, 1900, 1700, 2200)
#'   )
#'   result <- fhlus_score(sample_data, numerator = "housing_units",
#'                         rank_var = "median_income", denom = "land_area",
#'                         missing_pct = 0.10, graph = FALSE)
#'   fhlus_interactive(result, tooltip_vars = c("Median rent" = "median_rent"),
#'                     show_ci_bands = TRUE)
#' }
#'
#' @export
fhlus_interactive <- function(result,
                              id = NULL,
                              tooltip_vars = NULL,
                              title = "Fair Housing Land Use Score",
                              subtitle = NULL,
                              show_ci_bands = FALSE,
                              ci_alpha = 0.15,
                              color_var = "red",
                              color_denom = "blue",
                              boundary_label = "Tract Boundary",
                              annotation = TRUE,
                              annotation_x = 0.125,
                              annotation_y = 0.93,
                              highlight = TRUE,
                              width = NULL,
                              height = NULL,
                              source = "fhlus",
                              numerator = NULL,
                              denom = NULL,
                              rank_var = NULL) {
  check_interactive_deps()

  seg <- fhlus_segments(result, id = id, tooltip_vars = tooltip_vars,
                        numerator = numerator, denom = denom, rank_var = rank_var)
  meta <- attr(seg, "fhlus_meta")
  score <- base_score(result)
  has_ci <- "cum_var_lowest" %in% names(seg)

  if (show_ci_bands && !has_ci) {
    warning("show_ci_bands = TRUE but no CI data available. ",
            "Specify missing or missing_pct in fhlus_score() to generate CI curves.")
  }

  var_label <- paste("Cumulative", meta$numerator)
  denom_label <- paste("Cumulative", meta$denom)
  use_gl <- nrow(seg) > 2000

  p <- plotly::plot_ly(width = width, height = height, source = source)

  if (show_ci_bands && has_ci) {
    p <- add_ci_ribbon(p, seg, color_var, ci_alpha, name = "CI band",
                       legendgroup = "var")
  }

  x_pts <- c(0, seg$cum_denom_end)
  p <- plotly::add_trace(
    p, type = "scatter", mode = "lines",
    x = x_pts, y = x_pts,
    name = denom_label, legendgroup = "denom",
    line = list(color = color_denom, width = 2),
    hoverinfo = "skip"
  )
  p <- plotly::add_trace(
    p, type = "scatter", mode = "lines",
    x = x_pts, y = c(0, seg$cum_var_end),
    name = var_label, legendgroup = "var",
    line = list(color = color_var, width = 2),
    hoverinfo = "skip"
  )
  if (!is.null(boundary_label) && nchar(boundary_label) > 0) {
    p <- plotly::add_trace(
      p, type = "scatter", mode = "markers",
      x = seg$cum_denom_end, y = seg$cum_denom_end,
      name = boundary_label,
      marker = list(color = "black", size = 5),
      hoverinfo = "skip"
    )
  }
  p <- add_hover_layer(p, seg, build_tooltips(seg), color_var,
                       name = var_label, use_gl = use_gl)

  annotations <- c(
    list(score_annotation(annotation, score, annotation_x, annotation_y)),
    rank_annotations(meta$rank_var),
    ci_annotations(result)
  )
  annotations <- Filter(Negate(is.null), annotations)

  p <- layout_fhlus(
    p,
    title = title,
    subtitle = if (!is.null(subtitle)) subtitle else paste("Distribution of", meta$numerator),
    x_title = paste("Cumulative", meta$denom),
    y_title = "Cumulative Percent",
    annotations = annotations
  )

  if (highlight) {
    p <- add_highlight_js(p, list(curve_payload(seg, color_var)))
  }
  p
}

#' Interactive FHLUS Policy Comparison Plot
#'
#' Interactive counterpart to \code{fhlus_compare()}: overlays the cumulative
#' distribution curves of two FHLUS results. Hovering shows a single combined
#' pop-up for the geography under the cursor: shared details (ID, rank, rank
#' variable, denominator, extra \code{tooltip_vars}) once, a short block per
#' policy (value, share of total, concentration, cumulative share), and the
#' difference between them. The geography's segment is highlighted on both
#' curves.
#'
#' Requires the \pkg{plotly} and \pkg{htmlwidgets} packages.
#'
#' @param result_a,result_b Objects of class "fhlus" (from \code{fhlus_score()}).
#' @param label_a,label_b Labels for the two results.
#' @param id,tooltip_vars Passed to \code{fhlus_segments()} for both results.
#' @param title Plot title.
#' @param subtitle Plot subtitle, or NULL for "Cumulative distribution comparison".
#' @param color_a,color_b Curve colors.
#' @param show_reference Logical, whether to draw the proportional (diagonal)
#'   reference line.
#' @param show_ci Logical, whether to draw CI ribbon bands for results that
#'   have them.
#' @param ci_alpha Transparency of the CI ribbon bands (0-1).
#' @param rank_var Ranking variable name(s) for the "Lower/Higher" axis labels.
#'   Defaults to the rank variable recorded in \code{result_a}; set to FALSE to
#'   hide the labels.
#' @inheritParams fhlus_interactive
#'
#' @return A plotly htmlwidget.
#'
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   df <- data.frame(
#'     GEOID = sprintf("06037%06d", 1:10),
#'     all_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
#'     affordable_units = c(30, 20, 60, 10, 25, 5, 40, 15, 20, 10),
#'     median_income = c(45000, 62000, 38000, 71000, 55000,
#'                       82000, 48000, 66000, 59000, 74000),
#'     land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8)
#'   )
#'   r1 <- fhlus_score(df, "all_units", "median_income", "land_area", graph = FALSE)
#'   r2 <- fhlus_score(df, "affordable_units", "median_income", "land_area",
#'                     graph = FALSE)
#'   fhlus_compare_interactive(r1, r2, label_a = "All Housing",
#'                             label_b = "Affordable Housing")
#' }
#'
#' @export
fhlus_compare_interactive <- function(result_a, result_b,
                                      label_a = "Policy A",
                                      label_b = "Policy B",
                                      id = NULL,
                                      tooltip_vars = NULL,
                                      title = "FHLUS Policy Comparison",
                                      subtitle = NULL,
                                      color_a = "#E41A1C",
                                      color_b = "#377EB8",
                                      show_reference = TRUE,
                                      show_ci = FALSE,
                                      ci_alpha = 0.15,
                                      rank_var = NULL,
                                      highlight = TRUE,
                                      width = NULL,
                                      height = NULL,
                                      source = "fhlus") {
  check_interactive_deps()

  for (nm in c("result_a", "result_b")) {
    r <- get(nm)
    if (!inherits(r, "fhlus")) {
      stop(nm, " must be an object of class 'fhlus' (from fhlus_score()). Got: ",
           paste(class(r), collapse = ", "))
    }
  }

  seg_a <- fhlus_segments(result_a, id = id, tooltip_vars = tooltip_vars)
  seg_b <- fhlus_segments(result_b, id = id, tooltip_vars = tooltip_vars)
  score_a <- base_score(result_a)
  score_b <- base_score(result_b)
  difference <- score_a - score_b

  has_ci_a <- "cum_var_lowest" %in% names(seg_a)
  has_ci_b <- "cum_var_lowest" %in% names(seg_b)
  if (show_ci && !has_ci_a && !has_ci_b) {
    warning("show_ci = TRUE but neither result has CI data. ",
            "Rerun fhlus_score() with missing or missing_pct to generate CI curves. ",
            "Skipping CI bands.")
  } else if (show_ci && xor(has_ci_a, has_ci_b)) {
    warning("show_ci = TRUE but only one result has CI data ('",
            if (has_ci_a) label_a else label_b, "'). Showing that band only.")
  }

  legend_a <- sprintf("%s (FHLUS = %s)", label_a, round(score_a, 2))
  legend_b <- sprintf("%s (FHLUS = %s)", label_b, round(score_b, 2))
  use_gl <- max(nrow(seg_a), nrow(seg_b)) > 2000

  p <- plotly::plot_ly(width = width, height = height, source = source)

  if (show_ci && has_ci_a) {
    p <- add_ci_ribbon(p, seg_a, color_a, ci_alpha,
                       name = paste(label_a, "CI"), legendgroup = "a")
  }
  if (show_ci && has_ci_b) {
    p <- add_ci_ribbon(p, seg_b, color_b, ci_alpha,
                       name = paste(label_b, "CI"), legendgroup = "b")
  }

  if (show_reference) {
    diff_sign <- if (difference >= 0) "+" else ""
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines",
      x = c(0, 1), y = c(0, 1),
      name = sprintf("Proportional (Diff: %s%s)", diff_sign, round(difference, 2)),
      line = list(color = "gray", width = 1, dash = "dash"),
      hoverinfo = "skip"
    )
  }

  for (curve in list(list(seg = seg_a, legend = legend_a, color = color_a, group = "a"),
                     list(seg = seg_b, legend = legend_b, color = color_b, group = "b"))) {
    p <- plotly::add_trace(
      p, type = "scatter", mode = "lines",
      x = c(0, curve$seg$cum_denom_end), y = c(0, curve$seg$cum_var_end),
      name = curve$legend, legendgroup = curve$group,
      line = list(color = curve$color, width = 2),
      hoverinfo = "skip"
    )
  }

  # One combined tooltip per geography. When both results share the same
  # geographies and denominator, the curves line up on the x-axis, so a single
  # hover layer (hovermode "x") gives exactly one pop-up per position. Otherwise
  # each curve gets its own layer and the nearest curve's geography is shown.
  tip_args <- list(seg_a = seg_a, seg_b = seg_b, label_a = label_a, label_b = label_b,
                   color_a = color_a, color_b = color_b)
  aligned <- identical(seg_a$id, seg_b$id) &&
    isTRUE(all.equal(seg_a$cum_denom_end, seg_b$cum_denom_end))

  p <- add_hover_layer(p, seg_a, do.call(build_compare_tooltips, c(tip_args, order_by = "a")),
                       "#8c8c8c", name = label_a, use_gl = use_gl)
  if (!aligned) {
    p <- add_hover_layer(p, seg_b, do.call(build_compare_tooltips, c(tip_args, order_by = "b")),
                         "#8c8c8c", name = label_b, use_gl = use_gl)
  }

  if (is.null(rank_var)) rank_var <- attr(seg_a, "fhlus_meta")$rank_var
  annotations <- if (isFALSE(rank_var)) list() else rank_annotations(rank_var)

  p <- layout_fhlus(
    p,
    title = title,
    subtitle = if (!is.null(subtitle)) subtitle else "Cumulative distribution comparison",
    x_title = "Cumulative Percent of Denominator",
    y_title = "Cumulative Percent of Variable",
    annotations = annotations
  )
  if (!aligned) {
    p <- plotly::layout(p, hovermode = "closest", hoverdistance = 40)
  }

  if (highlight) {
    p <- add_highlight_js(p, list(curve_payload(seg_a, color_a),
                                  curve_payload(seg_b, color_b)))
  }
  p
}

#' Save an Interactive FHLUS Plot as HTML
#'
#' Writes an interactive FHLUS plot to an HTML file that opens in any web
#' browser, without R. By default the file is fully self-contained (all
#' JavaScript and CSS embedded), so it can be emailed or posted as-is.
#'
#' @param x A widget from \code{fhlus_interactive()} or
#'   \code{fhlus_compare_interactive()}, or an "fhlus" result (in which case
#'   \code{fhlus_interactive(x, ...)} is called first).
#' @param file Path of the HTML file to write.
#' @param selfcontained Logical, whether to embed all dependencies in the one
#'   file. This needs pandoc (bundled with RStudio, Positron and Quarto); if
#'   pandoc is not found a warning is issued and the dependencies are written
#'   to a folder next to the file instead.
#' @param title Browser tab title.
#' @param libdir Folder for dependencies when \code{selfcontained = FALSE};
#'   defaults to "<file name>_files" next to the file.
#' @param ... Passed to \code{fhlus_interactive()} when \code{x} is an "fhlus"
#'   result (e.g. \code{id}, \code{tooltip_vars}, \code{show_ci_bands}).
#'
#' @return The path of the written file, invisibly.
#'
#' @examples
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   sample_data <- data.frame(
#'     GEOID = sprintf("06037%06d", 1:10),
#'     housing_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
#'     median_income = c(45000, 62000, 38000, 71000, 55000,
#'                       82000, 48000, 66000, 59000, 74000),
#'     land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8)
#'   )
#'   result <- fhlus_score(sample_data, "housing_units", "median_income",
#'                         "land_area", graph = FALSE)
#'   out <- fhlus_save_html(result, tempfile(fileext = ".html"),
#'                          selfcontained = FALSE)
#' }
#'
#' @export
fhlus_save_html <- function(x, file, selfcontained = TRUE,
                            title = "Fair Housing Land Use Score",
                            libdir = NULL, ...) {
  check_interactive_deps()

  if (inherits(x, "fhlus")) {
    x <- fhlus_interactive(x, ...)
  } else if (inherits(x, "fhlus_compare")) {
    stop("fhlus_compare objects do not keep the underlying data. ",
         "Use fhlus_compare_interactive(result_a, result_b) and save that widget.")
  } else if (!inherits(x, "htmlwidget")) {
    stop("x must be an htmlwidget (from fhlus_interactive() or ",
         "fhlus_compare_interactive()) or an 'fhlus' result.")
  }

  out_dir <- dirname(file)
  if (!dir.exists(out_dir)) {
    stop("Directory does not exist: ", out_dir)
  }
  # saveWidget() is unreliable with relative paths outside the working directory
  file <- file.path(normalizePath(out_dir), basename(file))

  if (selfcontained && !pandoc_found()) {
    warning("pandoc not found, so the HTML cannot be fully self-contained. ",
            "Writing dependencies to a folder next to the file instead. ",
            "Install pandoc (or run from RStudio/Positron) for a single-file export.")
    selfcontained <- FALSE
  }

  # saveWidget() can leave its temporary dependency folder behind after
  # embedding; remove it only if we were the ones who created it
  default_libdir <- file.path(dirname(file),
                              paste0(tools::file_path_sans_ext(basename(file)), "_files"))
  libdir_existed <- dir.exists(default_libdir)

  htmlwidgets::saveWidget(x, file = file, selfcontained = selfcontained,
                          libdir = libdir, title = title)

  if (selfcontained && is.null(libdir) && !libdir_existed) {
    unlink(default_libdir, recursive = TRUE)
  }
  invisible(file)
}

#' Print method for FHLUS segment tables
#' @param x An object of class "fhlus_segments".
#' @param ... Passed to the tibble print method.
#' @export
print.fhlus_segments <- function(x, ...) {
  meta <- attr(x, "fhlus_meta")
  cat(sprintf("FHLUS segments: %d geographies, %s ranked by %s\n",
              nrow(x), meta$numerator, paste(meta$rank_var, collapse = " + ")))
  y <- x
  class(y) <- setdiff(class(y), "fhlus_segments")
  print(y, ...)
  invisible(x)
}

# ---- Internal helpers: data ----------------------------------------------

#' Resolve the column names used to compute a result
#' @noRd
resolve_fhlus_meta <- function(result, numerator = NULL, denom = NULL, rank_var = NULL) {
  meta <- if (is.null(result$meta)) list() else result$meta
  if (!is.null(numerator)) meta$numerator <- numerator
  if (!is.null(denom)) meta$denom <- denom
  if (!is.null(rank_var)) meta$rank_var <- rank_var

  missing_meta <- c("numerator", "denom", "rank_var")[
    vapply(c("numerator", "denom", "rank_var"), function(k) is.null(meta[[k]]), logical(1))
  ]
  if (length(missing_meta) > 0) {
    stop("This result does not record its column names (it may predate fhlus 0.3.0). ",
         "Please supply: ", paste(missing_meta, collapse = ", "))
  }
  meta
}

#' Drop the synthetic zero row that compute_score() adds for the AUC
#' @noRd
strip_zero_row <- function(d) {
  if (inherits(d, "sf")) {
    d <- sf::st_drop_geometry(d)
  }
  d <- as.data.frame(d)
  d <- d[d$rank != 0, , drop = FALSE]
  d[order(d$rank), , drop = FALSE]
}

#' Pick the ID column
#' @noRd
resolve_id_col <- function(d, id) {
  if (is.null(id)) {
    candidates <- intersect(c("GEOID", "geoid", "GEOID20", "GEOID10"), names(d))
    return(if (length(candidates) > 0) candidates[1] else NULL)
  }
  if (!is.character(id) || length(id) != 1) {
    stop("id must be a single column name.")
  }
  if (!id %in% names(d)) {
    stop(sprintf("ID column '%s' not found in the result data. Available columns: %s",
                 id, paste(setdiff(names(d), fhlus_internal_cols()), collapse = ", ")))
  }
  if (anyDuplicated(d[[id]])) {
    warning(sprintf("ID column '%s' has duplicate values; hover linking may be ambiguous.", id))
  }
  id
}

#' Validate tooltip variables and fill in display labels
#' @noRd
resolve_tooltip_vars <- function(d, tooltip_vars) {
  if (is.null(tooltip_vars) || length(tooltip_vars) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  if (!is.character(tooltip_vars)) {
    stop("tooltip_vars must be a character vector of column names.")
  }
  missing_cols <- setdiff(tooltip_vars, names(d))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "tooltip_vars not found in the result data: %s. Available columns: %s",
      paste(missing_cols, collapse = ", "),
      paste(setdiff(names(d), fhlus_internal_cols()), collapse = ", ")
    ))
  }
  labels <- names(tooltip_vars)
  if (is.null(labels)) labels <- rep("", length(tooltip_vars))
  labels[labels == ""] <- tooltip_vars[labels == ""]
  stats::setNames(unname(tooltip_vars), labels)
}

#' Columns added by fhlus_score() itself
#' @noRd
fhlus_internal_cols <- function() {
  c("rank", "share_denominator", "share_variable",
    "cumulativeVariable", "cumulativeDenom")
}

#' Base score of a result
#' @noRd
base_score <- function(result) {
  result$scores$score[result$scores$type == "base"][1]
}

# ---- Internal helpers: tooltips ------------------------------------------

#' Build one HTML tooltip per segment (single result)
#' @noRd
build_tooltips <- function(seg) {
  meta <- attr(seg, "fhlus_meta")
  n <- nrow(seg)

  header <- list(
    tt_title(attr(seg, "id_col"), seg$id),
    tt_muted(sprintf("Rank %d of %d (1 = lowest %s)", as.integer(seg$rank), n,
                     html_escape(paste(meta$rank_var, collapse = " + "))))
  )

  values <- list()
  for (rv in intersect(meta$rank_var, names(seg))) {
    values <- c(values, list(tt_row(rv, fmt_value(seg[[rv]]))))
  }
  values <- c(values, list(
    tt_row(meta$numerator, fmt_value(seg[[meta$numerator]]),
           paste(fmt_pct(seg$share_var), "of total")),
    tt_row(meta$denom, fmt_value(seg[[meta$denom]]),
           paste(fmt_pct(seg$share_denom), "of total"))
  ))
  if (!is.null(meta$exclude_area) && meta$exclude_area %in% names(seg)) {
    values <- c(values, list(tt_row(paste(meta$exclude_area, "(excluded)"),
                                    fmt_value(seg[[meta$exclude_area]]))))
  }
  values <- c(values, list(
    tt_row("Concentration", fmt_conc(seg$concentration), "of proportional share")
  ))

  cumulative <- list(
    tt_row(paste("Cumulative", meta$denom),
           fmt_range(seg$cum_denom_start, seg$cum_denom_end)),
    tt_row(paste("Cumulative", meta$numerator),
           fmt_range(seg$cum_var_start, seg$cum_var_end),
           ci_note(seg))
  )

  tt_compose(list(header, values, cumulative,
                  tt_extra_rows(list(seg), attr(seg, "tooltip_vars"))))
}

#' Build one combined HTML tooltip per geography for a policy comparison
#'
#' Shared details (ID, rank, rank variables, denominator, extra variables) are
#' shown once, followed by a short block per policy and their difference.
#' Rows follow `seg_a` (order_by = "a") or `seg_b` (order_by = "b").
#' @noRd
build_compare_tooltips <- function(seg_a, seg_b, label_a, label_b,
                                   color_a, color_b, order_by = c("a", "b")) {
  order_by <- match.arg(order_by)
  primary <- if (order_by == "a") seg_a else seg_b
  ids <- primary$id
  ia <- match(ids, seg_a$id)
  ib <- match(ids, seg_b$id)
  a <- seg_a[ia, ]
  b <- seg_b[ib, ]
  in_a <- !is.na(ia)
  in_b <- !is.na(ib)
  meta_a <- attr(seg_a, "fhlus_meta")
  meta_b <- attr(seg_b, "fhlus_meta")
  n <- nrow(primary)

  # Header: ID and rank (shown once when both results rank identically)
  rank_label <- html_escape(paste(meta_a$rank_var, collapse = " + "))
  same_rank <- isTRUE(all(a$rank == b$rank, na.rm = TRUE))
  rank_line <- if (same_rank) {
    sprintf("Rank %d of %d (1 = lowest %s)", as.integer(primary$rank), n, rank_label)
  } else {
    sprintf("Rank %s (A) / %s (B) of %d", fmt_value(a$rank), fmt_value(b$rank), n)
  }
  header <- list(tt_title(attr(primary, "id_col"), ids), tt_muted(rank_line))

  # Shared values
  shared <- list()
  for (rv in meta_a$rank_var) {
    shared <- c(shared, list(tt_row(rv, coalesce_fmt(a, b, rv))))
  }
  same_denom <- identical(meta_a$denom, meta_b$denom) &&
    isTRUE(all.equal(a[[meta_a$denom]][in_a & in_b], b[[meta_b$denom]][in_a & in_b]))
  if (same_denom) {
    share_denom <- ifelse(in_a, a$share_denom, b$share_denom)
    shared <- c(shared, list(tt_row(meta_a$denom, coalesce_fmt(a, b, meta_a$denom),
                                    paste(fmt_pct(share_denom), "of total"))))
  }

  policy_block <- function(s, meta, label, color, present) {
    lines <- list(
      paste0("<span style='color:", color, "'>\u25cf</span> <b>", html_escape(label), "</b>"),
      tt_indent(tt_row(meta$numerator, fmt_value(s[[meta$numerator]]),
                       paste(fmt_pct(s$share_var), "of total"))),
      tt_indent(tt_row("Concentration", fmt_conc(s$concentration)))
    )
    if (!same_denom) {
      lines <- c(lines, list(tt_indent(tt_row(meta$denom, fmt_value(s[[meta$denom]]),
                                               paste(fmt_pct(s$share_denom), "of total")))))
    }
    lines <- c(lines, list(tt_indent(tt_row("Cumulative",
                                             fmt_range(s$cum_var_start, s$cum_var_end),
                                             ci_note(s)))))
    block <- do.call(paste, c(lines, sep = "<br>"))
    absent <- paste0(lines[[1]], "<br>", tt_indent(tt_muted("<i>Not in this result</i>")))
    ifelse(present, block, absent)
  }

  difference <- list(
    tt_muted(paste0("Difference (", html_escape(label_a), " \u2212 ",
                    html_escape(label_b), ")")),
    tt_indent(tt_row("Share of total", fmt_pts(a$share_var - b$share_var))),
    tt_indent(tt_row("Concentration", fmt_signed_conc(a$concentration - b$concentration))),
    tt_indent(tt_row("Cumulative gap", fmt_pts(a$cum_var_end - b$cum_var_end)))
  )

  tt_compose(list(
    header,
    shared,
    list(policy_block(a, meta_a, label_a, color_a, in_a)),
    list(policy_block(b, meta_b, label_b, color_b, in_b)),
    difference,
    tt_extra_rows(list(a, b), attr(seg_a, "tooltip_vars"))
  ))
}

# Plain spaces collapse in plotly's SVG hover text, so use an em space
TT_GAP <- "\u2003"

#' Grey text for labels and secondary information
#' @noRd
tt_muted <- function(x) {
  paste0("<span style='color:#6b6b6b'>", x, "</span>")
}

#' A "label  value  note" row: muted label, bold value, optional muted note
#' @noRd
tt_row <- function(label, value, note = NULL) {
  out <- paste0(tt_muted(html_escape(label)), TT_GAP, "<b>", value, "</b>")
  if (!is.null(note)) {
    out <- paste0(out, ifelse(nzchar(note), paste0(TT_GAP, tt_muted(note)), ""))
  }
  out
}

#' Indent a row within a block
#' @noRd
tt_indent <- function(x) paste0(TT_GAP, x)

#' Bold title line: "<id column> <id>"
#' @noRd
tt_title <- function(id_col, ids) {
  label <- if (is.null(id_col)) "Geography #" else paste0(id_col, " ")
  paste0("<b>", html_escape(label), html_escape(ids), "</b>")
}

#' Rows for the user-requested extra variables, taking the first non-NA value
#' across the given segment tables
#' @noRd
tt_extra_rows <- function(segs, tooltip_vars) {
  Map(function(col, lab) tt_row(lab, coalesce_fmt(segs[[1]],
                                                  if (length(segs) > 1) segs[[2]] else NULL,
                                                  col)),
      unname(tooltip_vars), names(tooltip_vars))
}

#' Join sections (lists of row vectors) into tooltips, with a blank line
#' between non-empty sections
#' @noRd
tt_compose <- function(sections) {
  sections <- Filter(function(s) length(s) > 0, sections)
  texts <- lapply(sections, function(s) do.call(paste, c(s, sep = "<br>")))
  do.call(paste, c(texts, sep = "<br><br>"))
}

#' Format a column from `a`, falling back to `b` where `a` is missing
#' @noRd
coalesce_fmt <- function(a, b, col) {
  n <- nrow(a)
  va <- if (col %in% names(a)) a[[col]] else rep(NA, n)
  out <- fmt_value(va)
  if (!is.null(b) && col %in% names(b)) {
    use_b <- is.na(va)
    out[use_b] <- fmt_value(b[[col]])[use_b]
  }
  out
}

#' CI note for the cumulative-numerator row ("" when there is no CI)
#' @noRd
ci_note <- function(seg) {
  if (!"cum_var_lowest" %in% names(seg)) return(NULL)
  lo <- pmin(seg$cum_var_lowest, seg$cum_var_highest)
  hi <- pmax(seg$cum_var_lowest, seg$cum_var_highest)
  ifelse(is.na(lo), "", paste0("CI ", fmt_pct(lo), "\u2013", fmt_pct(hi)))
}

#' "12.0% -> 15.3%"
#' @noRd
fmt_range <- function(start, end) {
  paste0(fmt_pct(start), " \u2192 ", fmt_pct(end))
}

#' Concentration as "1.25x"
#' @noRd
fmt_conc <- function(x) {
  ifelse(is.na(x), "n/a", paste0(formatC(x, format = "f", digits = 2), "\u00d7"))
}

#' Signed concentration difference as "+0.42x"
#' @noRd
fmt_signed_conc <- function(x) {
  ifelse(is.na(x), "n/a", paste0(sprintf("%+.2f", x), "\u00d7"))
}

#' Signed percentage-point difference as "+2.4 pts"
#' @noRd
fmt_pts <- function(x) {
  ifelse(is.na(x), "n/a", sprintf("%+.1f pts", 100 * x))
}

#' Format values for display
#' @noRd
fmt_value <- function(x) {
  if (is.numeric(x)) {
    vapply(x, function(v) {
      if (is.na(v)) return("NA")
      if (!is.finite(v)) return(as.character(v))
      if (abs(v - round(v)) < 1e-9) {
        formatC(round(v), format = "f", digits = 0, big.mark = ",")
      } else {
        formatC(signif(v, 4), format = "fg", digits = 4, big.mark = ",")
      }
    }, character(1))
  } else {
    out <- html_escape(as.character(x))
    out[is.na(x)] <- "NA"
    out
  }
}

#' Format proportions as percentages
#' @noRd
fmt_pct <- function(x) {
  ifelse(is.na(x), "NA", sprintf("%.1f%%", 100 * x))
}

#' Minimal HTML escaping for tooltip text
#' @noRd
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# ---- Internal helpers: plotly --------------------------------------------

#' Stop with an informative message if plotly/htmlwidgets are missing
#' @noRd
check_interactive_deps <- function() {
  rlang::check_installed(c("plotly", "htmlwidgets"),
                         reason = "for interactive FHLUS plots.")
}

#' Is pandoc available for self-contained HTML?
#' @noRd
pandoc_found <- function() {
  requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()
}

#' Points spaced along each segment so hovering anywhere on it finds it
#'
#' Plotly hovers on points, not lines. One point per segment midpoint is not
#' enough: near a boundary, the midpoint of a narrow neighbor can be closer
#' than the midpoint of the segment under the cursor. Spacing points every
#' `step` of the x-axis (at least one per segment) keeps the nearest point
#' inside the hovered segment.
#' @noRd
hover_points <- function(seg, step = 0.005) {
  width <- seg$cum_denom_end - seg$cum_denom_start
  k <- pmax(1L, as.integer(ceiling(width / step)))
  idx <- rep(seq_len(nrow(seg)), k)
  t <- (sequence(k) - 0.5) / k[idx]
  data.frame(
    idx = idx,
    x = seg$cum_denom_start[idx] + t * width[idx],
    y = seg$cum_var_start[idx] + t * (seg$cum_var_end[idx] - seg$cum_var_start[idx])
  )
}

#' Add the invisible hover-target layer for one curve
#' @noRd
add_hover_layer <- function(p, seg, tooltips, color, name, use_gl = FALSE) {
  pts <- hover_points(seg)
  plotly::add_trace(
    p, type = if (use_gl) "scattergl" else "scatter", mode = "markers",
    x = pts$x, y = pts$y,
    text = tooltips[pts$idx],
    customdata = seg$id[pts$idx],
    hovertemplate = "%{text}<extra></extra>",
    hoverlabel = list(bgcolor = "white", bordercolor = color, align = "left",
                      font = list(color = "#222222", size = 12)),
    marker = list(size = 12, color = "rgba(0,0,0,0)"),
    name = name, showlegend = FALSE
  )
}

#' Add a CI ribbon for one curve
#' @noRd
add_ci_ribbon <- function(p, seg, color, alpha, name, legendgroup) {
  plotly::add_ribbons(
    p,
    x = c(0, seg$cum_denom_end),
    ymin = c(0, pmin(seg$cum_var_lowest, seg$cum_var_highest)),
    ymax = c(0, pmax(seg$cum_var_lowest, seg$cum_var_highest)),
    fillcolor = to_rgba(color, alpha),
    line = list(width = 0),
    name = name, legendgroup = legendgroup,
    hoverinfo = "skip"
  )
}

#' Convert any R color to an rgba() string
#' @noRd
to_rgba <- function(color, alpha) {
  rgb <- grDevices::col2rgb(color)
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

#' Shared layout for FHLUS plots
#' @noRd
layout_fhlus <- function(p, title, subtitle, x_title, y_title, annotations) {
  axis <- list(range = c(0, 1), tickformat = ".0%", zeroline = FALSE,
               gridcolor = "#e8e8e8", linecolor = "#bbbbbb")
  p <- plotly::layout(
    p,
    title = list(
      text = paste0(html_escape(title), "<br><sup>", html_escape(subtitle), "</sup>"),
      x = 0.02, xanchor = "left"
    ),
    xaxis = c(axis, list(title = list(text = html_escape(x_title), standoff = 28))),
    yaxis = c(axis, list(title = list(text = html_escape(y_title)))),
    hovermode = "x",
    legend = list(x = 1.02, y = 1, xanchor = "left"),
    margin = list(t = 80, b = 100),
    annotations = annotations,
    plot_bgcolor = "white",
    paper_bgcolor = "white"
  )
  plotly::config(
    p,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
    toImageButtonOptions = list(format = "png", filename = "fhlus_plot",
                                width = 1000, height = 650)
  )
}

#' Score annotation (mirrors the static plot)
#' @noRd
score_annotation <- function(annotation, score, x, y) {
  label <- if (isTRUE(annotation)) {
    paste("FHLUS =", round(score, 2))
  } else if (is.character(annotation)) {
    annotation
  } else {
    return(NULL)
  }
  list(text = html_escape(label), x = x, y = y, xref = "x", yref = "y",
       showarrow = FALSE, font = list(size = 14))
}

#' "Lower / Higher <rank_var>" labels under the x-axis
#' @noRd
rank_annotations <- function(rank_var) {
  if (is.null(rank_var)) return(list())
  rank_display <- html_escape(paste(rank_var, collapse = " + "))
  base <- list(xref = "paper", yref = "paper", y = -0.09, yanchor = "top",
               showarrow = FALSE, font = list(size = 12, color = "#555555"))
  list(
    c(base, list(text = paste0("<i>Lower ", rank_display, "</i>"), x = 0, xanchor = "left")),
    c(base, list(text = paste0("<i>Higher ", rank_display, "</i>"), x = 1, xanchor = "right"))
  )
}

#' MOE and CI text (mirrors the static plot)
#' @noRd
ci_annotations <- function(result) {
  if (is.null(result$confidence_interval)) return(list())
  pct_err <- result$scores$percent_error[result$scores$type == "base"][1]
  ci <- result$confidence_interval
  base <- list(xref = "x", yref = "y", x = 0.95, xanchor = "right",
               showarrow = FALSE, font = list(size = 12))
  list(
    c(base, list(y = 0.15, text = paste0("MOE = \u00b1 ", round(pct_err * 100, 2), "%"))),
    c(base, list(y = 0.08, text = sprintf("CI = (%s, %s)", round(ci[1], 2), round(ci[2], 2))))
  )
}

#' Segment geometry passed to the highlight JavaScript
#' @noRd
curve_payload <- function(seg, color) {
  list(
    color = color,
    id = I(seg$id),
    x0 = I(seg$cum_denom_start), x1 = I(seg$cum_denom_end),
    y0 = I(seg$cum_var_start), y1 = I(seg$cum_var_end)
  )
}

#' Highlight the hovered segment(s) and shade their slice of the x-axis
#'
#' Works in the viewer, in saved HTML and in Shiny. Highlighting is by ID, so
#' in a comparison the same geography lights up on both curves.
#' @noRd
add_highlight_js <- function(p, curves) {
  js <- "
function(el, x, data) {
  var baseShapes = (el.layout.shapes || []).slice();
  el.on('plotly_hover', function(ev) {
    var ids = {};
    ev.points.forEach(function(pt) {
      if (pt.customdata !== undefined && pt.customdata !== null) ids[String(pt.customdata)] = true;
    });
    var shapes = baseShapes.slice();
    var bands = {};
    data.curves.forEach(function(c) {
      for (var i = 0; i < c.id.length; i++) {
        if (!ids[String(c.id[i])]) continue;
        var key = c.x0[i] + ':' + c.x1[i];
        if (!bands[key]) {
          bands[key] = true;
          shapes.push({type: 'rect', xref: 'x', yref: 'paper', layer: 'below',
                       x0: c.x0[i], x1: c.x1[i], y0: 0, y1: 1,
                       fillcolor: '#9e9e9e', opacity: 0.18, line: {width: 0}});
        }
        shapes.push({type: 'line', xref: 'x', yref: 'y',
                     x0: c.x0[i], y0: c.y0[i], x1: c.x1[i], y1: c.y1[i],
                     line: {color: c.color, width: 7}, opacity: 0.55});
      }
    });
    Plotly.relayout(el, {shapes: shapes});
  });
  el.on('plotly_unhover', function() {
    Plotly.relayout(el, {shapes: baseShapes});
  });
}"
  htmlwidgets::onRender(p, js, data = list(curves = curves))
}
