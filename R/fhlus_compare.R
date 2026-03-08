#' FHLUS Policy Comparison Functions
#'
#' @description Functions for comparing two FHLUS results on a single plot
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_abline scale_color_manual scale_fill_manual scale_y_continuous scale_x_continuous coord_cartesian annotate theme_minimal theme margin labs element_text guide_legend
#' @importFrom tibble tibble
#' @importFrom scales percent

#' Compare Two FHLUS Results
#'
#' Produces a combined visualization overlaying cumulative distribution curves
#' from two FHLUS results (e.g., two policy alternatives) on a single plot.
#' Optionally draws confidence interval bands when the input results were
#' computed with \code{missing} or \code{missing_pct}.
#'
#' @param result_a An object of class "fhlus" (from \code{fhlus_score()})
#' @param result_b An object of class "fhlus" (from \code{fhlus_score()})
#' @param label_a Character label for the first result (default: "Policy A")
#' @param label_b Character label for the second result (default: "Policy B")
#' @param title Plot title (default: "FHLUS Policy Comparison")
#' @param subtitle Plot subtitle, or NULL for auto-generated subtitle
#' @param color_a Color for the first result's curve (default: "#E41A1C")
#' @param color_b Color for the second result's curve (default: "#377EB8")
#' @param show_reference Logical, whether to show the proportional distribution
#'   reference line (default: TRUE)
#' @param show_ci Logical, whether to draw CI ribbon bands around each curve
#'   (default: FALSE). Requires that the input results were computed with
#'   \code{missing} or \code{missing_pct} so that CI curve data is available.
#'   If a result lacks CI data, a warning is issued and its band is skipped.
#' @param ci_alpha Numeric, transparency for CI ribbon bands (default: 0.15)
#' @param rank_var Character name of the ranking variable, used for axis
#'   annotations ("Lower rank_var" / "Higher rank_var"). If NULL, annotations
#'   are omitted.
#'
#' @return A list of class "fhlus_compare" with components:
#'   \item{plot}{ggplot2 object (customizable with \code{+})}
#'   \item{scores}{tibble with columns: label, score, score_lowest, score_highest, auc}
#'   \item{difference}{numeric (score_a - score_b)}
#'   \item{label_a}{stored label for result_a}
#'   \item{label_b}{stored label for result_b}
#'
#' @examples
#' source("R/fhlus_core.R")
#' source("R/fhlus_compare.R")
#'
#' # Without CI bands
#' result1 <- fhlus_score(df, numerator = "units", rank_var = "income",
#'                        denom = "area", graph = FALSE)
#' result2 <- fhlus_score(df, numerator = "affordable_units", rank_var = "income",
#'                        denom = "area", graph = FALSE)
#' comparison <- fhlus_compare(result1, result2,
#'                             label_a = "All Housing",
#'                             label_b = "Affordable Housing",
#'                             rank_var = "income")
#'
#' # With CI bands (results must have been computed with missing/missing_pct)
#' result1 <- fhlus_score(df, numerator = "units", rank_var = "income",
#'                        denom = "area", missing_pct = 0.10, graph = FALSE)
#' result2 <- fhlus_score(df, numerator = "affordable_units", rank_var = "income",
#'                        denom = "area", missing = 50, graph = FALSE)
#' comparison <- fhlus_compare(result1, result2, show_ci = TRUE,
#'                             label_a = "All Housing",
#'                             label_b = "Affordable Housing",
#'                             rank_var = "income")
#'
#' @export
fhlus_compare <- function(result_a, result_b,
                          label_a = "Policy A",
                          label_b = "Policy B",
                          title = "FHLUS Policy Comparison",
                          subtitle = NULL,
                          color_a = "#E41A1C",
                          color_b = "#377EB8",
                          show_reference = TRUE,
                          show_ci = FALSE,
                          ci_alpha = 0.15,
                          rank_var = NULL) {

  # Validate inputs
  if (!inherits(result_a, "fhlus")) {
    stop("result_a must be an object of class 'fhlus' (from fhlus_score()). Got: ",
         paste(class(result_a), collapse = ", "))
  }
  if (!inherits(result_b, "fhlus")) {
    stop("result_b must be an object of class 'fhlus' (from fhlus_score()). Got: ",
         paste(class(result_b), collapse = ", "))
  }

  # Check CI data availability
  has_ci_a <- !is.null(result_a$data_lowest) && !is.null(result_a$data_highest)
  has_ci_b <- !is.null(result_b$data_lowest) && !is.null(result_b$data_highest)

  if (show_ci) {
    if (!has_ci_a && !has_ci_b) {
      warning("show_ci = TRUE but neither result has CI data. ",
              "Rerun fhlus_score() with missing or missing_pct to generate CI curves. ",
              "Skipping CI bands.")
    } else if (!has_ci_a) {
      warning("show_ci = TRUE but result_a ('", label_a, "') lacks CI data. ",
              "Rerun fhlus_score() with missing or missing_pct. ",
              "Only showing CI band for '", label_b, "'.")
    } else if (!has_ci_b) {
      warning("show_ci = TRUE but result_b ('", label_b, "') lacks CI data. ",
              "Rerun fhlus_score() with missing or missing_pct. ",
              "Only showing CI band for '", label_a, "'.")
    }
  }

  # Extract base scores
  score_a <- dplyr::filter(result_a$scores, type == "base")$score
  score_b <- dplyr::filter(result_b$scores, type == "base")$score
  difference <- score_a - score_b

  # Extract CI scores if available
  score_lowest_a <- if (has_ci_a) dplyr::filter(result_a$scores, type == "lowest")$score else NA_real_
  score_highest_a <- if (has_ci_a) dplyr::filter(result_a$scores, type == "highest")$score else NA_real_
  score_lowest_b <- if (has_ci_b) dplyr::filter(result_b$scores, type == "lowest")$score else NA_real_
  score_highest_b <- if (has_ci_b) dplyr::filter(result_b$scores, type == "highest")$score else NA_real_

  # Extract cumulative distribution data
  data_a <- result_a$data
  data_b <- result_b$data

  # Build legend labels with embedded scores
  legend_a <- sprintf("%s (FHLUS = %s)", label_a, round(score_a, 2))
  legend_b <- sprintf("%s (FHLUS = %s)", label_b, round(score_b, 2))

  # Build difference text for legend
  diff_sign <- ifelse(difference >= 0, "+", "")
  diff_text <- sprintf("Diff: %s%s", diff_sign, round(difference, 2))

  # Auto-generate subtitle if not provided
  if (is.null(subtitle)) {
    subtitle <- "Cumulative distribution comparison"
  }

  # Start building the plot
  gg <- ggplot2::ggplot() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      y = "Cumulative Percent of Variable",
      x = "Cumulative Percent of Denominator"
    )

  # CI ribbon bands (drawn first so lines render on top)
  if (show_ci && has_ci_a) {
    ribbon_a <- build_ci_ribbon(result_a$data_lowest, result_a$data_highest)
    gg <- gg +
      ggplot2::geom_ribbon(data = ribbon_a,
                           ggplot2::aes(x = cumulativeDenom,
                                        ymin = ci_lower, ymax = ci_upper,
                                        fill = legend_a),
                           alpha = ci_alpha)
  }
  if (show_ci && has_ci_b) {
    ribbon_b <- build_ci_ribbon(result_b$data_lowest, result_b$data_highest)
    gg <- gg +
      ggplot2::geom_ribbon(data = ribbon_b,
                           ggplot2::aes(x = cumulativeDenom,
                                        ymin = ci_lower, ymax = ci_upper,
                                        fill = legend_b),
                           alpha = ci_alpha)
  }

  # Reference line (proportional distribution diagonal)
  if (show_reference) {
    ref_label <- sprintf("Proportional (%s)", diff_text)
    gg <- gg +
      ggplot2::geom_abline(ggplot2::aes(slope = 1, intercept = 0,
                                        color = ref_label),
                           linetype = "dashed", linewidth = 0.5,
                           show.legend = TRUE)
  }

  # Policy A curve
  gg <- gg +
    ggplot2::geom_line(data = data_a,
                       ggplot2::aes(x = cumulativeDenom,
                                    y = cumulativeVariable,
                                    color = legend_a),
                       linewidth = 0.9)

  # Policy B curve
  gg <- gg +
    ggplot2::geom_line(data = data_b,
                       ggplot2::aes(x = cumulativeDenom,
                                    y = cumulativeVariable,
                                    color = legend_b),
                       linewidth = 0.9)

  # Color scale (for lines)
  color_values <- stats::setNames(c(color_a, color_b), c(legend_a, legend_b))
  if (show_reference) {
    ref_label <- sprintf("Proportional (%s)", diff_text)
    color_values <- c(color_values, stats::setNames("gray50", ref_label))
  }

  gg <- gg +
    ggplot2::scale_color_manual("", values = color_values)

  # Fill scale (for CI ribbons) — only add if ribbons were drawn
  if (show_ci && (has_ci_a || has_ci_b)) {
    fill_values <- stats::setNames(c(color_a, color_b), c(legend_a, legend_b))
    gg <- gg +
      ggplot2::scale_fill_manual("", values = fill_values,
                                 guide = ggplot2::guide_legend(override.aes = list(alpha = 0.3)))
  }

  gg <- gg +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(20, 20, 50, 20),
      legend.position = "right",
      legend.text = ggplot2::element_text(size = 9)
    )

  # Ranking annotations below x-axis
  if (!is.null(rank_var)) {
    rank_display <- if (length(rank_var) > 1) {
      paste(rank_var, collapse = " + ")
    } else {
      rank_var
    }
    gg <- gg +
      ggplot2::annotate("text", x = 0.05, y = -0.08,
                        label = paste("Lower", rank_display),
                        hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
      ggplot2::annotate("text", x = 0.95, y = -0.08,
                        label = paste("Higher", rank_display),
                        hjust = 1, vjust = 1, size = 3.5, fontface = "italic")
  }

  # Compile scores tibble (includes CI bounds when available)
  scores_tbl <- tibble::tibble(
    label = c(label_a, label_b),
    score = c(score_a, score_b),
    score_lowest = c(score_lowest_a, score_lowest_b),
    score_highest = c(score_highest_a, score_highest_b),
    auc = c(result_a$auc, result_b$auc)
  )

  # Build return object
  result <- list(
    plot = gg,
    scores = scores_tbl,
    difference = difference,
    label_a = label_a,
    label_b = label_b
  )

  class(result) <- c("fhlus_compare", "list")

  return(result)
}

#' Build CI ribbon data from lowest and highest curve data frames
#'
#' Takes the data frames produced by compute_score() for the lowest and highest
#' CI bounds and returns a single data frame with columns cumulativeDenom,
#' ci_lower, and ci_upper suitable for geom_ribbon.
#'
#' @param data_lowest Data frame from CI lowest bound (compute_score()$df)
#' @param data_highest Data frame from CI highest bound (compute_score()$df)
#' @return Data frame with cumulativeDenom, ci_lower, ci_upper
#' @noRd
build_ci_ribbon <- function(data_lowest, data_highest) {
  # Both data frames share the same cumulativeDenom values (same denominator,
  # same ordering — only numerator was modified for CI calculation).
  # Use pmin/pmax to get the envelope at each point.
  data.frame(
    cumulativeDenom = data_lowest$cumulativeDenom,
    ci_lower = pmin(data_lowest$cumulativeVariable, data_highest$cumulativeVariable),
    ci_upper = pmax(data_lowest$cumulativeVariable, data_highest$cumulativeVariable)
  )
}

#' Print method for FHLUS comparison results
#' @export
print.fhlus_compare <- function(x, ...) {
  cat("FHLUS Policy Comparison\n")
  cat("========================\n")

  # Format score line with optional CI range
  format_score_line <- function(label, score, score_low, score_high) {
    score_str <- round(score, 2)
    if (!is.na(score_low) && !is.na(score_high)) {
      sprintf("%-12s %s  [CI: %s to %s]\n",
              paste0(label, ":"), score_str,
              round(score_low, 2), round(score_high, 2))
    } else {
      sprintf("%-12s %s\n", paste0(label, ":"), score_str)
    }
  }

  cat(format_score_line(x$label_a, x$scores$score[1],
                        x$scores$score_lowest[1], x$scores$score_highest[1]))
  cat(format_score_line(x$label_b, x$scores$score[2],
                        x$scores$score_lowest[2], x$scores$score_highest[2]))

  diff_sign <- ifelse(x$difference >= 0, "+", "")
  higher_label <- if (x$difference > 0) {
    x$label_a
  } else if (x$difference < 0) {
    x$label_b
  } else {
    NULL
  }
  diff_note <- if (!is.null(higher_label)) {
    sprintf(" (%s scores higher)", higher_label)
  } else {
    " (equal scores)"
  }
  cat(sprintf("Difference:  %s%s%s\n", diff_sign, round(x$difference, 2), diff_note))

  if (!is.null(x$plot)) {
    print(x$plot)
  }

  invisible(x)
}
