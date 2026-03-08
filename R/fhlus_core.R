#' Core FHLUS Calculation Functions
#'
#' @description Functions for calculating the Fair Housing Land Use Score
#' @importFrom dplyr mutate arrange bind_rows case_when sym syms filter select group_by summarise left_join
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_color_manual ylab xlab scale_y_continuous scale_x_continuous annotate coord_cartesian theme_minimal theme margin ggsave element_blank geom_bar geom_hline scale_fill_manual coord_flip labs geom_text
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom scales percent
#' @importFrom DescTools AUC
#' @importFrom rlang sym syms :=

#' Calculate FHLUS Score
#'
#' @param df Data frame containing the variables
#' @param numerator Column name for the variable of interest (e.g., housing units)
#' @param rank_var Column name or vector of column names for ranking (e.g., income). Higher values = better opportunity.
#' @param denom Column name for denominator (e.g., land area)
#' @param missing Numeric value for margin of error (absolute units) or NULL
#' @param missing_pct Percentage for margin of error (0-1) or NULL. Overrides missing if both provided.
#' @param graph Logical, whether to produce a visualization
#' @param exclude_area Column name for areas to exclude (e.g., open space) or NULL
#' @param tie_breaker Vector of column names to use as tie breakers in ranking
#' @param boundary_label Label for boundary points on plot, or NULL to hide them
#' @param plot_subtitle Custom subtitle for the plot, or NULL for default. Used by
#'   \code{grouped_fhlus()} to label each group's plot with the group name.
#' @param show_ci_bands Logical, whether to draw CI ribbon bands on the plot
#'   when confidence interval data is available (default: FALSE). Requires
#'   \code{missing} or \code{missing_pct} to be specified.
#' @param ci_alpha Numeric, transparency for CI ribbon bands (default: 0.15).
#'   Only used when \code{show_ci_bands = TRUE}.
#' @param verbose Logical, whether to print warnings and messages
#'
#' @return A list of class "fhlus" with components:
#'   \item{scores}{Data frame with base, lowest, highest FHLUS scores}
#'   \item{data}{Processed data frame with cumulative percentages}
#'   \item{plot}{ggplot2 object if graph=TRUE, NULL otherwise}
#'   \item{auc}{Numeric, area under the curve value}
#'   \item{confidence_interval}{Numeric vector of length 2 with CI bounds, or NULL}
#'   \item{data_lowest}{Data frame with CI lowest-bound cumulative distributions, or NULL}
#'   \item{data_highest}{Data frame with CI highest-bound cumulative distributions, or NULL}
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   tract_id = 1:10,
#'   housing_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
#'   median_income = c(45000, 62000, 38000, 71000, 55000,
#'                     82000, 48000, 66000, 59000, 74000),
#'   land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8)
#' )
#'
#' # Basic FHLUS calculation
#' result <- fhlus_score(
#'   df = sample_data,
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area",
#'   graph = FALSE
#' )
#'
#' print(result)
#' result$scores
#'
#' # With confidence intervals (10% margin of error)
#' result_ci <- fhlus_score(
#'   df = sample_data,
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area",
#'   missing_pct = 0.10,
#'   graph = FALSE
#' )
#'
#' result_ci$confidence_interval
#'
#' @export
fhlus_score <- function(df,
                        numerator,
                        rank_var,
                        denom,
                        missing = NULL,
                        missing_pct = NULL,
                        graph = TRUE,
                        exclude_area = NULL,
                        tie_breaker = NULL,
                        boundary_label = "Tract Boundary",
                        plot_subtitle = NULL,
                        show_ci_bands = FALSE,
                        ci_alpha = 0.15,
                        verbose = TRUE) {

  # Input validation
  validate_inputs(df, numerator, rank_var, denom, verbose)

  # Adjust denominator for excluded areas if specified
  if (!is.null(exclude_area) && exclude_area %in% names(df)) {
    df <- adjust_for_excluded_area(df, denom, exclude_area, verbose)
  }

  # Handle ranking with potential ties
  df <- create_ranking(df, rank_var, tie_breaker, verbose)

  # Calculate missing if percentage provided
  if (!is.null(missing_pct)) {
    if (!is.null(missing) && verbose) {
      warning("Both missing and missing_pct provided. Using missing_pct.")
    }
    total_numerator <- sum(df[[numerator]], na.rm = TRUE)
    missing <- total_numerator * missing_pct
  }

  # Calculate base score
  base_result <- compute_score(df, numerator, "rank", denom)

  # Calculate confidence intervals if missing data specified
  ci_results <- NULL
  if (!is.null(missing) && missing > 0) {
    ci_results <- calculate_confidence_intervals(df, numerator, denom, missing, base_result)
  }

  # Create visualization
  plot_obj <- NULL
  if (graph) {
    plot_obj <- create_fhlus_plot(base_result, ci_results, missing,
                                    numerator, denom, rank_var, boundary_label,
                                    plot_subtitle, show_ci_bands, ci_alpha)
    print(plot_obj)
  }

  # Compile results
  results <- compile_results(base_result, ci_results, missing, plot_obj)

  return(results)
}

#' Validate inputs for FHLUS calculation
#' @noRd
validate_inputs <- function(df, numerator, rank_var, denom, verbose) {

  # Check for empty data frame
  if (nrow(df) == 0) {
    stop("Input data frame is empty (0 rows). Cannot compute FHLUS.")
  }

  # Check for minimum rows
  if (nrow(df) == 1) {
    stop("FHLUS requires at least 2 rows of data for meaningful calculation.")
  }

  # Check if required columns exist
  required_cols <- c(numerator, denom)
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(sprintf(
      "Missing required columns: %s. Available columns: %s",
      paste(missing_cols, collapse = ", "),
      paste(names(df), collapse = ", ")
    ))
  }

  # Check rank variables
  rank_cols <- if(length(rank_var) == 1) rank_var else rank_var
  missing_rank <- setdiff(rank_cols, names(df))

  if (length(missing_rank) > 0) {
    stop(sprintf(
      "Missing rank columns: %s. Available columns: %s",
      paste(missing_rank, collapse = ", "),
      paste(names(df), collapse = ", ")
    ))
  }

  # Check numeric types for numerator
  if (!is.numeric(df[[numerator]])) {
    stop(sprintf(
      "Column '%s' must be numeric, got %s. Use validate_fhlus_data() to check requirements.",
      numerator,
      class(df[[numerator]])[1]
    ))
  }

  # Check numeric types for denominator
  if (!is.numeric(df[[denom]])) {
    stop(sprintf(
      "Column '%s' must be numeric, got %s. Use validate_fhlus_data() to check requirements.",
      denom,
      class(df[[denom]])[1]
    ))
  }

  # Check for all-NA columns
  for (col in c(numerator, denom)) {
    if (all(is.na(df[[col]]))) {
      stop(sprintf(
        "Column '%s' contains only NA values. Cannot compute FHLUS.",
        col
      ))
    }
  }

  # Check non-negative values in numerator
  if (any(df[[numerator]] < 0, na.rm = TRUE)) {
    stop(sprintf(
      "Column '%s' contains negative values. FHLUS requires non-negative numerator values.",
      numerator
    ))
  }

  # Check non-negative values in denominator
  if (any(df[[denom]] < 0, na.rm = TRUE)) {
    stop(sprintf(
      "Column '%s' contains negative values. FHLUS requires non-negative denominator values.",
      denom
    ))
  }

  # Check for NA values (warnings only)
  if (verbose) {
    na_counts <- sapply(df[c(numerator, rank_cols, denom)], function(x) sum(is.na(x)))
    if (any(na_counts > 0)) {
      warning(sprintf(
        "NA values found in: %s. These will be excluded from calculations.",
        paste(names(na_counts)[na_counts > 0], collapse = ", ")
      ))
    }
  }

  # Check for zero denominator (warnings only)
  if (any(df[[denom]] == 0, na.rm = TRUE) && verbose) {
    n_zeros <- sum(df[[denom]] == 0, na.rm = TRUE)
    warning(sprintf(
      "%d zero values found in denominator. These will be excluded from calculations.",
      n_zeros
    ))
  }
}

#' Adjust denominator for excluded areas
#' @noRd
adjust_for_excluded_area <- function(df, denom, exclude_area, verbose) {
  if (verbose) {
    message(paste("Adjusting", denom, "by excluding", exclude_area))
  }

  # Create adjusted denominator
  df[[paste0(denom, "_adjusted")]] <- df[[denom]] - df[[exclude_area]]

  # Check for negative values
  if (any(df[[paste0(denom, "_adjusted")]] < 0, na.rm = TRUE)) {
    if (verbose) {
      warning("Negative values after area adjustment. Setting to 0.")
    }
    df[[paste0(denom, "_adjusted")]][df[[paste0(denom, "_adjusted")]] < 0] <- 0
  }

  # Use adjusted denominator
  df[[denom]] <- df[[paste0(denom, "_adjusted")]]

  return(df)
}

#' Create ranking with tie-breakers
#' @noRd
create_ranking <- function(df, rank_var, tie_breaker = NULL, verbose = TRUE) {

  # Handle multiple ranking variables
  if (length(rank_var) > 1) {
    if (verbose) {
      message("Multiple ranking variables provided. Using hierarchical ranking.")
    }
    # Create composite ranking
    df <- dplyr::arrange(df, !!!rlang::syms(rank_var))
  } else {
    # Single ranking variable
    df <- dplyr::arrange(df, !!rlang::sym(rank_var))

    # Check for ties
    if (any(duplicated(df[[rank_var]])) && verbose) {
      n_ties <- sum(duplicated(df[[rank_var]]))
      warning(paste(n_ties, "tied values found in", rank_var))

      if (!is.null(tie_breaker)) {
        message(paste("Using tie breakers:", paste(tie_breaker, collapse = ", ")))
        # Apply tie breakers
        order_vars <- c(rank_var, tie_breaker)
        df <- dplyr::arrange(df, !!!rlang::syms(order_vars))
      }
    }
  }

  # Add explicit rank
  df$rank <- seq_len(nrow(df))

  return(df)
}

#' Core computation of FHLUS score
#' @noRd
compute_score <- function(df_calc, numerator, rank_col, denom) {

  # Ensure proper ordering
  df_calc <- df_calc[order(df_calc[[rank_col]]), ]

  # Calculate total
  total_variable <- sum(df_calc[[numerator]], na.rm = TRUE)

  if (total_variable == 0) {
    return(list(
      df = df_calc,
      score = NA_real_,
      total_variable = 0,
      auc = NA_real_
    ))
  }

  # Calculate shares with division by zero protection
  total_denom <- sum(df_calc[[denom]], na.rm = TRUE)

  if (total_denom == 0 || !is.finite(total_denom)) {
    stop(sprintf(
      "Denominator sum is zero or invalid (sum = %s). Cannot compute shares. Check your denominator column.",
      as.character(total_denom)
    ))
  }

  df_calc$share_denominator <- df_calc[[denom]] / total_denom
  df_calc$share_variable <- df_calc[[numerator]] / total_variable
  df_calc$cumulativeVariable <- cumsum(df_calc$share_variable)
  df_calc$cumulativeDenom <- cumsum(df_calc$share_denominator)

  # Add zero row for proper AUC calculation
  zero_row <- df_calc[1, ]
  zero_row[[rank_col]] <- 0
  zero_row[[numerator]] <- 0
  zero_row$cumulativeVariable <- 0
  zero_row$cumulativeDenom <- 0
  zero_row$share_variable <- 0
  zero_row$share_denominator <- 0

  df_calc <- dplyr::bind_rows(zero_row, df_calc)

  # Calculate AUC with error handling
  auc_value <- tryCatch(
    DescTools::AUC(df_calc$cumulativeDenom, df_calc$cumulativeVariable),
    error = function(e) {
      stop(sprintf(
        "AUC calculation failed: %s. This may indicate invalid data.",
        e$message
      ))
    }
  )

  if (!is.finite(auc_value)) {
    stop(sprintf(
      "AUC calculation produced non-finite value (%s). Check for invalid data.",
      as.character(auc_value)
    ))
  }

  # Calculate best and worst possible scores
  max_rank <- max(df_calc[[rank_col]])
  best <- 0.5 * df_calc$share_denominator[df_calc[[rank_col]] == max_rank][1]
  worst <- (0.5 * df_calc$share_denominator[df_calc[[rank_col]] == 1][1]) +
    (1 - df_calc$share_denominator[df_calc[[rank_col]] == 1][1])

  # Calculate FHLUS score with floating-point safe comparison
  eps <- .Machine$double.eps
  score <- dplyr::case_when(
    # Positive score: auc < 0.5 and denominator for best is non-zero
    (0.5 - auc_value) > 0 & abs(0.5 - best) > eps ~
      (0.5 - auc_value) / (0.5 - best),
    # Negative score: auc > 0.5 and denominator for worst is non-zero
    (0.5 - auc_value) < 0 & abs(0.5 - worst) > eps ~
      -(0.5 - auc_value) / (0.5 - worst),
    # Zero score: auc very close to 0.5
    abs(0.5 - auc_value) < eps ~ 0,
    # Fallback
    TRUE ~ NA_real_
  )

  return(list(
    df = df_calc,
    score = score,
    total_variable = total_variable,
    auc = auc_value
  ))
}

#' Calculate confidence intervals for FHLUS
#' @noRd
calculate_confidence_intervals <- function(df, numerator, denom, missing, base_result) {

  # Calculate lowest bound (missing units in lowest rank)
  df_low <- df
  df_low <- df_low[order(df_low$rank), ]
  df_low[[numerator]][1] <- df_low[[numerator]][1] + missing
  lowest <- compute_score(df_low, numerator, "rank", denom)

  # Calculate highest bound (missing units in highest rank)
  df_high <- df
  df_high <- df_high[order(df_high$rank), ]
  max_rank_idx <- which.max(df_high$rank)
  df_high[[numerator]][max_rank_idx] <- df_high[[numerator]][max_rank_idx] + missing
  highest <- compute_score(df_high, numerator, "rank", denom)

  # Calculate percent error
  p_err <- missing / (base_result$total_variable + missing)

  return(list(
    lowest = lowest,
    highest = highest,
    percent_error = p_err,
    confidence_interval = c(lowest$score, highest$score)
  ))
}

#' Create FHLUS visualization
#' @noRd
create_fhlus_plot <- function(base_result, ci_results = NULL, missing = NULL,
                               numerator = "Variable", denom = "Area",
                               rank_var = "Rank", boundary_label = "Tract Boundary",
                               plot_subtitle = NULL,
                               show_ci_bands = FALSE, ci_alpha = 0.15) {

  # Create dynamic labels
  var_label <- paste("Cumulative", numerator)
  area_label <- paste("Cumulative", denom)

  # Handle multiple ranking variables for display
  rank_display <- if (length(rank_var) > 1) {
    paste(rank_var, collapse = " + ")
  } else {
    rank_var
  }

  # Create base plot
  gg <- ggplot2::ggplot(base_result$df, ggplot2::aes(x = cumulativeDenom))

  # Add CI ribbon band (drawn first so lines render on top)
  has_ci <- !is.null(ci_results) && !is.null(missing) && missing > 0
  if (show_ci_bands && has_ci) {
    ribbon_df <- data.frame(
      cumulativeDenom = ci_results$lowest$df$cumulativeDenom,
      ci_lower = pmin(ci_results$lowest$df$cumulativeVariable,
                      ci_results$highest$df$cumulativeVariable),
      ci_upper = pmax(ci_results$lowest$df$cumulativeVariable,
                      ci_results$highest$df$cumulativeVariable)
    )
    gg <- gg +
      ggplot2::geom_ribbon(data = ribbon_df,
                           ggplot2::aes(x = cumulativeDenom,
                                        ymin = ci_lower, ymax = ci_upper),
                           fill = "red", alpha = ci_alpha)
  } else if (show_ci_bands && !has_ci) {
    warning("show_ci_bands = TRUE but no CI data available. ",
            "Specify missing or missing_pct to generate CI curves.")
  }

  gg <- gg +
    ggplot2::geom_line(ggplot2::aes(y = cumulativeVariable, color = var_label),
                       size = 0.75) +
    ggplot2::geom_line(ggplot2::aes(y = cumulativeDenom, color = area_label),
                       size = 0.75)

  # Add boundary points if label is provided
  if (!is.null(boundary_label) && nchar(boundary_label) > 0) {
    gg <- gg +
      ggplot2::geom_point(ggplot2::aes(y = cumulativeDenom, color = boundary_label))
  }

  # Set up color scale
  color_values <- c("red", "blue")
  names(color_values) <- c(var_label, area_label)

  if (!is.null(boundary_label) && nchar(boundary_label) > 0) {
    color_values <- c(color_values, "black")
    names(color_values)[3] <- boundary_label
  }

  gg <- gg +
    ggplot2::scale_color_manual("", values = color_values) +
    ggplot2::labs(
      title = "Fair Housing Land Use Score",
      subtitle = if (!is.null(plot_subtitle)) plot_subtitle else paste("Distribution of", numerator),
      y = "Cumulative Percent",
      x = paste("Cumulative", denom)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::annotate("text", x = 0.125, y = 0.93,
                      label = paste("FHLUS =", round(base_result$score, 2))) +
    # X-axis ranking annotations
    ggplot2::annotate("text", x = 0.05, y = -0.08,
                      label = paste("Lower", rank_display),
                      hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
    ggplot2::annotate("text", x = 0.95, y = -0.08,
                      label = paste("Higher", rank_display),
                      hjust = 1, vjust = 1, size = 3.5, fontface = "italic") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.margin = ggplot2::margin(20, 20, 50, 20))

  # Add confidence interval annotations if applicable (right side, below legend)
  if (!is.null(ci_results) && !is.null(missing) && missing > 0) {
    gg <- gg +
      ggplot2::annotate("text", x = 0.95, y = 0.15,
                        hjust = 1, vjust = 1, size = 3.5,
                        label = paste("MOE = \u00b1", round(ci_results$percent_error * 100, 2), "%")) +
      ggplot2::annotate("text", x = 0.95, y = 0.08,
                        hjust = 1, vjust = 1, size = 3.5,
                        label = paste0("CI = (",
                                       round(ci_results$confidence_interval[1], 2),
                                       ", ",
                                       round(ci_results$confidence_interval[2], 2), ")"))
  }

  return(gg)
}

#' Compile FHLUS results
#' @noRd
compile_results <- function(base_result, ci_results = NULL, missing = NULL, plot_obj = NULL) {

  # Create scores data frame
  if (!is.null(ci_results) && !is.null(missing) && missing > 0) {
    scores_df <- tibble::tibble(
      type = c("base", "lowest", "highest"),
      score = c(base_result$score,
                ci_results$lowest$score,
                ci_results$highest$score),
      percent_error = c(ci_results$percent_error, NA, NA)
    )
  } else {
    scores_df <- tibble::tibble(
      type = "base",
      score = base_result$score,
      percent_error = NA_real_
    )
  }

  # Return comprehensive results
  results <- list(
    scores = scores_df,
    data = base_result$df,
    plot = plot_obj,
    auc = base_result$auc,
    confidence_interval = if(!is.null(ci_results)) ci_results$confidence_interval else NULL,
    data_lowest = if(!is.null(ci_results)) ci_results$lowest$df else NULL,
    data_highest = if(!is.null(ci_results)) ci_results$highest$df else NULL
  )

  class(results) <- c("fhlus", "list")

  return(results)
}

#' Print method for FHLUS results
#' @export
print.fhlus <- function(x, ...) {
  cat("Fair Housing Land Use Score (FHLUS) Results\n")
  cat("============================================\n\n")

  cat("Score Summary:\n")
  print(x$scores, n = Inf)

  if (!is.null(x$confidence_interval)) {
    cat("\n95% Confidence Interval: [",
        round(x$confidence_interval[1], 3), ", ",
        round(x$confidence_interval[2], 3), "]\n", sep = "")
  }

  cat("\nAUC:", round(x$auc, 4), "\n")

  invisible(x)
}
