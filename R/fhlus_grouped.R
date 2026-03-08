#' Grouped FHLUS Calculation Functions
#'
#' @description Functions for calculating FHLUS scores by groups (e.g., cities)
#' @importFrom dplyr mutate arrange bind_rows case_when sym syms filter select group_by summarise left_join
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom rlang sym syms :=

#' Calculate FHLUS Scores by Group
#'
#' @param df Data frame containing the variables
#' @param group_var Column name for grouping variable (e.g., "city")
#' @param numerator Column name for the variable of interest
#' @param rank_var Column name or vector for ranking
#' @param denom Column name for denominator
#' @param missing Numeric value for margin of error (absolute units) applied to all groups,
#'   or a column name (string) in \code{df} containing per-group error values.
#' @param missing_pct Numeric percentage (0-1) for margin of error applied to all groups,
#'   or a column name (string) in \code{df} containing per-group error percentages.
#' @param exclude_area Column name for areas to exclude
#' @param tie_breaker Vector of column names for tie breaking
#' @param graph Logical, whether to create graphs. When TRUE, plots are printed and
#'   also stored as a named list in the \code{"plots"} attribute of the result.
#'   Access via \code{attr(result, "plots")} or \code{result_plots <- attr(result, "plots")}.
#' @param graph_subset Vector of group values to graph (NULL for all)
#' @param verbose Logical, whether to print progress messages
#' @param parallel Logical, whether to use parallel processing (requires furrr package)
#'
#' @return Data frame of class "grouped_fhlus" with one row per group in wide format.
#'   Columns include \code{score}, \code{score_lowest}, \code{score_highest},
#'   \code{percent_error}, and \code{error_reason}. When \code{graph=TRUE}, plot objects
#'   are stored in \code{attr(result, "plots")} as a named list keyed by group value.
#'
#' @examples
#' # Create sample data with multiple cities
#' sample_data <- data.frame(
#'   city = rep(c("City A", "City B", "City C"), each = 5),
#'   tract_id = 1:15,
#'   housing_units = sample(50:200, 15, replace = TRUE),
#'   median_income = sample(30000:90000, 15, replace = TRUE),
#'   land_area = runif(15, 1, 5)
#' )
#'
#' # Calculate FHLUS by city
#' grouped_results <- grouped_fhlus(
#'   df = sample_data,
#'   group_var = "city",
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area",
#'   graph = FALSE,
#'   verbose = FALSE
#' )
#'
#' print(grouped_results)
#'
#' @export
grouped_fhlus <- function(df,
                          group_var,
                          numerator,
                          rank_var,
                          denom,
                          missing = NULL,
                          missing_pct = NULL,
                          exclude_area = NULL,
                          tie_breaker = NULL,
                          graph = FALSE,
                          graph_subset = NULL,
                          verbose = TRUE,
                          parallel = FALSE) {

  # Validate group variable
  if (!group_var %in% names(df)) {
    stop(sprintf(
      "Group variable '%s' not found in data. Available columns: %s",
      group_var,
      paste(names(df), collapse = ", ")
    ))
  }

  # Get unique groups
  groups <- unique(df[[group_var]])
  groups <- groups[!is.na(groups)]

  if (length(groups) == 0) {
    stop("No valid groups found after removing NA values.")
  }

  if (verbose) {
    message(paste("Processing", length(groups), "groups"))
  }

  # Prepare missing data handling
  missing_values <- prepare_missing_by_group(df, group_var, missing, missing_pct)

  # Process each group
  if (parallel && requireNamespace("furrr", quietly = TRUE)) {
    processed <- process_groups_parallel(df, groups, group_var, numerator,
                                         rank_var, denom, missing_values,
                                         exclude_area, tie_breaker, graph,
                                         graph_subset, verbose)
  } else {
    if (parallel && verbose) {
      message("Package 'furrr' not available. Using sequential processing.")
    }
    processed <- process_groups_sequential(df, groups, group_var, numerator,
                                           rank_var, denom, missing_values,
                                           exclude_area, tie_breaker, graph,
                                           graph_subset, verbose)
  }

  # Combine score results (long format from fhlus_score)
  combined_long <- dplyr::bind_rows(processed$scores, .id = "group_id")

  # Pivot to wide format: one row per group with score, score_lowest, score_highest
  combined_results <- pivot_scores_wide(combined_long, group_var)

  # Add summary statistics
  combined_results <- add_summary_statistics(combined_results)

  # Attach plot objects as a named list attribute
  if (length(processed$plots) > 0) {
    attr(combined_results, "plots") <- processed$plots
  }

  class(combined_results) <- c("grouped_fhlus", "data.frame")

  return(combined_results)
}

#' Prepare missing data values by group
#' @noRd
prepare_missing_by_group <- function(df, group_var, missing, missing_pct) {

  groups <- unique(df[[group_var]])
  missing_values <- list()

  if (!is.null(missing)) {
    if (is.character(missing) && length(missing) == 1 && missing %in% names(df)) {
      # Column name provided - look up per-group values
      group_missing <- dplyr::select(df, !!rlang::sym(group_var), !!rlang::sym(missing))
      group_missing <- unique(group_missing)

      for (i in 1:nrow(group_missing)) {
        g <- as.character(group_missing[[group_var]][i])
        missing_values[[g]] <- group_missing[[missing]][i]
      }
    } else if (is.numeric(missing) && length(missing) == 1) {
      # Single numeric value for all groups
      for (g in groups) {
        missing_values[[as.character(g)]] <- missing
      }
    } else {
      stop(sprintf(
        "Invalid missing parameter: '%s' is not a column name and is not a single numeric value.",
        as.character(missing)
      ))
    }
  }

  if (!is.null(missing_pct)) {
    if (is.character(missing_pct) && length(missing_pct) == 1 && missing_pct %in% names(df)) {
      # Column name provided - look up per-group values
      group_missing_pct <- dplyr::select(df, !!rlang::sym(group_var), !!rlang::sym(missing_pct))
      group_missing_pct <- unique(group_missing_pct)

      for (i in 1:nrow(group_missing_pct)) {
        g <- as.character(group_missing_pct[[group_var]][i])
        if (is.null(missing_values[[g]])) {
          missing_values[[g]] <- list(pct = group_missing_pct[[missing_pct]][i])
        }
      }
    } else if (is.numeric(missing_pct) && length(missing_pct) == 1) {
      # Single numeric percentage for all groups
      for (g in groups) {
        if (is.null(missing_values[[as.character(g)]])) {
          missing_values[[as.character(g)]] <- list(pct = missing_pct)
        }
      }
    }
  }

  return(missing_values)
}

#' Process a single group for FHLUS calculation (internal helper)
#'
#' Returns a list with \code{scores} (tibble) and \code{plot} (ggplot or NULL).
#' @noRd
process_single_group_internal <- function(group_data, g, group_var,
                                          numerator, rank_var, denom,
                                          missing_value, missing_pct_value,
                                          exclude_area, tie_breaker,
                                          should_graph) {

  # Validation: Check minimum rows
  if (nrow(group_data) < 2) {
    return(list(
      scores = tibble::tibble(
        !!rlang::sym(group_var) := g,
        type = "base",
        score = NA_real_,
        percent_error = NA_real_,
        error_reason = "insufficient_data"
      ),
      plot = NULL
    ))
  }

  # Validation: Check zero numerator
  total_num <- sum(group_data[[numerator]], na.rm = TRUE)
  if (total_num == 0 || !is.finite(total_num)) {
    return(list(
      scores = tibble::tibble(
        !!rlang::sym(group_var) := g,
        type = "base",
        score = NA_real_,
        percent_error = NA_real_,
        error_reason = "zero_numerator"
      ),
      plot = NULL
    ))
  }

  # Calculate FHLUS
  tryCatch({
    fhlus_result <- fhlus_score(
      df = group_data,
      numerator = numerator,
      rank_var = rank_var,
      denom = denom,
      missing = missing_value,
      missing_pct = missing_pct_value,
      graph = should_graph,
      exclude_area = exclude_area,
      tie_breaker = tie_breaker,
      plot_subtitle = as.character(g),
      verbose = FALSE
    )

    # Return scores with group identifier, and the plot object
    scores <- fhlus_result$scores %>%
      dplyr::mutate(!!rlang::sym(group_var) := g, error_reason = NA_character_)

    list(scores = scores, plot = fhlus_result$plot)

  }, error = function(e) {
    list(
      scores = tibble::tibble(
        !!rlang::sym(group_var) := g,
        type = "base",
        score = NA_real_,
        percent_error = NA_real_,
        error_reason = paste0("error: ", e$message)
      ),
      plot = NULL
    )
  })
}

#' Process groups sequentially
#' @noRd
process_groups_sequential <- function(df, groups, group_var, numerator,
                                      rank_var, denom, missing_values,
                                      exclude_area, tie_breaker, graph,
                                      graph_subset, verbose) {

  score_results <- list()
  plot_results <- list()

  for (i in seq_along(groups)) {
    g <- groups[i]

    if (verbose) {
      message(paste("Processing group", i, "of", length(groups), ":", g))
    }

    # Subset data for group
    group_data <- df[df[[group_var]] == g, ]

    # Determine if we should graph this group
    should_graph <- graph && (is.null(graph_subset) || g %in% graph_subset)

    # Get missing value for this group
    group_missing <- NULL
    group_missing_pct <- NULL

    if (!is.null(missing_values[[as.character(g)]])) {
      mv <- missing_values[[as.character(g)]]
      if (is.list(mv) && "pct" %in% names(mv)) {
        group_missing_pct <- mv$pct
      } else {
        group_missing <- mv
      }
    }

    # Process this group using the internal helper
    group_result <- process_single_group_internal(
      group_data, g, group_var, numerator, rank_var, denom,
      group_missing, group_missing_pct, exclude_area, tie_breaker, should_graph
    )

    # Separate scores and plot
    score_results[[as.character(g)]] <- group_result$scores

    if (!is.null(group_result$plot)) {
      plot_results[[as.character(g)]] <- group_result$plot
    }

    # Report skipped groups
    if (verbose && "error_reason" %in% names(group_result$scores)) {
      err_reason <- group_result$scores$error_reason[1]
      if (!is.na(err_reason)) {
        message(paste("  Skipping group", g, "-", err_reason))
      }
    }
  }

  return(list(scores = score_results, plots = plot_results))
}

#' Process groups in parallel
#' @noRd
process_groups_parallel <- function(df, groups, group_var, numerator,
                                    rank_var, denom, missing_values,
                                    exclude_area, tie_breaker, graph,
                                    graph_subset, verbose) {

  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop("Package 'furrr' required for parallel processing. Install with install.packages('furrr').")
  }

  # Set up parallel processing
  future::plan(future::multisession)

  # Create function to process single group
  process_single_group <- function(g) {
    group_data <- df[df[[group_var]] == g, ]

    should_graph <- graph && (is.null(graph_subset) || g %in% graph_subset)

    group_missing <- NULL
    group_missing_pct <- NULL

    if (!is.null(missing_values[[as.character(g)]])) {
      mv <- missing_values[[as.character(g)]]
      if (is.list(mv) && "pct" %in% names(mv)) {
        group_missing_pct <- mv$pct
      } else {
        group_missing <- mv
      }
    }

    # Use the internal helper (returns list with scores + plot)
    process_single_group_internal(
      group_data, g, group_var, numerator, rank_var, denom,
      group_missing, group_missing_pct, exclude_area, tie_breaker, should_graph
    )
  }

  # Process all groups in parallel
  raw_results <- furrr::future_map(groups, process_single_group, .progress = verbose)

  # Reset to sequential processing
  future::plan(future::sequential)

  names(raw_results) <- as.character(groups)

  # Separate scores and plots
  score_results <- lapply(raw_results, `[[`, "scores")
  plot_results <- Filter(Negate(is.null), lapply(raw_results, `[[`, "plot"))

  return(list(scores = score_results, plots = plot_results))
}

#' Pivot long-format grouped scores to wide format (one row per group)
#'
#' Converts type column (base/lowest/highest) into separate columns:
#' score, score_lowest, score_highest.
#' @noRd
pivot_scores_wide <- function(results_long, group_var) {

  # Split into base rows and CI rows
  base_rows <- results_long[results_long$type == "base", ]
  ci_rows <- results_long[results_long$type != "base", ]

  # Drop type column from base rows
  base_rows$type <- NULL

  # If CI rows exist, pivot them wider and join

  if (nrow(ci_rows) > 0) {
    # Keep only the columns needed for pivoting
    ci_pivot <- ci_rows[, c(group_var, "group_id", "type", "score")]

    ci_wide <- tidyr::pivot_wider(
      ci_pivot,
      names_from = "type",
      values_from = "score",
      names_prefix = "score_"
    )

    # Join CI columns onto base rows
    base_rows <- dplyr::left_join(
      base_rows,
      ci_wide,
      by = c(group_var, "group_id")
    )
  } else {
    # No CI rows — add NA columns
    base_rows$score_lowest <- NA_real_
    base_rows$score_highest <- NA_real_
  }

  return(base_rows)
}

#' Add summary statistics to grouped results
#' @noRd
add_summary_statistics <- function(results) {

  # Calculate statistics for each score column (wide format)
  score_cols <- intersect(c("score", "score_lowest", "score_highest"), names(results))

  summary_rows <- lapply(score_cols, function(col) {
    vals <- results[[col]]
    non_na <- vals[!is.na(vals)]
    data.frame(
      score_type = col,
      n_groups = length(vals),
      mean_score = if (length(non_na) > 0) mean(non_na) else NA_real_,
      median_score = if (length(non_na) > 0) stats::median(non_na) else NA_real_,
      sd_score = if (length(non_na) > 1) stats::sd(non_na) else NA_real_,
      min_score = if (length(non_na) > 0) min(non_na) else NA_real_,
      max_score = if (length(non_na) > 0) max(non_na) else NA_real_,
      n_positive = sum(non_na > 0),
      n_negative = sum(non_na < 0),
      n_zero = sum(abs(non_na) < .Machine$double.eps),
      n_na = sum(is.na(vals)),
      stringsAsFactors = FALSE
    )
  })

  summary_stats <- do.call(rbind, summary_rows)

  attr(results, "summary") <- summary_stats

  return(results)
}

#' Comparative analysis of grouped FHLUS results
#'
#' @param grouped_results Output from grouped_fhlus function
#' @param group_characteristics Data frame with group characteristics for correlation analysis
#' @param join_by Column to join on
#'
#' @return List with combined data, correlations, and summary statistics
#'
#' @examples
#' \dontrun{
#' # Create city characteristics data
#' city_chars <- data.frame(
#'   city = c("City A", "City B", "City C"),
#'   population = c(100000, 250000, 75000),
#'   median_income_city = c(55000, 68000, 48000)
#' )
#'
#' # Analyze correlations
#' analysis <- analyze_grouped_fhlus(
#'   grouped_results,
#'   group_characteristics = city_chars,
#'   join_by = "city"
#' )
#'
#' print(analysis$correlations)
#' }
#'
#' @export
analyze_grouped_fhlus <- function(grouped_results,
                                  group_characteristics = NULL,
                                  join_by = NULL) {

  # Wide format: one row per group already; drop CI/meta columns for correlation
  base_scores <- grouped_results %>%
    dplyr::select(-dplyr::any_of(c("score_lowest", "score_highest",
                                    "percent_error", "group_id", "error_reason")))

  # If characteristics provided, join and analyze
  if (!is.null(group_characteristics) && !is.null(join_by)) {
    combined <- base_scores %>%
      dplyr::left_join(group_characteristics, by = join_by)

    # Calculate correlations with numeric variables
    numeric_vars <- names(combined)[sapply(combined, is.numeric)]
    numeric_vars <- setdiff(numeric_vars, c("score", join_by))

    if (length(numeric_vars) > 0) {
      correlations <- sapply(numeric_vars, function(var) {
        stats::cor(combined$score, combined[[var]], use = "complete.obs")
      })

      correlation_df <- data.frame(
        variable = names(correlations),
        correlation = correlations,
        stringsAsFactors = FALSE
      )
      correlation_df <- dplyr::arrange(correlation_df, dplyr::desc(abs(correlation)))

      # Add significance tests
      correlation_df$p_value <- sapply(numeric_vars, function(var) {
        if (sum(!is.na(combined$score) & !is.na(combined[[var]])) > 2) {
          stats::cor.test(combined$score, combined[[var]])$p.value
        } else {
          NA_real_
        }
      })

      correlation_df$significant <- correlation_df$p_value < 0.05

      return(list(
        data = combined,
        correlations = correlation_df,
        summary = attr(grouped_results, "summary")
      ))
    }
  }

  return(list(
    data = base_scores,
    summary = attr(grouped_results, "summary")
  ))
}

#' Print method for grouped FHLUS results
#' @export
print.grouped_fhlus <- function(x, n = 10, ...) {
  cat("Grouped Fair Housing Land Use Score (FHLUS) Results\n")
  cat("====================================================\n\n")

  # Identify group variable (first column that's not a score/meta column)
  meta_cols <- c("score", "score_lowest", "score_highest",
                 "percent_error", "group_id", "error_reason")
  group_col <- setdiff(names(x), meta_cols)[1]

  n_groups <- length(unique(x[[group_col]]))
  cat("Number of groups:", n_groups, "\n\n")

  # Print summary if available
  if (!is.null(attr(x, "summary"))) {
    cat("Summary Statistics:\n")
    print(attr(x, "summary"))
  }

  if ("score" %in% names(x)) {
    cat("\nTop", n, "groups (by score):\n")
    top_groups <- x %>%
      dplyr::arrange(dplyr::desc(score)) %>%
      utils::head(n)
    print(as.data.frame(top_groups))

    cat("\nBottom", n, "groups (by score):\n")
    bottom_groups <- x %>%
      dplyr::arrange(score) %>%
      utils::head(n)
    print(as.data.frame(bottom_groups))
  } else {
    print(as.data.frame(x))
  }

  invisible(x)
}

#' Plot grouped FHLUS results
#'
#' @param grouped_results Output from grouped_fhlus function
#' @param score_col Column to plot: "score" (default), "score_lowest", or "score_highest".
#'   For backward compatibility, "base"/"lowest"/"highest" are also accepted.
#' @param n_groups Number of groups to highlight at top and bottom
#' @param highlight_groups Vector of group names to highlight (overrides n_groups)
#'
#' @return ggplot object
#'
#' @examples
#' \dontrun{
#' # Plot grouped results
#' p <- plot_grouped_fhlus(
#'   grouped_results,
#'   score_col = "score",
#'   n_groups = 5
#' )
#' print(p)
#' }
#'
#' @export
plot_grouped_fhlus <- function(grouped_results,
                               score_col = "score",
                               n_groups = 10,
                               highlight_groups = NULL) {

  # Backward compatibility mapping
  compat_map <- c(
    "base" = "score",
    "lowest" = "score_lowest",
    "highest" = "score_highest"
  )
  if (score_col %in% names(compat_map)) {
    score_col <- compat_map[[score_col]]
  }

  if (!score_col %in% names(grouped_results)) {
    stop(sprintf(
      "Column '%s' not found in results. Available score columns: %s",
      score_col,
      paste(intersect(c("score", "score_lowest", "score_highest"), names(grouped_results)),
            collapse = ", ")
    ))
  }

  # Work with selected score column
  plot_data <- grouped_results
  plot_data$.plot_score <- plot_data[[score_col]]
  plot_data <- plot_data[!is.na(plot_data$.plot_score), ]
  plot_data <- plot_data[order(-plot_data$.plot_score), ]

  # Get group variable name
  meta_cols <- c("score", "score_lowest", "score_highest",
                 "percent_error", "group_id", "error_reason", ".plot_score")
  group_var <- setdiff(names(plot_data), meta_cols)[1]

  # Add ranking
  plot_data$rank <- seq_len(nrow(plot_data))

  # Identify groups to highlight
  if (!is.null(highlight_groups)) {
    plot_data$highlight <- plot_data[[group_var]] %in% highlight_groups
  } else {
    # Highlight top and bottom n_groups
    plot_data$highlight <- plot_data$rank <= n_groups |
      plot_data$rank > (nrow(plot_data) - n_groups)
  }

  # Pretty label for subtitle
  score_label <- switch(score_col,
    "score" = "Base score",
    "score_lowest" = "Lowest bound",
    "score_highest" = "Highest bound",
    score_col
  )

  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = stats::reorder(!!rlang::sym(group_var), .plot_score),
    y = .plot_score)) +
    ggplot2::geom_bar(stat = "identity",
                      ggplot2::aes(fill = .plot_score > 0)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "darkred"),
                               labels = c("TRUE" = "Positive", "FALSE" = "Negative"),
                               name = "Score") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = paste("FHLUS Scores by", group_var),
      subtitle = score_label,
      x = group_var,
      y = "FHLUS Score"
    ) +
    ggplot2::theme_minimal()

  # Add labels for highlighted groups
  if (any(plot_data$highlight)) {
    p <- p + ggplot2::geom_text(
      data = plot_data[plot_data$highlight, ],
      ggplot2::aes(label = round(.plot_score, 2)),
      hjust = ifelse(plot_data$.plot_score[plot_data$highlight] > 0, -0.1, 1.1),
      size = 3
    )
  }

  # Adjust axis labels if too many groups
  if (nrow(plot_data) > 30) {
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

  return(p)
}
