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
#' @param exclude_area Column name for areas to exclude (e.g., open space) or
#'   NULL. The score then uses \code{denom - exclude_area} (floored at 0), stored
#'   in \code{result$data} as \code{<denom>_adjusted}; the \code{denom} column
#'   keeps its original values.
#' @param rank_na How to handle rows where a rank variable is \code{NA}:
#'   \describe{
#'     \item{\code{"auto"}}{(default) rows with no numerator (0) are dropped
#'       with a message; rows with numerator values are imputed from their
#'       spatial neighbors (see \code{\link{spatial_impute}()}), since dropping
#'       them would remove units and bias the score. If they can't be imputed
#'       (no geometry, \pkg{spdep} not installed, or no neighbor with a value),
#'       \code{fhlus_score()} stops with an error naming them.}
#'     \item{\code{"impute"}}{impute every row with a missing rank value. Rows
#'       with numerator values that can't be imputed are an error; rows without
#'       are dropped.}
#'     \item{\code{"drop"}}{drop every such row. Dropping rows with numerator
#'       values gives a warning listing them and the numerator lost.}
#'   }
#'   Imputed rows are ranked with the rest and flagged in
#'   \code{result$data$.rank_imputed}; \code{result$meta$rank_na} records the
#'   dropped and imputed rows.
#' @param impute_method,impute_k Imputation method (\code{"mean"},
#'   \code{"median"} or \code{"idw"}) and number of nearest neighbors, passed
#'   to \code{spatial_impute()}.
#' @param tie_breaker Vector of column names to use as tie breakers in ranking
#' @param boundary_label Label for boundary points on plot, or NULL to hide them
#' @param plot_subtitle Custom subtitle for the plot, or NULL for default. Used by
#'   \code{grouped_fhlus()} to label each group's plot with the group name.
#' @param show_ci_bands Logical, whether to draw CI ribbon bands on the plot
#'   when confidence interval data is available (default: FALSE). Requires
#'   \code{missing} or \code{missing_pct} to be specified.
#' @param ci_alpha Numeric, transparency for CI ribbon bands (default: 0.15).
#'   Only used when \code{show_ci_bands = TRUE}.
#' @param annotation Controls the score annotation on the plot. \code{TRUE}
#'   (default) shows "FHLUS = X"; \code{NULL} or \code{FALSE} suppresses it;
#'   a character string displays that text instead.
#' @param annotation_x Numeric, x-position of the annotation (0-1, default: 0.125).
#' @param annotation_y Numeric, y-position of the annotation (0-1, default: 0.93).
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
#'   \item{meta}{List of the column names and settings used (numerator, denom,
#'     denom_used, rank_var, tie_breaker, exclude_area, missing, rank_na). Used
#'     by \code{fhlus_segments()} and \code{fhlus_interactive()}.
#'     \code{denom_used} is the column the score was computed from
#'     (\code{<denom>_adjusted} with \code{exclude_area}, otherwise
#'     \code{denom}). \code{rank_na} is a list of two data frames:
#'     \code{dropped} (id, rank_var, numerator_value, denom_value) and
#'     \code{imputed} (id, rank_var, imputed_value, method, k).}
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
                        rank_na = c("auto", "drop", "impute"),
                        impute_method = c("mean", "median", "idw"),
                        impute_k = 5,
                        boundary_label = "Tract Boundary",
                        plot_subtitle = NULL,
                        show_ci_bands = FALSE,
                        ci_alpha = 0.15,
                        annotation = TRUE,
                        annotation_x = 0.125,
                        annotation_y = 0.93,
                        verbose = TRUE) {

  rank_na <- match.arg(rank_na)
  impute_method <- match.arg(impute_method)

  # Input validation
  validate_inputs(df, numerator, rank_var, denom, verbose, tie_breaker)

  # Drop rows with NA or non-finite numerator/denominator values
  df <- drop_incomplete_rows(df, numerator, denom)

  # Rows with a missing rank value: drop, impute or stop (before ranking, so
  # imputed rows are ranked with the rest)
  rank_na_out <- handle_missing_rank(df, numerator, rank_var, denom, rank_na,
                                     impute_method, impute_k, verbose)
  df <- rank_na_out$df
  if (nrow(df) < 2) {
    stop("Fewer than 2 rows remain after removing rows with missing values. Cannot compute FHLUS.")
  }

  # Adjust denominator for excluded areas if specified. The original column
  # is kept; the score uses <denom>_adjusted.
  denom_used <- denom
  if (!is.null(exclude_area) && exclude_area %in% names(df)) {
    df <- adjust_for_excluded_area(df, denom, exclude_area, verbose)
    denom_used <- paste0(denom, "_adjusted")
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
  base_result <- compute_score(df, numerator, "rank", denom_used)

  # Calculate confidence intervals if missing data specified
  ci_results <- NULL
  if (!is.null(missing) && missing > 0) {
    ci_results <- calculate_confidence_intervals(df, numerator, denom_used, missing, base_result)
  }

  # With no variation in the rank variable(s), the row order is arbitrary and
  # no area is higher-opportunity than another, so the score is 0 by definition
  if (nrow(unique(df[rank_var])) == 1 && !is.na(base_result$score)) {
    if (verbose) {
      warning("All rows share the same rank value. FHLUS is 0 by definition.")
    }
    base_result$score <- 0
    if (!is.null(ci_results)) {
      ci_results$lowest$score <- 0
      ci_results$highest$score <- 0
      ci_results$confidence_interval <- c(0, 0)
    }
  }

  # Create visualization
  plot_obj <- NULL
  if (graph) {
    plot_obj <- create_fhlus_plot(base_result, ci_results, missing,
                                    numerator, denom, rank_var, boundary_label,
                                    plot_subtitle, show_ci_bands, ci_alpha,
                                    annotation, annotation_x, annotation_y)
    print(plot_obj)
  }

  # Compile results
  results <- compile_results(base_result, ci_results, missing, plot_obj)
  results$meta <- list(
    numerator = numerator,
    denom = denom,
    denom_used = denom_used,
    rank_var = rank_var,
    tie_breaker = tie_breaker,
    exclude_area = if (!is.null(exclude_area) && exclude_area %in% names(df)) exclude_area else NULL,
    missing = missing,
    rank_na = rank_na_out$log
  )

  return(results)
}

#' Validate inputs for FHLUS calculation
#' @noRd
validate_inputs <- function(df, numerator, rank_var, denom, verbose,
                            tie_breaker = NULL) {

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

  # Check tie breaker columns
  missing_tb <- setdiff(tie_breaker, names(df))

  if (length(missing_tb) > 0) {
    stop(sprintf(
      "Tie breaker columns not found: %s. Available columns: %s",
      paste(missing_tb, collapse = ", "),
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

  # Check for NA and infinite values (warnings only)
  if (verbose) {
    # Missing rank values are reported by handle_missing_rank()
    na_counts <- vapply(unique(c(numerator, denom)), function(col) sum(is.na(df[[col]])), numeric(1))
    if (any(na_counts > 0)) {
      warning(sprintf(
        "NA values found in: %s. These rows will be excluded from calculations.",
        paste(names(na_counts)[na_counts > 0], collapse = ", ")
      ))
    }
    inf_counts <- vapply(unique(c(numerator, denom)), function(col) sum(is.infinite(df[[col]])), numeric(1))
    if (any(inf_counts > 0)) {
      warning(sprintf(
        "Infinite values found in: %s. These rows will be excluded from calculations.",
        paste(names(inf_counts)[inf_counts > 0], collapse = ", ")
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

#' Drop rows with NA or non-finite numerator or denominator values
#' @noRd
drop_incomplete_rows <- function(df, numerator, denom) {
  keep <- is.finite(df[[numerator]]) & is.finite(df[[denom]])
  df <- df[keep, , drop = FALSE]

  if (nrow(df) < 2) {
    stop("Fewer than 2 rows remain after removing NA and infinite values. Cannot compute FHLUS.")
  }

  return(df)
}

#' Drop, impute or refuse rows with missing rank values
#'
#' Returns list(df, log): `df` without the dropped rows, with imputed values
#' filled in and a `.rank_imputed` flag; `log` is list(dropped, imputed).
#' See the `rank_na` argument of fhlus_score() for the rules.
#' @noRd
handle_missing_rank <- function(df, numerator, rank_var, denom, rank_na,
                                method, k, verbose) {
  id_col <- resolve_id_col(df, NULL)
  ids <- if (is.null(id_col)) as.character(seq_len(nrow(df))) else as.character(df[[id_col]])
  id_text <- function(idx) {
    shown <- utils::head(ids[idx], 5)
    more <- if (length(idx) > 5) sprintf(", and %d more", length(idx) - 5) else ""
    paste0(if (is.null(id_col)) "rows" else paste0(id_col, "s"), ": ",
           paste(shown, collapse = ", "), more)
  }
  num_text <- function(idx) {
    paste(fmt_value(sum(df[[numerator]][idx])), numerator)
  }

  df$.rank_imputed <- FALSE
  log <- list(
    dropped = data.frame(id = character(0), rank_var = character(0),
                         numerator_value = numeric(0), denom_value = numeric(0),
                         stringsAsFactors = FALSE),
    imputed = data.frame(id = character(0), rank_var = character(0),
                         imputed_value = numeric(0), method = character(0),
                         k = integer(0), stringsAsFactors = FALSE)
  )
  is_na <- function(d) Reduce(`|`, lapply(rank_var, function(v) is.na(d[[v]])))
  na_rows <- is_na(df)
  if (!any(na_rows)) return(list(df = df, log = log))

  has_num <- df[[numerator]] > 0
  want <- switch(rank_na,
                 auto = na_rows & has_num,
                 impute = na_rows,
                 drop = rep(FALSE, nrow(df)))

  # Impute, one rank variable at a time, using only rows that have a value or
  # are being imputed as the neighbor pool
  reason <- NULL
  if (any(want)) {
    reason <- impute_unavailable_reason(df)
    if (is.null(reason)) {
      for (v in rank_var) {
        rows <- which(want & is.na(df[[v]]))
        if (length(rows) == 0) next
        pool <- which(!is.na(df[[v]]) | want)
        filled <- tryCatch(
          suppressWarnings(suppressMessages(
            spatial_impute(df[pool, ], v, method = method, k = min(k, length(pool) - 1))
          ))[[v]],
          error = function(e) {
            reason <<- paste0("spatial imputation failed (", conditionMessage(e), ")")
            NULL
          })
        if (is.null(filled)) break
        new <- filled[match(rows, pool)]
        ok <- !is.na(new)
        df[[v]][rows[ok]] <- new[ok]
        df$.rank_imputed[rows[ok]] <- TRUE
        log$imputed <- rbind(log$imputed, data.frame(
          id = ids[rows[ok]], rank_var = rep(v, sum(ok)), imputed_value = new[ok],
          method = rep(method, sum(ok)), k = rep(as.integer(k), sum(ok)),
          stringsAsFactors = FALSE))
      }
    }
  }

  still_na <- is_na(df)
  failed <- which(want & still_na & has_num)
  if (length(failed) > 0) {
    if (is.null(reason)) reason <- "they have no neighbors with a value"
    stop(sprintf(paste0(
      "rank_var %s is missing for %d neighborhood(s) that contain numerator values ",
      "(%s; %s). These cannot be dropped without biasing the FHLUS, and cannot be ",
      "imputed because %s. Fill these values before scoring, pass spatial (sf) data ",
      "so they can be imputed, or set rank_na = \"drop\" to drop them anyway."),
      paste0("'", rank_var, "'", collapse = "/"), length(failed), id_text(failed),
      num_text(failed), reason), call. = FALSE)
  }

  drop <- which(still_na)
  if (length(drop) > 0) {
    missing_vars <- vapply(drop, function(i) {
      paste(rank_var[vapply(rank_var, function(v) is.na(df[[v]][i]), logical(1))],
            collapse = ", ")
    }, character(1))
    log$dropped <- data.frame(id = ids[drop], rank_var = missing_vars,
                              numerator_value = df[[numerator]][drop],
                              denom_value = df[[denom]][drop],
                              stringsAsFactors = FALSE)
    lost <- drop[df[[numerator]][drop] > 0]
    if (length(lost) > 0) {
      # Always warn: this changes the score
      warning(sprintf(paste0(
        "Dropped %d neighborhood(s) with missing %s that contain numerator values ",
        "(%s; %s lost). This biases the FHLUS; use rank_na = \"auto\" to impute them instead."),
        length(lost), paste(rank_var, collapse = "/"), id_text(lost), num_text(lost)),
        call. = FALSE)
    }
    quiet <- setdiff(drop, lost)
    if (length(quiet) > 0 && verbose) {
      message(sprintf(
        "Dropped %d neighborhood(s) with missing %s and no %s (%s).",
        length(quiet), paste(rank_var, collapse = "/"), numerator, id_text(quiet)))
    }
  }
  if (verbose && nrow(log$imputed) > 0) {
    imp <- which(df$.rank_imputed)
    message(sprintf(
      "Imputed missing %s for %d neighborhood(s) from their %d nearest neighbors (%s; %s).",
      paste(unique(log$imputed$rank_var), collapse = "/"), length(imp), as.integer(k),
      method, id_text(imp)))
  }

  # Only keep imputation records for rows that are scored
  if (length(drop) > 0) {
    log$imputed <- log$imputed[!log$imputed$id %in% ids[drop], , drop = FALSE]
    df <- df[-drop, , drop = FALSE]
  }
  rownames(log$imputed) <- NULL
  list(df = df, log = log)
}

#' Why rank values can't be imputed, or NULL if they can
#' @noRd
impute_unavailable_reason <- function(df) {
  if (!inherits(df, "sf")) return("the data has no geometry (it is not an sf object)")
  if (!requireNamespace("sf", quietly = TRUE)) return("the sf package is not installed")
  if (!requireNamespace("spdep", quietly = TRUE)) return("the spdep package is not installed")
  NULL
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
    # Create composite ranking (tie breakers apply after all rank variables)
    df <- dplyr::arrange(df, !!!rlang::syms(c(rank_var, tie_breaker)))
  } else {
    # Single ranking variable
    df <- dplyr::arrange(df, !!rlang::sym(rank_var))

    # Check for ties
    if (any(duplicated(df[[rank_var]]))) {
      if (verbose) {
        n_ties <- sum(duplicated(df[[rank_var]]))
        warning(paste(n_ties, "tied values found in", rank_var))
      }

      if (!is.null(tie_breaker)) {
        if (verbose) {
          message(paste("Using tie breakers:", paste(tie_breaker, collapse = ", ")))
        }
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
                               show_ci_bands = FALSE, ci_alpha = 0.15,
                               annotation = TRUE, annotation_x = 0.125,
                               annotation_y = 0.93) {

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
    {
      ann_label <- if (isTRUE(annotation)) {
        paste("FHLUS =", round(base_result$score, 2))
      } else if (is.character(annotation)) {
        annotation
      } else {
        NULL
      }
      if (!is.null(ann_label)) {
        ggplot2::annotate("text", x = annotation_x, y = annotation_y, label = ann_label)
      } else {
        NULL
      }
    } +
    # X-axis ranking annotations
    ggplot2::annotate("text", x = 0.05, y = -0.12,
                      label = paste("Lower", rank_display),
                      hjust = 0, vjust = 1, size = 3.5, fontface = "italic") +
    ggplot2::annotate("text", x = 0.95, y = -0.12,
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
