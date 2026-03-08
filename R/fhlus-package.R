#' @keywords internal
"_PACKAGE"

#' fhlus: Fair Housing Land Use Score Calculation
#'
#' Calculate the Fair Housing Land Use Score (FHLUS), a statistical measure
#' analyzing spatial distribution of housing units (or other variables) across
#' geographic areas ranked by opportunity indicators (e.g., income, poverty rate).
#'
#' @section Main Functions:
#' Core FHLUS calculation:
#' \itemize{
#'   \item \code{\link{fhlus_score}}: Calculate FHLUS for a single dataset
#'   \item \code{\link{grouped_fhlus}}: Calculate FHLUS for multiple groups (e.g., by city)
#' }
#'
#' Data preparation and validation:
#' \itemize{
#'   \item \code{\link{validate_fhlus_data}}: Validate data before calculation
#'   \item \code{\link{prepare_spatial_fhlus}}: Prepare spatial data (requires sf package)
#'   \item \code{\link{aggregate_to_neighborhoods}}: Aggregate units to neighborhoods
#'   \item \code{\link{spatial_impute}}: Impute missing values using spatial neighbors
#' }
#'
#' Analysis and output:
#' \itemize{
#'   \item \code{\link{analyze_grouped_fhlus}}: Comparative analysis of grouped results
#'   \item \code{\link{plot_grouped_fhlus}}: Visualize grouped FHLUS results
#'   \item \code{\link{calculate_multiple_indices}}: Calculate FHLUS for multiple opportunity metrics
#'   \item \code{\link{export_fhlus}}: Export results to CSV, XLSX, JSON, or RDS
#' }
#'
#' @section FHLUS Score Interpretation:
#' The FHLUS score ranges from -1 to +1:
#' \itemize{
#'   \item \strong{+1.0}: All units concentrated in highest-opportunity areas (ideal distribution)
#'   \item \strong{0.0}: Proportional distribution matching area distribution (neutral)
#'   \item \strong{-1.0}: All units concentrated in lowest-opportunity areas (concerning distribution)
#' }
#'
#' Positive scores indicate housing is concentrated in higher-opportunity areas.
#' Negative scores indicate housing is concentrated in lower-opportunity areas.
#'
#' @section Getting Started:
#' Basic usage:
#' \preformatted{
#' # Prepare your data with: numerator (e.g., housing_units),
#' # rank_var (e.g., median_income), and denom (e.g., land_area)
#'
#' result <- fhlus_score(
#'   df = my_data,
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area"
#' )
#'
#' print(result)
#' }
#'
#' For grouped analysis by jurisdiction:
#' \preformatted{
#' grouped <- grouped_fhlus(
#'   df = regional_data,
#'   group_var = "city",
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area"
#' )
#' }
#'
#' @section Confidence Intervals:
#' FHLUS supports confidence intervals when there is uncertainty in the data:
#' \preformatted{
#' # With 10% margin of error
#' result <- fhlus_score(
#'   df = my_data,
#'   numerator = "housing_units",
#'   rank_var = "median_income",
#'   denom = "land_area",
#'   missing_pct = 0.10
#' )
#'
#' result$confidence_interval
#' }
#'
#' @section References:
#' For more information about fair housing metrics and land use analysis, see the
#' package vignettes: \code{vignette("fhlus-intro")} and \code{vignette("fhlus-advanced")}
#'
#' @docType package
#' @name fhlus-package
NULL
