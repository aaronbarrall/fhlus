# fhlus 0.4.0

## New Features

### Policy Comparison Visualization

* New module `R/fhlus_compare.R` with `fhlus_compare()` for overlaying two FHLUS results on a single plot — useful for comparing policy alternatives, housing types, or time periods.
* Legend labels embed scores directly: `"Policy A (FHLUS = 0.45)"`.
* Difference annotation included in the reference line legend entry.
* Returns an `fhlus_compare` class object with `$plot`, `$scores`, `$difference`.
* S3 `print.fhlus_compare()` method displays score summary with optional CI ranges.

### CI Ribbon Bands

* `fhlus_score()` gains `show_ci_bands` (default `FALSE`) and `ci_alpha` (default `0.15`) parameters. When `show_ci_bands = TRUE` and `missing`/`missing_pct` is specified, a shaded ribbon shows the envelope between the lowest and highest cumulative distribution curves on the plot.
* `fhlus_compare()` supports `show_ci = TRUE` to draw CI ribbon bands for each policy on the comparison plot. Works when one or both results have CI data — warns and skips bands for any result that lacks it.
* Works identically with both `missing` (absolute units) and `missing_pct` (percentage) since both flow through the same CI calculation path.

### Stored CI Curve Data

* `fhlus_score()` result objects now include `$data_lowest` and `$data_highest` — the full cumulative distribution data frames from the CI bound calculations (or `NULL` when no CI was computed). Previously this data was computed internally but discarded.
* This enables downstream tools (like `fhlus_compare()`) to draw CI curves without recomputation.

---

# fhlus 0.3.0

## New Features

### Wide-Format Grouped Output

* `grouped_fhlus()` now returns **one row per group** with columns `score`, `score_lowest`, and `score_highest` instead of multiple rows per group with a `type` column.
* The `type` column is no longer present in grouped results.
* Summary statistics (via `attr(result, "summary")`) now report per score column (`score_type`) instead of per `type`.

### Per-Group Error Rates

* `missing` and `missing_pct` parameters in `grouped_fhlus()` now accept **column names** so each group can have a different error rate (e.g., `missing_pct = "pct_error"`).
* Fixed a bug in `prepare_missing_by_group()` where `missing = "column_name"` was incorrectly treated as a scalar value because `length("column_name") == 1`. Column names are now checked with `is.character()` before the length check.

### Grouped Plot Storage and Labeling

* `grouped_fhlus(graph=TRUE)` now **stores plot objects** in `attr(result, "plots")` as a named list keyed by group value. Previously plots were printed but discarded.
* Each group's FHLUS plot now includes the **group name as the subtitle** (e.g., "SAN FRANCISCO") so you can tell which plot belongs to which group.
* `fhlus_score()` gains a `plot_subtitle` parameter for custom plot subtitles.
* MOE/CI annotations on individual FHLUS plots moved to the bottom-right to avoid overlapping with the ranking direction labels.

### Updated Plot API

* `plot_grouped_fhlus()` parameter renamed from `type` to `score_col` (default `"score"`).
* Backward-compatible: old values `"base"`, `"lowest"`, `"highest"` are automatically mapped to `"score"`, `"score_lowest"`, `"score_highest"`.

### Updated Analysis Functions

* `analyze_grouped_fhlus()` no longer filters by `type == "base"` — results are already one row per group.
* `reshape_multiple_indices()` updated to handle wide-format grouped input.

---

# fhlus 0.2.0

## New Features

### Spatial Exclusion Layer Support

* `prepare_spatial_fhlus()` gains a new `exclude_layer` parameter that accepts an sf polygon layer (e.g., parks or open-space shapefile) for true geometric exclusion (#1).
* The exclusion layer is dissolved via `st_union()`, intersected with source features via `st_intersection()`, and actual excluded area per feature is calculated from the intersection geometry.
* CRS mismatches between `spatial_data` and `exclude_layer` are auto-detected and transformed with an informative message.
* Attribute-based exclusion (`exclude_zones`/`zone_column`) and spatial layer exclusion (`exclude_layer`) can be used separately or combined — when combined, exclusions are additive and capped at `total_area`.
* New internal helper `calculate_excluded_areas_spatial()` handles the dissolve-intersect-aggregate pipeline with `tryCatch` error recovery for topology issues.
* 7 new tests covering basic spatial exclusion, non-overlapping layers, CRS auto-transformation, input validation, combined exclusion, area capping, and backward compatibility.

---

# fhlus 0.1.0

## Initial CRAN Release

This is the first release of the fhlus package to CRAN.

### Features

#### Core Functionality
* `fhlus_score()`: Calculate Fair Housing Land Use Score for single datasets
* `grouped_fhlus()`: Calculate FHLUS for multiple groups with sequential or parallel processing
* Support for confidence intervals via `missing` and `missing_pct` parameters
* Automatic visualization with cumulative distribution curves
* S3 print methods for clean output display

#### Data Preparation and Validation
* `validate_fhlus_data()`: Comprehensive data validation with detailed error reporting
* `prepare_spatial_fhlus()`: Spatial data preparation with area calculations (requires sf)
* `aggregate_to_neighborhoods()`: Spatial aggregation of point data
* `spatial_impute()`: Spatial imputation using k-nearest neighbors or IDW methods

#### Analysis and Visualization
* `analyze_grouped_fhlus()`: Comparative analysis with correlation calculations
* `plot_grouped_fhlus()`: Publication-ready visualizations of grouped results
* `calculate_multiple_indices()`: Simultaneous analysis of multiple opportunity metrics
* `export_fhlus()`: Export to CSV, Excel, JSON, or RDS formats

### Robustness Improvements

#### Edge Case Handling
* Safe handling of empty data frames (0 rows)
* Graceful handling of insufficient data (< 2 rows)
* Protection against zero or all-NA values in critical columns
* Proper handling of tied ranking values with optional tie-breakers
* Support for extreme distributions and edge cases

#### Numerical Stability
* Division by zero protection in all calculations
* Floating-point safe comparisons using `.Machine$double.eps`
* Robust AUC calculation with error handling
* Input validation for data types and value ranges
* Non-finite value detection and handling

#### Input Validation
* Comprehensive column existence checking
* Numeric type validation with informative error messages
* Negative value detection in numerator and denominator
* All-NA column detection
* Minimum data requirement enforcement (≥ 2 rows)

### Testing

* **96 comprehensive tests** covering:
  - 30+ input validation tests
  - 26+ edge case tests
  - 20+ functionality tests
  - Grouped analysis tests
  - Utility function tests
* Test coverage exceeds 80% for core functions

### Documentation

* Complete roxygen2 documentation for all exported functions
* Working examples for all functions (wrapped in `\donttest{}` for spatial functions)
* Package-level documentation with interpretation guide
* Detailed parameter descriptions and return value specifications
* Comprehensive README with quick start guide

### Error Messages

Enhanced error messages following best practices:
* Clear indication of what went wrong
* Suggestion of corrective actions
* Reference to validation functions where appropriate
* Contextual information (e.g., available columns, actual values)

### Dependencies

**Required:**
* dplyr (>= 1.0.0)
* ggplot2 (>= 3.4.0)
* tibble (>= 3.1.0)
* tidyr (>= 1.2.0)
* rlang (>= 1.0.0)
* DescTools (>= 0.99.44)
* scales

**Optional (Suggests):**
* sf (spatial operations)
* spdep (spatial imputation)
* furrr (parallel processing)
* future (parallel backend)
* openxlsx (Excel export)
* jsonlite (JSON export)
* testthat (≥ 3.0.0, testing)
* knitr (vignettes)
* rmarkdown (vignettes)
* covr (code coverage)

### Breaking Changes

This is an initial release, so there are no breaking changes.

### Known Issues

None at this time. Please report any issues at <https://github.com/yourusername/fhlus/issues>

### Future Plans

Potential features for future releases:
* Additional visualization options
* More imputation methods for spatial data
* Support for weighted FHLUS calculations
* Additional opportunity index presets
* Interactive visualizations (shiny integration)

---

For a complete list of changes and updates, see the commit history at <https://github.com/yourusername/fhlus>
