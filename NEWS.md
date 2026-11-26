# fhlus (development version)

## New Features

### Linked Plot and Map

* New `fhlus_linked()` shows the interactive FHLUS curve next to a map of the same geographies (MapLibre GL), linked both ways: hovering a segment outlines its neighborhood, hovering a neighborhood highlights its segment, and clicking either pins it. The map can be colored by rank or by the numerator, or shown as outlines, over a light, street or aerial basemap (keyless Esri tiles); all of these switch in the page and work in files saved with `fhlus_save_html()`. It takes the geometry from results computed on sf data (e.g. after `prepare_spatial_fhlus()`), or from a separate `geometry` layer matched by ID. Supports up to `max_features = 5000` geographies, simplifying large inputs automatically.
* `fhlus_linked(numerator_layer = )` draws the site-level numerator features on the map: points as circles sized by `numerator_var`, or polygons (e.g. parcels) as shapes. Each feature is linked to the geography containing it, so hovering a feature highlights its geography and curve segment, and hovering or pinning a geography outlines its features. Geography tooltips gain the feature count and total, `numerator_tooltip_vars` adds columns to the feature tooltips, and the layer can be shown or hidden in the page.
* New Shiny bindings `fhlus_linkedOutput()` and `renderFhlus_linked()`; the hovered and pinned IDs arrive as `input$<id>_hover` and `input$<id>_selected`.
* MapLibre GL JS 5.24.1 (BSD-3-Clause) is bundled in `inst/htmlwidgets/lib/`.

### Boundary Clipping and Numerator Joins

* `prepare_spatial_fhlus()` can now clip neighborhoods to a jurisdiction (`boundary`) and sum a spatial numerator layer into them (`numerator`, `numerator_vars`), in a fixed order: reproject, clip, calculate areas and exclusions, assign the numerator. It prints numbered progress steps (`verbose = TRUE`), and warns up front when a job looks slow (lat/lon data, very detailed boundary or exclusion layers, very large numerator layers).
* With `boundary_id`, `prepare_spatial_fhlus()` clips to several jurisdictions at once and stacks the result, ready for `grouped_fhlus(group_var = boundary_id)`. A tract crossing two jurisdictions appears once in each, with its area split. It also works inside `lapply()` over jurisdictions.
* New `clip_to_boundary()` clips neighborhoods to a boundary and adds `original_area` and `share_in_boundary`. Slivers under `min_share` (default 1%) of a tract's area are dropped.
* New `assign_to_neighborhoods()` sums point or polygon numerators into neighborhoods. Every neighborhood is kept, with 0 where nothing falls. Polygons are assigned by `"point_on_surface"` (default) or split by `"area_weighted"` overlap. It reports how much was assigned and handles features outside every neighborhood with `outside = "warn"` / `"error"` / `"ignore"`.

### Missing Rank Values

* New `rank_na` argument to `fhlus_score()` and `grouped_fhlus()` (with `impute_method` and `impute_k`). Previously every row with a missing rank value was dropped, which silently removed units when the row had any. Now, by default (`"auto"`), rows with no numerator are dropped with a message, and rows with numerator values are imputed from their spatial neighbors with `spatial_impute()` before ranking. If they can't be imputed (plain data frame, spdep not installed, or no neighbor with a value), `fhlus_score()` stops with an error naming them and the units at stake. `"impute"` imputes every missing row; `"drop"` drops them all, warning about any numerator lost. **This changes the default:** plain data frames with missing rank values in rows with units now error instead of dropping those rows.
* `result$meta$rank_na` records the `dropped` and `imputed` rows, and `result$data$.rank_imputed` flags imputed rows. Popups in `fhlus_interactive()` and `fhlus_linked()` mark imputed values ("imputed from 5 nearest neighbors (mean)"). `grouped_fhlus()` adds `n_rank_imputed` and `n_rank_dropped` columns.
* `fhlus_linked(geometry = )` draws neighborhoods dropped for a missing rank value in grey as "not scored", instead of leaving a gap.

## Bug Fixes

* With `exclude_area`, `fhlus_score()` no longer overwrites the denominator column with the available area. `result$data` keeps the original `denom` values and adds `<denom>_adjusted`, which the score uses (`result$meta$denom_used`). Scores and curves are unchanged. Popups now show the total, the excluded area (with its share of the total) and the available area used in the score; before, the available area was labeled with the denominator's name next to an excluded area that could be larger than it.
* `fhlus_to_list()` output is now `format_version` 2: `meta` gains `denom_used` and `rank_na`, and segments carry `<denom>_adjusted` and `.rank_imputed`.
* Popup values of 1,000 or more are shown in full (140,954,937 rather than 141,000,000).
* `fhlus_score()` no longer fails on sf input when `verbose = TRUE` (the default). The NA/Inf checks picked up the sticky geometry column.
* `aggregate_to_neighborhoods()` now wraps `assign_to_neighborhoods()`. Previously a point on a shared tract edge was counted in both tracts, polygon features were counted in full in every tract they touched, and features outside every tract were dropped silently (they now trigger a warning).

## Documentation

* The README has a complete function reference and a longer spatial section.

---

# fhlus 0.5.0

## New Features

### Interactive Plots

* New `fhlus_interactive()` builds an interactive (plotly) version of the FHLUS plot. Hovering over the curve shows the geography under the cursor: its ID (e.g. GEOID), rank, raw rank-variable/numerator/denominator values, its shares and concentration, cumulative percentages, CI range, and any extra `tooltip_vars` — including variables not used in the ranking. The hovered segment is highlighted and its slice of the x-axis shaded.
* New `fhlus_compare_interactive()` is the interactive counterpart to `fhlus_compare()`. Hovering shows one combined pop-up per geography — shared details once, a block per policy, and their difference (share of total, concentration, cumulative gap) — and highlights the geography on both curves.
* New `fhlus_save_html()` saves an interactive plot (or an `fhlus` result directly) as a standalone HTML file that opens in any browser without R.
* New `fhlus_segments()` returns one row per geography with its segment coordinates, shares, concentration (`share_var / share_denom`), and raw values. It underlies the interactive plots and is keyed by geography ID for joining to spatial data.
* Inputs above 2,000 geographies automatically use WebGL for the hover layer.
* `plotly` and `htmlwidgets` are new *suggested* dependencies; the core package does not require them.
* New vignette, `vignette("interactive-plots")`, walks through the interactive plots, pop-ups, HTML export, the segment table, and Shiny use.
* The README has a new "Interactive Plots" section with illustrated examples (`man/figures/`).

### Result Metadata

* `fhlus_score()` results gain a `$meta` element recording the column names and settings used (`numerator`, `denom`, `rank_var`, `tie_breaker`, `exclude_area`, `missing`). Existing elements are unchanged.

### JSON Output

* New `fhlus_to_list()` flattens a result into plain lists and data frames (scores, CI, meta, and the `fhlus_segments()` table with HTML tooltips) for sending to a web front end with `jsonlite::toJSON()`.

## Bug Fixes

* `tie_breaker` was only applied when `verbose = TRUE`. It is now always applied, and also with multiple `rank_var`s. Scores computed with `tie_breaker` and `verbose = FALSE` (including through `grouped_fhlus()`) may change.
* `fhlus_score()` now errors with a clear message when a `tie_breaker` column doesn't exist.
* Rows with NA values in the numerator, denominator or rank variable(s), or infinite numerator/denominator values, are now dropped before scoring, as the warning always said. Previously they made the AUC calculation fail.
* When every row has the same rank value, the score is now 0 (with a warning) instead of depending on the arbitrary row order.
* `%>%` is now imported, so `grouped_fhlus()` printing, `analyze_grouped_fhlus()`, `calculate_multiple_indices()` and the spatial helpers work when the package is installed rather than sourced.

---

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

## Initial Release

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
