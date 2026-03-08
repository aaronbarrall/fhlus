# fhlus: Fair Housing Land Use Score

<!-- badges: start -->
[![R-CMD-check](https://github.com/yourusername/fhlus/workflows/R-CMD-check/badge.svg)](https://github.com/yourusername/fhlus/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/fhlus)](https://CRAN.R-project.org/package=fhlus)
<!-- badges: end -->

## Overview

The **fhlus** package calculates the Fair Housing Land Use Score (FHLUS), a statistical measure that analyzes the spatial distribution of housing units (or other variables) across geographic areas ranked by opportunity indicators.

**FHLUS scores range from -1 to +1:**

- **+1.0**: All units concentrated in highest-opportunity areas (ideal)
- **0.0**: Proportional distribution matching area distribution (neutral)
- **-1.0**: All units concentrated in lowest-opportunity areas (concerning)

This tool helps urban planners, housing researchers, and policymakers assess whether affordable housing or other resources are equitably distributed across neighborhoods with varying levels of opportunity.

## Installation

Install from CRAN:

```r
install.packages("fhlus")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("yourusername/fhlus")
```

## Quick Start

```r
library(fhlus)

# Create sample data
sample_data <- data.frame(
  tract_id = 1:10,
  housing_units = c(100, 150, 200, 120, 180, 90, 160, 140, 110, 130),
  median_income = c(45000, 62000, 38000, 71000, 55000,
                    82000, 48000, 66000, 59000, 74000),
  land_area = c(2.5, 3.1, 1.8, 2.9, 3.5, 2.2, 2.7, 3.3, 2.1, 2.8)
)

# Calculate FHLUS
result <- fhlus_score(
  df = sample_data,
  numerator = "housing_units",  # Variable of interest
  rank_var = "median_income",    # Opportunity indicator (higher = better)
  denom = "land_area"            # Denominator (typically area)
)

# View results
print(result)
result$scores
```

## Key Features

### Core Functionality

- **`fhlus_score()`**: Calculate FHLUS for a single dataset
- **`grouped_fhlus()`**: Calculate FHLUS for multiple groups (e.g., by city or county)
- **`fhlus_compare()`**: Compare two FHLUS results on a single plot (e.g., policy alternatives)
- **Confidence intervals**: Handle uncertainty with `missing` or `missing_pct` parameters
- **CI ribbon bands**: Visualize confidence interval envelopes with `show_ci_bands = TRUE`
- **Visualization**: Automatic generation of cumulative distribution curves

### Data Preparation

- **`validate_fhlus_data()`**: Check data quality before calculation
- **`prepare_spatial_fhlus()`**: Prepare spatial data with attribute-based or spatial layer exclusions (requires `sf` package)
- **`aggregate_to_neighborhoods()`**: Aggregate point data to geographic units
- **`spatial_impute()`**: Impute missing values using spatial neighbors

### Analysis & Comparison Tools

- **`fhlus_compare()`**: Side-by-side policy comparison with overlaid curves and CI bands
- **`analyze_grouped_fhlus()`**: Comparative analysis with correlations
- **`plot_grouped_fhlus()`**: Visualize grouped results
- **`calculate_multiple_indices()`**: Analyze multiple opportunity metrics
- **`export_fhlus()`**: Export to CSV, Excel, JSON, or RDS

## Example: Grouped Analysis

```r
# Calculate FHLUS for multiple cities
regional_data <- data.frame(
  city = rep(c("City A", "City B", "City C"), each = 10),
  tract_id = 1:30,
  housing_units = sample(50:200, 30, replace = TRUE),
  median_income = sample(30000:90000, 30, replace = TRUE),
  land_area = runif(30, 1, 5)
)

grouped_results <- grouped_fhlus(
  df = regional_data,
  group_var = "city",
  numerator = "housing_units",
  rank_var = "median_income",
  denom = "land_area"
)

# View results — one row per group with score, score_lowest, score_highest
print(grouped_results)

# Per-group error rates (each city can have a different margin of error)
# regional_data$pct_error <- c(rep(0.05, 10), rep(0.10, 10), rep(0.15, 10))
# grouped_results <- grouped_fhlus(..., missing_pct = "pct_error")

# Visualize
plot_grouped_fhlus(grouped_results, score_col = "score", n_groups = 3)
```

## Example: Confidence Intervals

```r
# With 10% margin of error
result <- fhlus_score(
  df = sample_data,
  numerator = "housing_units",
  rank_var = "median_income",
  denom = "land_area",
  missing_pct = 0.10
)

# View confidence interval
result$confidence_interval
result$scores  # Shows base, lowest, highest scores

# With CI ribbon band on the plot
result <- fhlus_score(
  df = sample_data,
  numerator = "housing_units",
  rank_var = "median_income",
  denom = "land_area",
  missing_pct = 0.10,
  show_ci_bands = TRUE  # Shaded envelope between CI curves
)
```

## Example: Policy Comparison

```r
# Compare two variables (e.g., all housing vs. affordable housing)
result_all <- fhlus_score(sample_data, "housing_units", "median_income",
                          "land_area", missing_pct = 0.10, graph = FALSE)
result_aff <- fhlus_score(sample_data, "affordable_units", "median_income",
                          "land_area", missing_pct = 0.10, graph = FALSE)

# Overlay both curves on one plot
comparison <- fhlus_compare(
  result_all, result_aff,
  label_a = "All Housing",
  label_b = "Affordable Housing",
  show_ci = TRUE,       # CI ribbon bands (optional)
  rank_var = "median_income"
)

comparison$plot        # ggplot object
comparison$scores      # Score table with CI bounds
comparison$difference  # Numeric difference
```

## Example: Multiple Opportunity Indices

```r
# Analyze multiple opportunity measures
sample_data$poverty_rate <- runif(10, 0.05, 0.30)

indices <- list(
  income = list(
    numerator = "housing_units",
    rank_var = "median_income",
    denom = "land_area"
  ),
  poverty = list(
    numerator = "housing_units",
    rank_var = "poverty_rate",
    denom = "land_area",
    ascending = FALSE  # Lower poverty is better
  )
)

results <- calculate_multiple_indices(sample_data, indices)
print(results$wide)
```

## Interpretation Guide

### Understanding FHLUS Scores

- **Positive scores (0 to +1)**: Housing units are more concentrated in higher-opportunity areas
  - **0.5 to 1.0**: Strong concentration in high-opportunity areas
  - **0.0 to 0.5**: Moderate concentration in high-opportunity areas

- **Negative scores (-1 to 0)**: Housing units are more concentrated in lower-opportunity areas
  - **-0.5 to 0.0**: Moderate concentration in low-opportunity areas
  - **-1.0 to -0.5**: Strong concentration in low-opportunity areas

- **Near-zero scores**: Distribution roughly proportional to available land area

### What Makes a "Good" Score?

This depends on context:

- For **affordable housing**: A positive score might indicate equitable access to opportunity
- For **market-rate housing**: Patterns may reflect market forces
- Consider scores in combination with other equity metrics

## Data Requirements

Your data must include:

1. **Numerator**: The variable of interest (e.g., housing units, affordable units)
2. **Rank variable**: Opportunity indicator(s) where higher values = better opportunity
   - For "lower is better" metrics (e.g., poverty rate), use negative values or the `ascending = FALSE` parameter
3. **Denominator**: Typically land area in the same units across observations

Minimum requirements:
- At least 2 observations
- Numeric columns for numerator, rank_var, and denominator
- No all-NA columns

## Spatial Data Support

```r
library(sf)

# Read spatial data
tracts_sf <- st_read("tracts.shp")

# Attribute-based exclusion (marks entire features)
tracts_prepared <- prepare_spatial_fhlus(
  spatial_data = tracts_sf,
  area_crs = 3857,              # Projected CRS for area calculation
  exclude_zones = "Open space", # Optional: exclude certain areas
  zone_column = "land_use"
)

# Spatial layer exclusion (true geometric intersection)
parks_sf <- st_read("parks.shp")
tracts_prepared <- prepare_spatial_fhlus(
  spatial_data = tracts_sf,
  area_crs = 3857,
  exclude_layer = parks_sf  # Dissolves & intersects with source features
)

# Both approaches can be combined (exclusions are additive)
tracts_prepared <- prepare_spatial_fhlus(
  spatial_data = tracts_sf,
  area_crs = 3857,
  exclude_zones = "Open space",
  zone_column = "land_use",
  exclude_layer = parks_sf
)

# Calculate FHLUS
result <- fhlus_score(
  df = tracts_prepared,
  numerator = "housing_units",
  rank_var = "median_income",
  denom = "available_area"  # Created by prepare_spatial_fhlus
)
```

## Export Results

```r
# Export to CSV
export_fhlus(result, filename = "my_fhlus_results", format = "csv")

# Export to Excel with plot
export_fhlus(result, filename = "my_fhlus_results",
             format = "xlsx", include_plot = TRUE)

# Export full R object
export_fhlus(result, filename = "my_fhlus_results", format = "rds")
```

## Getting Help

- **Documentation**: See `?fhlus_score`, `?grouped_fhlus`, or `?fhlus_compare` for detailed help
- **Vignettes**:
  - `vignette("fhlus-intro")`: Introduction and basic usage
  - `vignette("fhlus-advanced")`: Advanced features and spatial data
- **Issues**: Report bugs at <https://github.com/yourusername/fhlus/issues>

## Citation

If you use this package in publications, please cite:

```r
citation("fhlus")
```

## License

MIT License - see LICENSE file for details

## Contributing

Contributions are welcome! Please see CONTRIBUTING.md for guidelines.
