# cycif.importer

<!-- badges: start -->
<!-- badges: end -->

A comprehensive R package for analyzing CyCIF (Cyclic Immunofluorescence) multiplexed tissue imaging data. The package provides a modular three-stage pipeline for importing UNMICST cell segmentation data, applying gating thresholds, assigning regions of interest (ROIs), sampling cells, and generating summary statistics.

## Installation

You can install the development version of cycif.importer from GitHub:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install cycif.importer
devtools::install_github("your-username/cycif.importer")
```

## Quick Start

### Complete Pipeline

```r
library(cycif.importer)

# Run the complete pipeline with default settings
results <- run_complete_pipeline(
  data_dir = "/path/to/unmicst/data",
  roi_dir = "/path/to/roi/data",
  gate_table_path = "/path/to/gates.csv",
  output_dir = "/path/to/output"
)
```

### Modular Approach

```r
# Stage 1: Import and Gate
cell_data <- load_cell_data("/path/to/data")
gated_data <- cycif_import_and_gate(cell_data)

# Add marker combinations
gated_data <- calculate_marker_combinations(gated_data, get_default_marker_combinations())

# Stage 2: Sample
sampled_data <- cycif_sample(gated_data, sample_size = 50000)

# Stage 3: Summarize
results <- cycif_summarize(sampled_data)

# Save results
save_cycif_results(results, "/path/to/output")
```

## Pipeline Overview

The package implements a three-stage analysis pipeline:

### Stage 1: Import & Gate
- Load UNMICST cell segmentation data
- Standardize coordinate systems (pixel to micron conversion)
- Assign cells to regions of interest (ROIs) using polygon coordinates
- Apply single and double marker gating thresholds
- Expand ROI boundaries to include nearby cells

### Stage 2: Sample
- Sample cells according to specified strategy:
  - `"all_cells"`: Sample from all cells
  - `"roi_only"`: Sample only from ROI-assigned cells
- Configurable sample sizes per slide
- Combines data from multiple slides

### Stage 3: Summarize
- Generate summary statistics by slide and ROI
- Create stratified summaries based on cell populations
- Add slide and ROI metadata
- Support for custom filtering expressions

## Key Features

### Flexible Marker Combinations
Define complex marker combinations using formula syntax:

```r
custom_combos <- list(
  activated_cd8 = ~CD8ap & Ki67p,
  exhausted_cd8 = ~CD8ap & PD1p & LAG3p,
  ifn_positive = ~pSTAT1p | pSTAT3p | HLA_Ep
)

data_with_combos <- calculate_marker_combinations(gated_data, custom_combos)
```

### ROI Management
- Automatic cell assignment to polygonal ROIs
- ROI boundary expansion for including nearby cells
- Support for multiple ROI types per slide

### Scalable Processing
- Handles large datasets through sampling strategies
- Memory-efficient processing of multiple slides
- Parallelizable design for high-throughput analysis

### Comprehensive Output
- Cell-level data with all transformations
- ROI-level summary statistics
- Population-specific summaries
- Metadata integration

## Data Requirements

### Input File Formats

**Cell Data**: `*--unmicst_cell.csv`
- Required columns: `CellID`, `X_centroid`, `Y_centroid`, marker columns
- Marker columns should contain raw intensity values

**ROI Data**: `*-rois.csv` (optional)
- Required columns: `Name`, `all_points`
- `all_points` format: "x1,y1 x2,y2 x3,y3 ..." (space-separated coordinate pairs)

**Gate Thresholds**: CSV file (optional)
- Columns: `slideName` + marker columns
- Values should be log-transformed thresholds

**Metadata**: CSV files (optional)
- Slide metadata: must include `slideName` column
- ROI metadata: must include `ROIname` column

## Advanced Usage

### Custom Sampling Strategies
```r
# Sample only from ROI cells
sampled_roi <- cycif_sample(
  gated_data,
  sample_size = 10000,
  sampling_mode = "roi_only"
)
```

### Custom Summary Filters
```r
custom_filters <- list(
  "Epithelial" = ~PanCKp,
  "CD8_T_cells" = ~CD8ap & !PanCKp,
  "Activated_T" = ~(CD8ap | CD4p) & Ki67p
)

results <- cycif_summarize(sampled_data, summary_filters = custom_filters)
```

### ROI Expansion
```r
gated_data <- cycif_import_and_gate(
  cell_data = cell_data,
  roi_data = roi_data,
  expand_distance = 100  # microns
)
```

## Package Structure

The package is organized into logical modules:

- **Pipeline functions**: Core three-stage pipeline (`R/pipeline.R`)
- **Stage 1 helpers**: Data import and ROI assignment (`R/stage1_helpers.R`)
- **Stage 3 helpers**: Summary statistics (`R/stage3_helpers.R`)
- **Marker combinations**: Formula-based marker logic (`R/marker_combinations.R`)
- **File I/O**: Data loading and saving (`R/file_io.R`)
- **Complete pipeline**: High-level wrapper functions (`R/complete_pipeline.R`)

## Documentation

- Package documentation: `help(package = "cycif.importer")`
- Function help: `?function_name`
- Vignette: `vignette("cycif-analysis-pipeline", package = "cycif.importer")`

## Testing

Run the test suite:

```r
devtools::test()
```

## Contributing

Please read our contribution guidelines and submit issues or pull requests on GitHub.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
