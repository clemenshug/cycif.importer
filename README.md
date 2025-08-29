cycif.importer
================
Clemens Hug

<!-- README.md is generated from README.Rmd. Please edit that file -->

# cycif.importer

<!-- badges: start -->

<!-- badges: end -->

The `cycif.importer` package provides functions to import CyCIF (Cyclic
Immunofluorescence) multiplexed tissue imaging data ran through the
[MCMICRO](https://mcmicro.org/) pipeline into R and perform basic ROI
and marker gating.

## Features

- **Data Import**: Load UNMICST cell segmentation data and ROI
  definitions
- **Gating**: Apply single and double marker gating thresholds
- **ROI Assignment**: Assign cells to regions of interest polygons
- **Marker Combinations**: Phenotype cells using custom marker
  combinations
- **Sampling**: Subsample cells
- **Summarization**: Generate stratified summaries with custom groupings

## Installation

You can install the development version of cycif.importer from
[GitHub](https://github.com/clemenshug/cycif.importer) with:

``` r
# install.packages("remotes")
remotes::install_github("clemenshug/cycif.importer")
```

## Pipeline Overview

The `cycif.importer` package implements a modular three-stage analysis
pipeline:

### Stage 1: Import & Gate

- Load cell quantification data from UNMICST output
- Load ROI definitions
- Apply single and double marker gates
- Assign cells to ROIs with optional boundary expansion

### Stage 2: Sample

- Apply flexible sampling strategies
- Support for all-cells or ROI-only sampling
- Configurable sample sizes per slide

### Stage 3: Summarize

- Generate summary statistics with custom filters
- Support for multiple grouping strategies
- Export results as CSV files

## Example with Built-in Data

The package includes example data from two tissue samples with
pre-defined gates and ROIs:

``` r
library(cycif.importer)
library(dplyr)

# Set up paths to example data
data_dir <- system.file("extdata/example_quants", package = "cycif.importer")
roi_dir <- system.file("extdata/example_rois", package = "cycif.importer")
gate_file <- system.file("extdata/example_gates.csv", package = "cycif.importer")
output_dir <- tempdir()

# Run the complete pipeline
results <- cycif_pipeline(
  counts = data_dir,
  rois = roi_dir,
  gate_table = gate_file,
  output_dir = output_dir,
  summary_filters = list(
    `PanCK+` = ~PanCKp,
    `PanCK-` = ~!PanCKp,
    `CD8+` = ~CD8ap,
    `CD4+` = ~CD4p
  ),
  summary_groups = list(
    by_roi = c("slideName", "ROI", "ROIname"),
    by_slide = c("slideName"),
    by_roi_type = c("slideName", "ROIname")
  ),
  sample_size = 25000,
  sampling_mode = "all_cells",
  expand_distance = 50  # Expand ROI boundaries by 50 microns
)
```

The `output_dir` directory will contain the following files:

- `sampled_cells.csv.gz`: Cells sampled from the dataset with gates and
  ROI assignments
- Summaries for each of the defined filter and grouping combinations
  e.g.
  - `by_roi__PanCK+.csv.gz`: Summary statistics grouped by ROI including
    only PanCK+ cells
  - `by_slide__CD8+.csv.gz`: Summary statistics grouped by slide
    including only CD8+ cells

<!-- -->

    #>  [1] "sampled_cells.csv.gz"               "summary_by_roi__CD4+.csv.gz"       
    #>  [3] "summary_by_roi__CD8+.csv.gz"        "summary_by_roi__PanCK-.csv.gz"     
    #>  [5] "summary_by_roi__PanCK+.csv.gz"      "summary_by_roi_type__CD4+.csv.gz"  
    #>  [7] "summary_by_roi_type__CD8+.csv.gz"   "summary_by_roi_type__PanCK-.csv.gz"
    #>  [9] "summary_by_roi_type__PanCK+.csv.gz" "summary_by_slide__CD4+.csv.gz"     
    #> [11] "summary_by_slide__CD8+.csv.gz"      "summary_by_slide__PanCK-.csv.gz"   
    #> [13] "summary_by_slide__PanCK+.csv.gz"

## Modular Usage

You can also use individual functions for more control:

``` r
# Load data step by step
cell_data <- cycif_load_cell_data(data_dir)
roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))

# Apply gates
gates <- readr::read_csv(gate_file)
gated_data <- cycif_apply_gates(cell_data, gates)

# Assign ROIs
data_with_rois <- cycif_assign_rois(
  gated_data,
  roi_data,
  scale_factor = 0.65,
  expand_distance = 50
)

# Sample cells
sampled_data <- cycif_sample(
  data_with_rois,
  sample_size = 10000,
  sampling_mode = "roi_only"
)

# Generate summaries
summaries <- cycif_summarize(
  sampled_data,
  summary_filters = list(`CD8+` = ~CD8ap),
  summary_groups = list(by_slide = "slideName")
)
```

## Data Formats

The package expects specific input file formats. Here are examples using
the built-in test data:

### Cell Quantification Data (UNMICST Output)

Cell-level quantification files from UNMICST segmentation (`.csv` or
`.csv.gz` format)

#### Directory Structure

The cell quantification data directory should contain one `.csv` or
`.csv.gz` file per slide:

    data_dir/
    ├── LSP11060--unmicst_cell.csv.gz
    ├── LSP11064--unmicst_cell.csv.gz
    └── LSP11065--unmicst_cell.csv

- Files should follow the pattern: `{slideName}--unmicst_cell.csv[.gz]`
- The `slideName` portion (before `--unmicst_cell`) must match the
  `slideName` in your gate thresholds file
- Compressed (`.gz`) files are automatically detected and handled

#### Example

| CellID | X_centroid | Y_centroid | Area |  PanCK |    CD8a |     CD4 |  FoxP3 |
|-------:|-----------:|-----------:|-----:|-------:|--------:|--------:|-------:|
| 117590 |   10665.23 |    7437.14 |  106 | 648.93 | 1055.87 | 1874.10 | 569.80 |
| 135190 |   20959.12 |    6581.12 |  135 | 694.76 | 1152.69 | 2156.97 | 793.32 |
| 288659 |    9708.67 |   13880.33 |   82 | 699.87 | 1254.49 | 2169.05 | 603.46 |

Example Cell Quantification Data

**Required columns**:

- `CellID`: Unique cell identifier
- `X_centroid`, `Y_centroid`: Cell center coordinates (pixels)
- **Marker columns**: Quantified intensity values for each marker

**Optional columns**:

- `Area`: Cell area
- `MajorAxisLength`, `MinorAxisLength`: Cell shape metrics
- Additional morphological features

### Gate Thresholds File

CSV file containing marker intensity thresholds for each slide:

| slideName | PanCK | CD8a |  CD4 | FoxP3 | Ki67 |
|:----------|------:|-----:|-----:|------:|-----:|
| LSP11060  |  7.40 |  7.5 | 8.50 |  7.65 |  7.5 |
| LSP11064  |  6.95 |  7.6 | 8.55 |  7.40 |  7.7 |

Example Gate Thresholds (log2 transformed intensities)

**Required columns**:

- `slideName`: Must match slide names in cell data files
- **Marker columns**: Log2-transformed intensity thresholds for each
  marker

### Region of Interest (ROI) Files

CSV files defining polygon regions for each slide.

#### Directory Structure

The ROI directory should contain one `.csv` file per slide with ROI
definitions:

    roi_dir/
    ├── LSP11060-rois.csv
    ├── LSP11064-rois.csv
    └── LSP11065-rois.csv

- Files should follow the pattern: `{slideName}-rois.csv`
- The `slideName` portion (before `-rois`) must match the slide names in
  your cell quantification files
- Each file contains all ROI polygons for that specific slide

#### Example

|    Id | Name                | Text                | type    |
|------:|:--------------------|:--------------------|:--------|
| 17690 | Incidental STIC     | Incidental STIC     | Polygon |
| 17691 | Incidental Fimbriae | Incidental Fimbriae | Polygon |
| 17693 | Incidental STIL     | Incidental STIL     | Polygon |

Example ROI Definitions

**Required columns**:

- `Id`: Unique ROI identifier
- `Name`: ROI name/identifier
- `all_points`: Polygon coordinates as “x1,y1 x2,y2 …” string

**Optional columns**:

- `Text`: ROI description
- Geometric properties (X, Y, Width, Height, etc.)

### Slide Metadata (Optional)

CSV file with additional slide-level information:

| slideName | patient_id | tissue_type    | treatment |
|:----------|:-----------|:---------------|:----------|
| LSP11060  | P001       | Ovary          | Control   |
| LSP11064  | P002       | Fallopian tube | Treated   |

Example Slide Metadata Structure

**Required columns**:

- `slideName`: Must match slide names in other files

**Optional columns**:

- Any additional slide-level annotations

### ROI Metadata (Optional)

CSV file with additional ROI-level information:

| ROIname             | region_type | pathologist_score |
|:--------------------|:------------|------------------:|
| Incidental STIC     | Lesion      |                 3 |
| Incidental Fimbriae | Normal      |                 1 |
| Incidental STIL     | Lesion      |                 2 |

Example ROI Metadata Structure

**Required columns**:

- `ROIname`: Must match ROI names in ROI definition files

**Optional columns**:

- Any additional ROI-level annotations

### Output Data

- **Sampled Cells**: Cell-level data with gates, ROI assignments, and
  metadata
- **Summaries**: Aggregated statistics by user-defined groups and
  filters

## Customization

The pipeline supports extensive customization:

``` r
# Custom marker combinations
marker_combos <- list(
  regulatory_T = ~CD4p & FoxP3p,
  cytotoxic_T = ~CD8ap & GZMBp,
  exhausted_T = ~CD8ap & PD1p
)

# Custom double gates
double_gates <- data.frame(
  marker1 = c("CD4", "CD8a"),
  marker2 = c("FoxP3", "GZMB")
)

# Run with custom parameters
results <- cycif_pipeline(
  data_dir = data_dir,
  roi_dir = roi_dir,
  marker_combinations = marker_combos,
  double_gates = double_gates,
  scale_factor = 0.5,  # Different pixel-to-micron conversion
  expand_distance = 100,  # Larger ROI expansion
  output_dir = output_dir
)
```
