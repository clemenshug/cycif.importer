# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`cycif.importer` is an R package for analyzing CyCIF (Cyclic Immunofluorescence) multiplexed tissue imaging data from the MCMICRO pipeline. It provides a modular three-stage pipeline for importing cell segmentation data, applying marker gates, assigning cells to regions of interest (ROIs), and generating stratified summaries.

## Development Commands

### Building and Testing

```r
# Load the package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a specific test file
testthat::test_file("tests/testthat/test-overlap-resolution.R")

# Build documentation
devtools::document()

# Check package
devtools::check()

# Install locally
devtools::install()
```

### Running Example Pipeline

```r
library(cycif.importer)

# Set up paths to example data
data_dir <- system.file("extdata/example_quants", package = "cycif.importer")
roi_dir <- system.file("extdata/example_rois", package = "cycif.importer")
gate_file <- system.file("extdata/example_gates.csv", package = "cycif.importer")
output_dir <- tempdir()

# Run complete pipeline
results <- cycif_pipeline(
  counts = data_dir,
  rois = roi_dir,
  gate_table = gate_file,
  output_dir = output_dir,
  sample_size = 25000,
  expand_distance = 50
)
```

## Architecture

### Three-Stage Pipeline

The package implements a modular pipeline where data flows through three sequential stages:

1. **Import & Gate** (`R/file_io.R`, `R/roi.R`, `R/gating.R`)
   - Load UNMICST cell quantification data
   - Load ROI polygon definitions
   - Assign cells to ROIs using spatial point-in-polygon operations
   - Apply single and double marker gates
   - Expand ROI boundaries to include nearby cells

2. **Sample** (`R/sample.R`)
   - Subsample cells with flexible strategies (all cells or ROI-only)
   - Configurable sample sizes per slide

3. **Summarize** (`R/summary.R`)
   - Generate stratified summaries with custom filters and groupings
   - Export results as CSV files

### Critical Spatial Processing

**ROI Assignment** (`R/roi.R`):
- Uses `sf` (simple features) package for spatial operations
- Converts ROI polygon strings to sf geometries via `roi_df_to_sf()`
- Performs spatial intersection with `sf::st_within()` to find cells in ROIs
- Supports ROI boundary expansion using k-nearest neighbor search (FNN package)
- Handles overlapping ROIs via `roi_priority` parameter (first in list trumps later)

**ROI Overlap Detection & Resolution** (`R/overlap.R`):
- `analyze_roi_overlaps()` detects four types of overlap issues:
  - `multiway_overlap`: Three or more ROIs overlap (requires manual ROI redrawing)
  - `large_overlap`: >50% overlap (requires roi_priority or manual redrawing)
  - `fully_contained`: One ROI fully inside another (resolvable with roi_priority)
  - `small_overlap`: <50% overlap (resolvable with `resolve_roi_overlaps()`)
- `resolve_polygon_overlaps()` splits small overlaps using Voronoi diagrams:
  - Creates Voronoi tessellation of intersection region
  - Finds shortest path between polygon touch points using `sfnetworks`
  - Splits both polygons along this path with `lwgeom::st_split()`
  - Keeps larger piece from each split

### Gating System

**Single Gates** (`R/gating.R`):
- Thresholds are log2-transformed intensities
- Creates boolean columns with 'p' suffix (e.g., `CD8a` → `CD8ap`)
- Comparison: `marker_value > exp(log2_threshold)`

**Double Gates**:
- Combines two single gates with AND logic
- Column naming: `marker1p` + `marker2p` → `marker1pmarker2p`

**Marker Combinations**:
- Defined as R formula expressions supporting `&`, `|`, `!` operators
- Example: `regulatory_T = ~CD4p & FoxP3p`
- Evaluated in data context using `rlang::eval_tidy()`

### Data Structure Flow

The package uses a consistent pattern of "standardized input" throughout:

- **List by slide**: Primary internal format `list(slide1 = df1, slide2 = df2)`
- **Single dataframe with slideName**: Alternative accepted format
- `standardize_input_data()` helper normalizes all inputs to list-by-slide format

Key coordinate transformations:
- Input: `X_centroid`, `Y_centroid` (pixels)
- Standardized: `Xt`, `Yt` (microns) via `scale_factor` (default 0.65 pixels/micron)

## Key Implementation Patterns

### Input Flexibility

The main pipeline function `cycif_pipeline()` accepts EITHER file paths OR pre-loaded data for all inputs (counts, rois, gates, metadata). The `resolve_input_data()` helper determines whether to load from file or use provided data.

### Coordinate Systems

- All ROI polygons and cell coordinates are converted to microns early in processing
- The `scale_factor` parameter (default 0.65) controls pixel→micron conversion
- ROI expansion distances are specified in microns, not pixels

### Spatial Feature Processing

When working with ROI geometries:
- Always validate geometries with `sf::st_make_valid()` before spatial operations
- Use `sf::st_is_valid(reason = TRUE)` to diagnose invalid geometries
- Cast multipart geometries carefully - helper `cast_quiet()` suppresses expected warnings
- The `parse_point_str()` function handles conversion from string coordinates to polygon matrices

### Metadata Joining

Slide and ROI metadata are added via `add_slide_metadata()` and `add_roi_metadata()`:
- Use `purrr::quietly()` wrapper to suppress duplicate warnings in pipeline
- Join on `slideName` for slide metadata, `ROIname` for ROI metadata
- Metadata is optional - functions handle NULL gracefully

## Testing Strategy

- `test-loaders.R`: File I/O and data loading
- `test-assign-rois.R`: ROI assignment and spatial operations
- `test-overlap-resolution.R`: Overlap detection and Voronoi-based splitting
- `test-gating-combos.R`: Single/double gates and marker combinations
- `test-summary.R`: Stratified summary generation
- `test-pipeline-output.R`: Output file formats
- `test-full-pipeline-integration.R`: End-to-end pipeline execution

## Common Pitfalls

1. **ROI Overlaps**: The pipeline will error if overlapping ROIs are detected without a resolution strategy. Always run `analyze_roi_overlaps()` first or provide `roi_priority`.

2. **Scale Factor Mismatch**: The scale_factor must match the pixel→micron conversion of your microscopy system. Default is 0.65, but this varies by instrument.

3. **Gate Thresholds**: Remember gates are in log2 space. The comparison is `marker_value > exp(log2_threshold)`, not `log2(marker_value) > threshold`.

4. **ROI File Naming**: ROI files MUST match pattern `{slideName}-rois.csv` and cell data files MUST match `{slideName}--unmicst_cell.csv[.gz]`. The slideName portion must be identical.

5. **Polygon Validity**: When creating or modifying ROI polygons, ensure first and last points are identical (closed polygon) and points form a valid geometry (no self-intersections).

## Package Dependencies

Critical spatial packages:
- `sf`: Simple features for spatial geometries
- `sfnetworks`: Network analysis on spatial features (used in Voronoi splitting)
- `lwgeom`: Advanced geometry operations (polygon splitting)
- `FNN`: Fast k-nearest neighbor search (ROI expansion)

Standard tidyverse:
- `dplyr`, `purrr`, `rlang`, `readr`, `stringr`
- `powerjoin`: Enhanced joining operations
