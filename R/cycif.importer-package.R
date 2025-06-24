#' cycif.importer: CyCIF Analysis Pipeline
#'
#' @description
#' A comprehensive toolkit for analyzing CyCIF (Cyclic Immunofluorescence)
#' multiplexed tissue imaging data. The package provides a modular three-stage
#' pipeline for importing UNMICST cell segmentation data, applying gating
#' thresholds, assigning regions of interest (ROIs), sampling cells, and
#' generating summary statistics.
#'
#' @section Main Pipeline Functions:
#' The core pipeline consists of three main stages:
#'
#' \describe{
#'   \item{\code{\link{cycif_import_and_gate}}}{Stage 1: Import data and apply gates}
#'   \item{\code{\link{cycif_sample}}}{Stage 2: Sample cells according to strategy}
#'   \item{\code{\link{cycif_summarize}}}{Stage 3: Generate summary statistics}
#' }
#'
#' @section Complete Pipeline:
#' For convenience, use \code{\link{run_complete_pipeline}} to run all stages
#' with file I/O handling.
#'
#' @section Marker Combinations:
#' Create complex marker combinations using \code{\link{calculate_marker_combinations}}
#' with formula-based expressions.
#'
#' @section File I/O:
#' \describe{
#'   \item{\code{\link{load_cell_data}}}{Load UNMICST cell data}
#'   \item{\code{\link{load_roi_data}}}{Load ROI polygon data}
#'   \item{\code{\link{save_cycif_results}}}{Save analysis results}
#' }
#'
#' @keywords internal
"_PACKAGE"
