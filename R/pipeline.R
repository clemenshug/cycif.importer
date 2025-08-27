#' Run Complete CyCIF Analysis Pipeline
#'
#' Runs the complete CyCIF analysis pipeline from data loading through
#' result saving. Accepts either file paths or pre-loaded data for maximum flexibility.
#'
#' @param counts Either a directory path containing cell data files, or pre-loaded
#'   cell data as a list of data frames by slide name or single data frame with slideName column
#' @param rois Either a directory path containing ROI files, or pre-loaded
#'   ROI data as a data frame (defaults to `counts` if `counts` is a path)
#' @param gate_table Either a path to gate table CSV file, or pre-loaded
#'   gate table data frame (optional)
#' @param slide_info Either a path to slide metadata CSV file, or pre-loaded
#'   slide metadata data frame (optional)
#' @param roi_info Either a path to ROI metadata CSV file, or pre-loaded
#'   ROI metadata data frame (optional)
#' @param output_dir Directory to save results
#' @param summary_filters Named list of formula expressions for creating
#'   stratified summaries. Default creates PanCK+/- summaries.
#'   Example: `list("panCK+" = ~PanCKp, "CD8+" = ~CD8ap)`
#' @param summary_groups Named list of character vectors defining different
#'   grouping strategies for summaries. Default groups by ROI.
#'   Example: `list("by_roi" = c("slideName", "ROI", "ROIname"), by_slide = c("slideName"))`
#' @param sampling_mode Sampling mode, either "all_cells" or "roi_only"
#' @param sample_size Number of cells to sample per slide (NULL for no sampling)
#' @param marker_combinations List of marker combination definitions. If NULL,
#'   uses default combinations. Example: `list(strange_cells = ~p53p & CD11cp)`
#' @param double_gates Data frame with marker1, marker2 columns for double gates
#' @param scale_factor Scale factor to convert pixels to microns (default 0.65)
#' @param expand_distance Distance in microns to expand ROIs (default 50,
#'   set to 0 to disable)
#' @param slide_filter Optional vector of slide names to process
#'
#' @return List with analysis results (also saves files to `output_dir`)
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic pipeline run with file paths
#' results <- cycif_pipeline(
#'   counts = "/path/to/data",
#'   output_dir = "/path/to/output"
#' )
#'
#' # Pipeline with pre-loaded data
#' cell_data <- cycif_load_cell_data("/path/to/data")
#' roi_data <- cycif_load_roi_data("/path/to/rois")
#' results <- cycif_pipeline(
#'   counts = cell_data,
#'   rois = roi_data,
#'   output_dir = "/path/to/output"
#' )
#'
#' # Mixed approach - some paths, some pre-loaded data
#' results <- cycif_pipeline(
#'   counts = "/path/to/data",
#'   rois = my_roi_dataframe,
#'   gate_table = "/path/to/gates.csv",
#'   output_dir = "/path/to/output",
#'   summary_filters = list("CD8+" = ~CD8ap, "CD4+" = ~CD4p),
#'   sample_size = 25000,
#'   sampling_mode = "roi_only"
#' )
#' }
cycif_pipeline <- function(
  counts,
  rois = NULL,
  gate_table = NULL,
  slide_info = NULL,
  roi_info = NULL,
  output_dir,
  summary_filters = list(`PanCK+` = ~PanCKp, `PanCK-` = ~!PanCKp),
  summary_groups = list(
    by_roi = c("slideName", "ROI", "ROIname"),
    by_slide = c("slideName"),
    by_roi_type = c("slideName", "ROIname")
  ),
  sampling_mode = "all_cells",
  sample_size = 50000,
  marker_combinations = NULL,
  double_gates = NULL,
  scale_factor = 0.65,
  expand_distance = 50,
  slide_filter = NULL
) {

  message("Starting complete CyCIF pipeline...")

  # Load or use provided cell data
  cell_data <- resolve_input_data(
    counts,
    cycif_load_cell_data,
    list(slide_filter = slide_filter),
    "cell data"
  )

  # Handle ROI data - default to counts path if rois is NULL and counts is a path
  if (is.null(rois) && is.character(counts)) {
    rois <- counts
  }
  roi_data <- resolve_input_data(
    rois,
    function(path) cycif_load_roi_data(path, names(cell_data)),
    list(),
    "ROI data"
  )

  # Load metadata tables
  gate_thresholds <- resolve_input_data(
    gate_table,
    function(path) readr::read_csv(path, show_col_types = FALSE),
    list(),
    "gate table"
  )

  slide_metadata <- resolve_input_data(
    slide_info,
    function(path) readr::read_csv(path, show_col_types = FALSE),
    list(),
    "slide metadata"
  )

  roi_metadata <- resolve_input_data(
    roi_info,
    function(path) readr::read_csv(path, show_col_types = FALSE),
    list(),
    "ROI metadata"
  )

  message("Assigning cells to ROIs...")
  cell_data_roi <- cycif_assign_rois(
    cell_data,
    roi_data = roi_data,
    scale_factor = scale_factor,
    expand_distance = expand_distance
  )

  # Use default double gates if none provided
  if (is.null(double_gates)) {
    double_gates <- get_default_double_gates()
  }
  message("Applying gates...")
  gated_data <- cycif_apply_gates(
    cell_data_roi,
    gate_thresholds,
    double_gates = double_gates
  )

  # Use default marker combinations if none provided
  if (is.null(marker_combinations)) {
    marker_combinations <- get_default_marker_combinations()
  }

  # Calculate marker combinations
  gated_data <- cycif_marker_combinations(gated_data, marker_combinations)

  message("Sampling cells...")
  sampled_data <- cycif_sample(
    gated_data,
    sample_size = sample_size,
    sampling_mode = sampling_mode
  )

  # Show warnings once, then silently
  add_slide_metadata_silently <- \(...) {
    purrr::quietly(add_slide_metadata)(...)[["result"]]
  }
  add_roi_metadata_silently <- \(...) {
    purrr::quietly(add_roi_metadata)(...)[["result"]]
  }
  sampled_data <- add_slide_metadata(
    sampled_data,
    slide_metadata
  )

  sampled_data <- add_roi_metadata(
    sampled_data,
    roi_metadata
  )

  message("Summarizing results...")
  summarized_data <- cycif_summarize(
    sampled_data,
    summary_filters = summary_filters,
    summary_groups = summary_groups
  )
  summarized_data <- purrr::map(
    summarized_data,
    \(x) {
      if ("slideName" %in% names(x)) {
        x <- add_slide_metadata_silently(x, slide_metadata)
      }
      if ("ROIname" %in% names(x)) {
        x <- add_roi_metadata_silently(x, roi_metadata)
      }
      x
    }
  )

  message("Saving results...")
  readr::write_csv(
    sampled_data,
    file.path(output_dir, "sampled_cells.csv.gz")
  )
  purrr::iwalk(
    summarized_data,
    \(x, n) {
      readr::write_csv(
        x,
        file.path(output_dir, paste0("summary_", n, ".csv.gz"))
      )
    }
  )

  gated_data <- purrr::map(
    gated_data,
    \(x) {
      x <- add_slide_metadata_silently(x, slide_metadata)
      x <- add_roi_metadata_silently(x, roi_metadata)
      x
    }
  )

  message("Pipeline complete!")
  return(
    list(
      all_cells = gated_data,
      sampled_cells = sampled_data
    ) |>
     c(summarized_data)
  )
}
