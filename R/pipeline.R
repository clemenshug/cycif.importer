#' Run Complete CyCIF Analysis Pipeline
#'
#' Runs the complete CyCIF analysis pipeline from data loading through
#' result saving.
#'
#' @param data_dir Directory containing cell data files
#' @param roi_dir Directory containing ROI files (defaults to `data_dir`)
#' @param gate_table_path Path to gate table CSV file (optional)
#' @param slide_info_path Path to slide metadata CSV file (optional)
#' @param roi_info_path Path to ROI metadata CSV file (optional)
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
#' # Basic pipeline run
#' results <- run_complete_pipeline(
#'   data_dir = "/path/to/data",
#'   output_dir = "/path/to/output"
#' )
#'
#' # Advanced pipeline with custom parameters
#' results <- run_complete_pipeline(
#'   data_dir = "/path/to/data",
#'   roi_dir = "/path/to/rois",
#'   gate_table_path = "/path/to/gates.csv",
#'   output_dir = "/path/to/output",
#'   summary_filters = list("CD8+" = ~CD8ap, "CD4+" = ~CD4p),
#'   sample_size = 25000,
#'   sampling_mode = "roi_only"
#' )
#' }
cycif_pipeline <- function(
  data_dir,
  roi_dir = data_dir,
  gate_table_path = NULL,
  slide_info_path = NULL,
  roi_info_path = NULL,
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

  # Validate input
  if (!dir.exists(data_dir)) {
    stop("Data directory does not exist")
  }
  if (!dir.exists(roi_dir)) {
    stop(sprintf("ROI directory \"%s\" does not exist", roi_dir))
  }
  if (!is.null(gate_table_path) && !file.exists(gate_table_path)) {
    stop(sprintf("Gate table file \"%s\" does not exist", gate_table_path))
  }
  if (!is.null(slide_info_path) && !file.exists(slide_info_path)) {
    stop(sprintf("Slide info file \"%s\" does not exist", slide_info_path))
  }
  if (!is.null(roi_info_path) && !file.exists(roi_info_path)) {
    stop(sprintf("ROI info file \"%s\" does not exist", roi_info_path))
  }

  message("Loading data...")
  cell_data <- cycif_load_cell_data(
    data_dir,
    slide_filter = slide_filter
  )

  message("Loading ROI data...")
  roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))

  # Load metadata tables
  gate_thresholds <- NULL
  if (!is.null(gate_table_path)) {
    gate_thresholds <- readr::read_csv(gate_table_path, show_col_types = FALSE)
  }

  slide_metadata <- NULL
  if (!is.null(slide_info_path)) {
    slide_metadata <- readr::read_csv(slide_info_path, show_col_types = FALSE)
  }

  roi_metadata <- NULL
  if (!is.null(roi_info_path)) {
    roi_metadata <- readr::read_csv(roi_info_path, show_col_types = FALSE)
  }

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

  sampled_with_meta <- sampled_data
  if (!is.null(slide_metadata)) {
    message("Adding slide metadata...")
    sampled_with_meta <- left_join(
      sampled_with_meta,
      slide_metadata,
      by = "slideName"
    )
  }
  if (!is.null(roi_metadata)) {
    message("Adding ROI metadata...")
    sampled_with_meta <- left_join(
      sampled_with_meta,
      roi_metadata,
      by = "ROIname"
    )
  }

  message("Summarizing results...")
  summarized_data <- cycif_summarize(
    sampled_with_meta,
    summary_filters = summary_filters,
    summary_groups = summary_groups
  )

  message("Saving results...")
  readr::write_csv(
    sampled_with_meta,
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

  message("Pipeline complete!")
  return(
    list(
      all_cells = gated_data,
      sampled_cells = sampled_with_meta
    ) |>
     c(summarized_data)
  )
}
