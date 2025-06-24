#' Load cell data from UNMICST files
#'
#' Loads cell segmentation data from UNMICST CSV files in a directory.
#' Files should follow the naming pattern "*--unmicst_cell.csv".
#'
#' @param data_dir Directory containing *--unmicst_cell.csv files
#' @param slide_filter Optional vector of slide names to load
#'
#' @return List of data frames by slide name
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all cell data files
#' cell_data <- cycif_load_cell_data("/path/to/data")
#'
#' # Load specific slides only
#' cell_data <- cycif_load_cell_data("/path/to/data", slide_filter = c("slide1", "slide2"))
#' }
cycif_load_cell_data <- function(data_dir, slide_filter = NULL) {
  file_list <- list.files(data_dir, pattern = "--unmicst_cell\\.csv$", full.names = TRUE)
  slide_names <- stringr::str_replace(basename(file_list), "--unmicst_cell\\.csv", "")

  if (length(file_list) == 0) {
    stop("No --unmicst_cell.csv files found in data directory")
  }

  # Apply slide filter if provided
  if (!is.null(slide_filter)) {
    filter_indices <- which(slide_names %in% slide_filter)
    if (length(filter_indices) == 0) {
      stop("No files found matching slide filter")
    }
    file_list <- file_list[filter_indices]
    slide_names <- slide_names[filter_indices]
  }

  # Load all files
  cell_data <- list()
  for (i in seq_along(file_list)) {
    message(sprintf("Loading %s...", basename(file_list[i])))
    data1 <- readr::read_csv(file_list[i], show_col_types = FALSE)
    data1$slideName <- slide_names[i]
    cell_data[[slide_names[i]]] <- data1
  }

  return(cell_data)
}

#' Load ROI polygon data
#'
#' Loads region of interest (ROI) polygon data from CSV files.
#' Files should follow the naming pattern "*-rois.csv".
#'
#' @param roi_dir Directory containing *-rois.csv files
#' @param slide_names Optional vector of slide names to load ROIs for
#'
#' @return Data frame with ROI polygon data, or NULL if no files found
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ROI data for all slides
#' roi_data <- cycif_load_roi_data("/path/to/rois")
#'
#' # Load ROI data for specific slides
#' roi_data <- cycif_load_roi_data("/path/to/rois", slide_names = c("slide1", "slide2"))
#' }
cycif_load_roi_data <- function(roi_dir, slide_names = NULL) {
  roi_files <- list.files(roi_dir, pattern = "-rois\\.csv$", full.names = TRUE)

  if (length(roi_files) == 0) {
    warning("No ROI files found")
    return(NULL)
  }

  roi_data <- list()

  for (roi_file in roi_files) {
    slide_name <- stringr::str_replace(basename(roi_file), "-rois\\.csv", "")

    if (!is.null(slide_names) && !slide_name %in% slide_names) {
      next
    }

    message(sprintf("Loading ROIs for %s...", slide_name))
    rois <- readr::read_csv(roi_file, show_col_types = FALSE)
    rois$slideName <- slide_name
    roi_data[[slide_name]] <- rois
  }

  if (length(roi_data) > 0) {
    return(dplyr::bind_rows(roi_data))
  } else {
    return(NULL)
  }
}

#' Save results to files
#'
#' Saves the results from `cycif_summarize()` to CSV and QS files.
#'
#' @param results Result list from `cycif_summarize()`
#' @param output_dir Directory to save files (will be created if it doesn't exist)
#'
#' @return NULL (called for side effects)
#' @export
#'
#' @examples
#' \dontrun{
#' results <- cycif_summarize(sampled_data)
#' cycif_save_results(results, "/path/to/output")
#' }
cycif_save_results <- function(results, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Save sampled data
  if (!is.null(results$sampled_cells)) {
    readr::write_csv(
      convert_logical_to_numeric(results$sampled_cells),
      file.path(output_dir, "sampled_cells.csv.gz")
    )
    message("Saved sampled_cells.csv.gz")
  }

  # Save all stratified summaries (new flexible approach)
  all_summaries <- c(
    list(roi = results$roi_summaries),
    results$stratified_summaries
  )

  for (summary_name in names(all_summaries)) {
    if (!is.null(all_summaries[[summary_name]])) {
      # Create a clean filename from the summary name
      clean_name <- gsub("[/<>:\"?\\*]", "_", summary_name)
      filename <- paste0(clean_name, "_summary.csv.gz")

      readr::write_csv(
        convert_logical_to_numeric(all_summaries[[summary_name]]),
        file.path(output_dir, filename)
      )
      message(sprintf("Saved %s", filename))
    }
  }

  # Save complete results as QS
  qs::qsave(results, file.path(output_dir, "all_results.qs"))
  message("Saved all_results.qs")
}
