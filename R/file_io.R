#' Match files using glob pattern and extract slide names
#'
#' Internal helper that converts a glob pattern to regex and matches files.
#' The first `*` in the pattern is used to extract the slide name.
#'
#' @param dir Directory to search for files
#' @param pattern Glob pattern where first `*` represents slide name
#'   (e.g., "*--unmicst_cell.csv*" or "Sample_*_data.csv")
#'
#' @return Named character vector of matched file paths with slide names as names
#' @keywords internal
match_files_with_pattern <- function(dir, pattern) {
  # Check that pattern contains at least one *
  if (!grepl("\\*", pattern)) {
    stop("File pattern must contain at least one '*' to indicate slide name position")
  }

  # Convert glob pattern to regex
  # Escape special regex characters except * and ?
  regex_pattern <- stringr::str_replace_all(pattern, "([.^$\\\\|+{}\\[\\]()])", "\\\\\\1") |>
    # Replace ? with .
    stringr::str_replace_all(stringr::fixed("?"), ".") |>
    # Replace first * with (.+?) to capture slide name, others with .*
    stringr::str_replace(stringr::fixed("*"), "(.+)") |>
    stringr::str_replace_all(stringr::fixed("*"), ".*")

  # Find matching files
  all_files <- list.files(
    dir,
    full.names = TRUE
  )
  slide_names <- stringr::str_match(basename(all_files), paste0("^", regex_pattern, "$"))[, 2]
  matched_indices <- which(!is.na(slide_names))
  purrr::set_names(
    all_files[matched_indices],
    slide_names[matched_indices]
  )
}

#' Load cell data from UNMICST files
#'
#' Loads cell segmentation data from UNMICST CSV files in a directory.
#' Files should follow the naming pattern specified by `file_pattern`.
#'
#' @param data_dir Directory containing cell data files
#' @param slide_filter Optional vector of slide names to load
#' @param file_pattern Glob pattern for matching files. The first `*` indicates
#'   where the slide name appears in the filename. Default: "*--unmicst_cell.csv*"
#'   Examples: "Sample_*_cells.csv", "*_data.csv.gz"
#'
#' @return List of data frames by slide name
#' @export
#'
#' @examples
#' \dontrun{
#' # Load all cell data files with default pattern
#' cell_data <- cycif_load_cell_data("/path/to/data")
#'
#' # Load specific slides only
#' cell_data <- cycif_load_cell_data("/path/to/data", slide_filter = c("slide1", "slide2"))
#'
#' # Use custom file pattern
#' cell_data <- cycif_load_cell_data("/path/to/data", file_pattern = "Sample_*_cells.csv.gz")
#' }
cycif_load_cell_data <- function(data_dir, slide_filter = NULL, file_pattern = "*--unmicst_cell.csv*") {
  # Match files using pattern
  matched <- match_files_with_pattern(data_dir, file_pattern)

  if (length(matched) == 0) {
    stop(sprintf("No files matching pattern '%s' found in data directory", file_pattern))
  }

  # Apply slide filter if provided
  if (!is.null(slide_filter)) {
    matched <- matched[slide_filter]
    if (length(matched) == 0) {
      stop("No files found matching slide filter")
    }
  }

  # Load all files
  cell_data <- purrr::imap(
    matched,
    \(file, slide) {
      message(sprintf("Loading %s...", basename(file)))
      readr::read_csv(file, show_col_types = FALSE) |>
        dplyr::mutate(slideName = slide)
    }
  )

  return(cell_data)
}

#' Load ROI polygon data
#'
#' Loads region of interest (ROI) polygon data from CSV files.
#' Files should follow the naming pattern specified by `file_pattern`.
#'
#' @param roi_dir Directory containing ROI files
#' @param slide_names Optional vector of slide names to load ROIs for
#' @param file_pattern Glob pattern for matching files. The first `*` indicates
#'   where the slide name appears in the filename. Default: "*-rois.csv"
#'   Examples: "Sample_*_rois.csv", "*_roi_data.csv"
#'
#' @return Data frame with ROI polygon data, or NULL if no files found
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ROI data for all slides with default pattern
#' roi_data <- cycif_load_roi_data("/path/to/rois")
#'
#' # Load ROI data for specific slides
#' roi_data <- cycif_load_roi_data("/path/to/rois", slide_names = c("slide1", "slide2"))
#'
#' # Use custom file pattern
#' roi_data <- cycif_load_roi_data("/path/to/rois", file_pattern = "Sample_*_rois.csv")
#' }
cycif_load_roi_data <- function(roi_dir, file_pattern = "*-rois.csv") {
  # Match files using pattern
  matched <- match_files_with_pattern(roi_dir, file_pattern)

  if (length(matched) == 0) {
    warning(sprintf("No files matching pattern '%s' found", file_pattern))
    return(NULL)
  }

  roi_data <- purrr::map(
    matched,
    \(file) {
      message(sprintf("Loading ROIs from %s...", basename(file)))
      readr::read_csv(file, show_col_types = FALSE)
    }
  ) |>
    dplyr::bind_rows(.id = "slideName")

  return(roi_data)
}
