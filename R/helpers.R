#' Validate data frame structure for cell data
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context Character string describing the context for error messages
#'
#' @return NULL (invisibly) if validation passes, otherwise stops with error
#' @keywords internal
validate_cell_data_structure <- function(data, required_cols) {
  # Get the variable name from the call
  data_name <- deparse(substitute(data))

  if (!is.data.frame(data)) {
    stop(sprintf("%s must be a data frame", data_name))
  }

  if (nrow(data) == 0) {
    warning(sprintf("%s is empty", data_name))
    return(invisible(NULL))
  }

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("%s must contain columns: %s",
                data_name, paste(missing_cols, collapse = ", ")))
  }

  invisible(NULL)
}

#' Standardize input data formats
#'
#' Converts input data to a standardized list of dataframes by slide
#'
#' @param data Input data: list of dataframes, single dataframe with slideName,
#'   or single dataframe without slideName
#'
#' @return List of dataframes by slide name
#' @export
#'
#' @examples
#' \dontrun{
#' # Single dataframe with slideName column
#' std_data <- standardize_input_data(my_data)
#'
#' # List of dataframes
#' std_data <- standardize_input_data(list(slide1 = df1, slide2 = df2))
#' }
standardize_input_data <- function(data) {
  if (is.null(data)) {
    return(list())
  }

  if (is.list(data) && !is.data.frame(data)) {
    # Already a list of dataframes
    return(data)
  } else if (is.data.frame(data)) {
    if ("slideName" %in% names(data)) {
      # Single dataframe with slideName column - split by slide
      return(split(data, data$slideName))
    } else {
      # Single dataframe without slideName - treat as single slide
      return(list("slide1" = data))
    }
  } else {
    stop("Input data must be a list of dataframes or a single dataframe")
  }
}

#' Add slide metadata to summary
#'
#' @param summary_data Data frame with summary statistics
#' @param slide_metadata Data frame with slide metadata
#'
#' @return Data frame with metadata joined
#' @export
add_slide_metadata <- function(summary_data, slide_metadata) {
  validate_cell_data_structure(summary_data, "slideName")
  validate_cell_data_structure(slide_metadata, "slideName")

  summary_data %>%
    dplyr::left_join(slide_metadata, by = "slideName")
}

#' Add ROI metadata to summary
#'
#' @param summary_data Data frame with summary statistics
#' @param roi_metadata Data frame with ROI metadata
#'
#' @return Data frame with metadata joined
#' @export
add_roi_metadata <- function(summary_data, roi_metadata) {
  validate_cell_data_structure(summary_data, "ROIname")
  validate_cell_data_structure(roi_metadata, "ROIname")

  summary_data %>%
    dplyr::left_join(roi_metadata, by = "ROIname")
}

#' Convert logical columns to numeric (for saving)
#'
#' @param df Data frame with potential logical columns
#'
#' @return Data frame with logical columns converted to numeric
#' @keywords internal
convert_logical_to_numeric <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(is.logical), as.numeric))
}
