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
#' @param data Data frame with with `SlideName` column
#' @param slide_metadata Data frame with slide metadata
#'
#' @return Data frame with metadata joined
#' @export
add_slide_metadata <- function(data, slide_metadata) {
  if (is.null(slide_metadata) || nrow(slide_metadata) == 0) {
    message("No slide metadata provided, returning data unchanged.")
    return(data)
  }
  validate_cell_data_structure(data, "slideName")
  validate_cell_data_structure(slide_metadata, "slideName")

  # Ensure that slideName is unique in slide_metadata
  if (anyDuplicated(slide_metadata$slideName) > 0) {
    stop(
      "`slide_metadata` contains duplicate slide names. Please ensure each slideName is unique. ",
      printf(
        "Duplicate slide names: %s",
        paste(unique(slide_metadata$slideName[duplicated(slide_metadata$slideName)]), collapse = ", ")
      )
    )
  }

  powerjoin::power_left_join(
    data,
    slide_metadata,
    by = "slideName",
    check = powerjoin::check_specs(
      unmatched_keys_left = "warn"
    )
  )
}

#' Add ROI metadata to summary
#'
#' @param data Data frame with with `SlideName` column
#' @param roi_metadata Data frame with ROI metadata
#'
#' @return Data frame with metadata joined
#' @export
add_roi_metadata <- function(data, roi_metadata) {
  if (is.null(roi_metadata) || nrow(roi_metadata) == 0) {
    message("No ROI metadata provided, returning data.")
    return(data)
  }
  validate_cell_data_structure(data, "ROIname")
  validate_cell_data_structure(roi_metadata, "ROIname")

  # Ensure that ROIname is unique in roi_metadata
  if (anyDuplicated(roi_metadata$ROIname) > 0) {
    stop(
      "`roi_metadata` contains duplicate ROInames. Please ensure each ROIname is unique. ",
      sprintf("Duplicate ROInames: %s",
        paste(unique(roi_metadata$ROIname[duplicated(roi_metadata$ROIname)]), collapse = ", ")
      )
    )
  }

  powerjoin::power_left_join(
    data,
    roi_metadata,
    by = "ROIname",
    check = powerjoin::check_specs(
      unmatched_keys_left = "warn"
    )
  )
}

#' Convert logical columns to numeric (for saving)
#'
#' @param df Data frame with potential logical columns
#'
#' @return Data frame with logical columns converted to numeric
#' @keywords internal
convert_logical_to_numeric <- function(df) {
  mutate(df, across(where(is.logical), as.numeric))
}
