#' Validate data frame structure for cell data
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context Character string describing the context for error messages
#' @param no_missing Logical indicating if missing values are allowed
#'
#' @return NULL (invisibly) if validation passes, otherwise stops with error
#' @keywords internal
validate_cell_data_structure <- function(data, required_cols, no_missing = FALSE) {
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

  if (no_missing) {
    # Missing values are NA, '' or NULL
    na_cols <- purrr::map_lgl(
      data[required_cols],
      \(x) any(is.na(x) | x == "" | is.null(x))
    )
    if (any(na_cols)) {
      stop(sprintf("%s contains missing values in columns: %s",
                   data_name, paste(required_cols[na_cols], collapse = ", ")))
    }
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
      sprintf(
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

#' Resolve input data from either path or pre-loaded data
#'
#' Flexible helper function that accepts either file paths or pre-loaded data
#' and returns the appropriate data structure. This allows pipeline functions
#' to work with both file-based and in-memory workflows.
#'
#' @param input Either a file/directory path (character) or pre-loaded data
#' @param loader_func Function to use for loading from path (e.g., cycif_load_cell_data)
#' @param loader_args List of additional arguments to pass to loader_func
#' @param data_type Character string describing the data type for error messages
#'
#' @return Loaded or validated data structure
#' @keywords internal
resolve_input_data <- function(input, loader_func, loader_args = list(), data_type = "data") {
  if (is.null(input)) {
    return(NULL)
  }
  
  if (is.character(input) && length(input) == 1) {
    # Input is a path - load using the provided loader function
    message(sprintf("Loading %s from path: %s", data_type, input))
    do.call(loader_func, c(list(input), loader_args))
  } else {
    # Input is pre-loaded data - validate and return
    if (is.data.frame(input) || (is.list(input) && !is.data.frame(input))) {
      message(sprintf("Using pre-loaded %s", data_type))
      input
    } else {
      stop(sprintf("Invalid %s input: must be a file path, data frame, or list of data frames", data_type))
    }
  }
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
