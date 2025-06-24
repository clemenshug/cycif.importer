#' Summarize sampled data
#'
#' Generates summary statistics from sampled data with optional stratified summaries.
#'
#' @param data Single dataframe with sampled data, or a list of dataframes
#' @param summary_filters Named list of formula expressions for creating
#'   stratified summaries. Set to NULL to disable.
#'   Example: `list("CD8+" = ~CD8ap, "CD4+" = ~CD4p)`
#' @param summary_groups Character vector defining grouping variables.
#'   Default: `c("slideName", "ROI", "ROIname")`
#' @return List with summary statistics including:
#'   - `summary`: Main summary dataframe
#'   - `stratified`: Named list of filtered summaries (if filters provided)
#'   - `data`: The input data as single dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic summarization
#' results <- cycif_summarize(data)
#'
#' # With stratified summaries
#' results <- cycif_summarize(
#'   data,
#'   summary_filters = list("CD8+" = ~CD8ap, "CD4+" = ~CD4p)
#' )
#' }
cycif_summarize <- function(
  data,
  summary_filters = NULL,
  summary_groups = c("slideName", "ROI", "ROIname")
) {
  # Standardize and combine data into single dataframe
  data_list <- standardize_input_data(data)
  data <- dplyr::bind_rows(data_list, .id = "slideName")

  # Validate data structure and required columns
  validate_cell_data_structure(data, summary_groups)

  # Create main summary
  main_summary <- create_summary(data, summary_groups)

  # Create stratified summaries if filters provided
  stratified <- if (!is.null(summary_filters)) {
    create_stratified_summaries(data, summary_filters, summary_groups)
  } else {
    NULL
  }

  list(
    summary = main_summary,
    stratified = stratified,
    data = data
  )
}

#' Create summary statistics by grouping variables
#'
#' @param data Data frame with cell measurements
#' @param group_vars Character vector of column names to group by
#' @return Data frame with summary statistics
#' @keywords internal
create_summary <- function(data, group_vars) {
  group_syms <- rlang::syms(group_vars)

  data %>%
    dplyr::group_by(!!!group_syms) %>%
    dplyr::summarize(
      cell_count = dplyr::n(),
      dplyr::across(
        dplyr::where(function(x) is.numeric(x) || is.logical(x)),
        mean,
        na.rm = TRUE,
        .names = "mean_{.col}"
      ),
      .groups = "drop"
    )
}

#' Create stratified summaries using marker combinations
#'
#' @param data Data frame with cell measurements
#' @param summary_filters Named list of formula expressions
#' @param summary_groups Character vector of grouping variables
#' @return Named list of summary data frames
#' @keywords internal
create_stratified_summaries <- function(data, summary_filters, summary_groups) {
  # Use cycif_marker_combinations to efficiently compute all filters at once
  data_with_filters <- cycif_marker_combinations(data, summary_filters)

  # Extract filter column names
  filter_names <- names(summary_filters)

  # Create summaries for each filter
  summaries <- purrr::map(filter_names, function(filter_name) {
    # Check if filter column exists
    if (!filter_name %in% names(data_with_filters)) {
      message("Skipping ", filter_name, ": filter column not created")
      return(NULL)
    }

    # Filter data
    filtered_data <- data_with_filters[data_with_filters[[filter_name]], ]

    # Validate filtered data has required columns and is not empty
    tryCatch({
      validate_cell_data_structure(filtered_data, summary_groups)
      create_summary(filtered_data, summary_groups)
    }, error = function(e) {
      message("Skipping ", filter_name, ": ", e$message)
      return(NULL)
    })
  })

  names(summaries) <- filter_names
  summaries[!purrr::map_lgl(summaries, is.null)]
}
