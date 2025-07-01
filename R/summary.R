#' Summarize sampled data
#'
#' Generates summary statistics from cells using custom subsets
#' and groups.
#'
#' @param data Single dataframe with sampled data, or a list of dataframes
#' @param summary_filters Named list of formula expressions for creating
#'   stratified summaries. Set to NULL to disable.
#'   Example: `list("CD8+" = ~CD8ap, "CD4+" = ~CD4p)`
#' @param summary_groups Named list of character vectors defining different grouping strategies.
#'   Each element represents a different summary grouping.
#'   Default: `list("by_roi" = c("slideName", "ROI", "ROIname"))`
#' @return Flat named list with summary dataframes for each grouping strategy and filter combination
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic summarization with multiple grouping strategies
#' results <- cycif_summarize(
#'   data,
#'   summary_groups = list(
#'     "by_roi" = c("slideName", "ROI", "ROIname"),
#'     "by_slide" = c("slideName")
#'   )
#' )
#'
#' # With stratified summaries
#' results <- cycif_summarize(
#'   data,
#'   summary_filters = list("CD8+" = ~CD8ap, "CD4+" = ~CD4p),
#'   summary_groups = list(
#'     "by_roi" = c("slideName", "ROI", "ROIname"),
#'     "by_slide" = c("slideName")
#'   )
#' )
#' }
cycif_summarize <- function(
  data,
  summary_filters = NULL,
  summary_groups = list("by_roi" = c("slideName", "ROI", "ROIname"))
) {
  # Memory-efficient approach: work with standardized list and only combine when needed
  data <- standardize_input_data(data)

  if (any(purrr::map_lgl(names(summary_filters), is.null))) {
    stop("All summary_filters must be named formula expressions")
  }
  if (any(purrr::map_lgl(names(summary_groups), is.null))) {
    stop("All summary_groups must be named character vectors")
  }

  # Early validation on first slide to catch column issues
  all_summary_vars <- unique(unlist(summary_groups))
  purrr::walk(
    data,
    \(x) validate_cell_data_structure(x, all_summary_vars)
  )

  # Pre-compute filter columns once if needed (memory efficient)
  data_with_filters <- NULL
  if (!is.null(summary_filters)) {
    data_with_filters <- cycif_marker_combinations(data, summary_filters)
  }

  results <- list()
  for (group_name in names(summary_groups)) {
    for (filter_name in names(summary_filters)) {
      if (filter_name %in% attr(data_with_filters, "missing_combinations")) {
        warning(sprintf("Skipping group '%s' with filter '%s': missing columns (%s)", group_name, filter_name, paste(attr(data_with_filters, "missing_vars"), collapse = ", ")))
        next
      }
      group_vars <- summary_groups[[group_name]]
      filter_name_sym <- rlang::ensym(filter_name)

      res_slide <- data_with_filters |>
        purrr::map(\(x) filter(x, !!filter_name_sym)) |>
        bind_rows(.id = "slideName") |>
        create_summary(group_vars = group_vars)

      # Store in results
      results[[paste(group_name, filter_name, sep = "__")]] <- res_slide
    }
  }

  return(results)
}

#' Create summary statistics by grouping variables
#'
#' @param data Data frame with cell measurements
#' @param group_vars Character vector of column names to group by
#' @return Data frame with summary statistics
#' @keywords internal
create_summary <- function(data, group_vars) {
  group_syms <- rlang::syms(group_vars)

  data |>
    group_by(!!!group_syms) |>
    summarize(
      cell_count = n(),
      across(
        where(function(x) is.numeric(x) || is.logical(x)),
        .fns = list(mean = \(x) mean(x, na.rm = TRUE)),
        .names = "{.fn}_{.col}"
      ),
      .groups = "drop"
    )
}
