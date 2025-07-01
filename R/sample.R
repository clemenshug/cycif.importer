#' Sample cells
#'
#' Samples cells according to specified strategy and size.
#'
#' @param data List of dataframes by slide, single dataframe with
#'   slideName
#' @param sample_size Number of cells to sample per slide (NULL for no sampling)
#' @param sampling_mode Either "all_cells" or "roi_only"
#'
#' @return Single combined dataframe with sampled data
#' @export
#'
#' @examples
#' \dontrun{
#' # Sample 10,000 cells from all cells
#' sampled_data <- cycif_sample(data, sample_size = 10000)
#'
#' # Sample only from ROI cells
#' sampled_data <- cycif_sample(
#'   data,
#'   sample_size = 5000,
#'   sampling_mode = "roi_only"
#' )
#' }
cycif_sample <- function(
  data,
  sample_size = 50000,
  sampling_mode = "all_cells"
) {
  sampling_mode <- match.arg(sampling_mode, c("all_cells", "roi_only"))

  # Standardize input data
  slide_list <- standardize_input_data(data)
  slide_names <- names(slide_list)

  sampled_slides <- list()

  for (slide_name in slide_names) {
    message(sprintf("Sampling slide: %s", slide_name))
    data1 <- slide_list[[slide_name]]

    # Determine sampling pool
    if (sampling_mode == "all_cells") {
      sampling_pool <- data1
    } else { # roi_only
      sampling_pool <- data1 |> filter(ROIname != "none")
      if (nrow(sampling_pool) == 0) {
        message(sprintf("  No ROI cells found for %s", slide_name))
        next
      }
    }

    # Sample cells
    if (is.null(sample_size) || sample_size >= nrow(sampling_pool)) {
      sampled_data <- sampling_pool
      message(sprintf("  Using all %d cells", nrow(sampled_data)))
    } else {
      sampled_data <- sampling_pool[sample(nrow(sampling_pool), sample_size), ]
      message(sprintf("  Sampled %d from %d cells", sample_size, nrow(sampling_pool)))
    }

    # Add slideName column if not present
    if (!"slideName" %in% names(sampled_data)) {
      sampled_data$slideName <- slide_name
    }

    sampled_slides[[slide_name]] <- sampled_data
  }

  # Combine all sampled data
  if (length(sampled_slides) > 0) {
    all_sampled <- bind_rows(sampled_slides)
  } else {
    all_sampled <- NULL
  }

  return(all_sampled)
}
