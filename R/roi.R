#' Import UNMICST data and assign ROIs
#'
#' Imports raw cell data, standardizes coordinates, and assigns ROIs to cells.
#' Returns processed data as list of dataframes by slide.
#'
#' @param cell_data Data frame with cell measurements, single dataframe with
#'   slideName, or list of dataframes by slide
#' @param roi_data Data frame with ROI polygon definitions (optional)
#' @param scale_factor Scale factor for converting pixel coordinates to microns
#'   (default 0.65)
#' @param expand_distance Distance in microns to expand ROIs (default 50, set
#'   to 0 to disable)
#'
#' @return List of dataframes by slide name with ROI assignments
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with cell data only
#' processed_data <- cycif_import_and_assign_rois(cell_data)
#'
#' # With ROI assignment
#' processed_data <- cycif_import_and_assign_rois(
#'   cell_data = my_cells,
#'   roi_data = my_rois
#' )
#' }
cycif_assign_rois <- function(
  cell_data,
  roi_data = NULL,
  scale_factor = 0.65,
  expand_distance = 50
) {
  # Standardize input data
  slide_list <- standardize_input_data(cell_data)
  roi_data <- standardize_input_data(roi_data)

  slide_names <- names(slide_list)
  message(sprintf("Processing %d slides: %s", length(slide_names), paste(slide_names, collapse = ", ")))

  # Process each slide
  processed_slides <- list()

  for (slide_name in slide_names) {
    message(sprintf("Processing slide: %s", slide_name))
    data1 <- slide_list[[slide_name]]

    # Standardize coordinate column names
    data1 <- standardize_coordinates(data1, scale_factor)

    # Assign ROIs if polygon data provided
    rois_current_slide <- roi_data[[slide_name]]
    rois_sf <- roi_df_to_sf(rois_current_slide, scale_factor)
    data1 <- assign_rois_to_cells(data1, rois_sf, scale_factor)

    # Expand ROIs if requested
    if (expand_distance > 0) {
      data1 <- expand_roi_boundaries(data1, expand_distance)
    }

    processed_slides[[slide_name]] <- data1
  }

  return(processed_slides)
}

#' Standardize coordinate columns and convert to microns
#'
#' @param data1 Data frame with cell measurements
#' @param scale_factor Scale factor for converting pixels to microns
#'
#' @return Data frame with standardized coordinate columns
#' @keywords internal
standardize_coordinates <- function(data, scale_factor) {
  # Rename coordinate columns to match MATLAB naming
  names(data) <- stringr::str_replace(names(data), "X_centroid", "Xt")
  names(data) <- stringr::str_replace(names(data), "Y_centroid", "Yt")

  # Convert coordinates from pixels to microns
  data <- data |>
    mutate(
      across(
        any_of(c("Xt", "Yt")),
        \(x) x * scale_factor
      )
    )

  return(data)
}

#' Convert ROI points string to polygon matrix
#'
#' @param roi_points_str String with polygon points "x1,y1 x2,y2 x3,y3 ..."
#' @param roi_type Type of ROI ("Polyline", "Polygon", "Rectangle")
#' @param roi_name Name of the ROI
#' @param roi_id Numeric ID of the ROI
#' @param scale_factor Scale factor for coordinate conversion
#'
#' @return Matrix with parsed points (x, y)
#' @keywords internal
parse_point_str <- function(roi_points_str, roi_type, roi_name, roi_id, scale_factor) {
  point_pairs <- stringr::str_split_1(roi_points_str, " ")
  points_matrix <- matrix(NA, nrow = length(point_pairs), ncol = 2)

  for (i in seq_along(point_pairs)) {
    coords <- as.numeric(stringr::str_split_1(point_pairs[i], ","))
    if (length(coords) == 2) {
      points_matrix[i, ] <- coords
    } else {
      warning(sprintf("Invalid point format '%s'. Using ROI without this point.", point_pairs[i]))
    }
  }

  # Remove any rows with NA values
  points_matrix <- points_matrix[complete.cases(points_matrix), , drop = FALSE]

  if (nrow(points_matrix) < 3) {
    warning(sprintf("  ROI %d (%s): not enough points to form a polygon. Skipping ROI.", roi_id, roi_name))
    return(NULL)
  }

  if (roi_type %in% c("Polyline", "Polygon")) {
    if (nrow(points_matrix) < 3) {
      # For Polygon and Polyline, we need at least 3 points
      warning(sprintf("  ROI %d (%s): not enough points to form a polygon. Skipping ROI.", roi_id, roi_name))
      return(NULL)
    }
    # Ensure polygon is closed (first and last points are the same)
    if (!identical(points_matrix[1, ], points_matrix[nrow(points_matrix), ])) {
      points_matrix <- rbind(points_matrix, points_matrix[1, ])
    }
  }
  if (roi_type == "Rectangle" && nrow(points_matrix) != 4) {
    warning(sprintf("  ROI %d (%s): Rectangle doesn't consist of 4 points. Skipping ROI.", roi_id, roi_name))
    return(NULL)
  }
  if (!roi_type %in% c("Polygon", "Polyline", "Rectangle")) {
    warning(sprintf("  ROI %d (%s): Unsupported type '%s'. Skipping ROI.", roi_id, roi_name, roi_type))
    return(NULL)
  }

  points_matrix <- points_matrix * scale_factor

  points_matrix
}

#' Assign ROIs to cells based on polygon coordinates
#'
#' @param data Data frame with cell coordinates
#' @param roi_data Sf geometry data frame with ROI polygons
#' @param scale_factor Scale factor for coordinate conversion
#'
#' @return Data frame with ROI assignments added
#' @keywords internal
assign_rois_to_cells <- function(data, roi_data, scale_factor) {
  # Initialize ROI columns
  data$ROI <- 0
  data$ROIname <- "none"

  if (is.null(roi_data) || nrow(roi_data) == 0) {
    return(data)
  }

  message(sprintf("  Processing %d ROIs", nrow(roi_data)))

  # Convert cell coordinates to sf points
  cell_points <- data |>
    filter(!is.na(Xt) & !is.na(Yt)) |>
    sf::st_as_sf(coords = c("Xt", "Yt"))

  if (nrow(cell_points) == 0) {
    warning("No valid cell coordinates found")
    return(data)
  }

  # Set same CRS for both datasets
  sf::st_crs(cell_points) <- sf::st_crs(roi_data)

  # Use spatial intersection to find which cells are in which ROIs
  intersections <- sf::st_within(cell_points, roi_data)

  n_multiple_roi <- sum(purrr::map_int(intersections, length) > 1)
  if (n_multiple_roi > 0) {
    warning(
      sprintf(
        "Found %d cells in multiple ROIs. Assigning first ROI only.",
        n_multiple_roi
      )
    )
  }
  roi_ids <- purrr::map_int(intersections, \(x) ifelse(length(x) > 0, x[1], 0))
  data$ROI <- roi_ids
  data$ROIname <- ifelse(roi_ids > 0, roi_data$Name[roi_ids], "none")

  # Report assignment results
  assigned_count <- sum(data$ROI != 0)
  message(sprintf("  Assigned %d cells to ROIs", assigned_count))

  data
}

#' Expand ROI boundaries to include nearby cells
#'
#' @param data1 Data frame with ROI assignments
#' @param expand_distance Distance in microns to expand ROIs
#'
#' @return Data frame with expanded ROI assignments
#' @keywords internal
expand_roi_boundaries <- function(data1, expand_distance) {
  roi_cells <- data1 |> filter(ROI != 0)

  if (nrow(roi_cells) == 0 || expand_distance <= 0) {
    return(data1)
  }

  # Get unique ROI information
  unique_rois <- data1 |>
    filter(ROI != 0) |>
    distinct(ROI, ROIname)

  # Store original assignments
  data1$ROI_original <- data1$ROI
  data1$ROIname_original <- data1$ROIname

  # Process each ROI for expansion
  for (i in seq_len(nrow(unique_rois))) {
    current_roi <- unique_rois$ROI[i]
    current_roi_name <- unique_rois$ROIname[i]

    message(sprintf("    Expanding ROI %d (%s) by %d microns", current_roi, current_roi_name, expand_distance))

    # Get cells in current ROI
    roi_coords <- data1 |>
      filter(ROI == current_roi) |>
      select(Xt, Yt, CellID)

    # Get cells not in any ROI
    unassigned_coords <- data1 |>
      filter(ROI == 0) |>
      select(Xt, Yt, CellID)

    if (nrow(roi_coords) == 0 || nrow(unassigned_coords) == 0) {
      next
    }

    # Find nearest neighbors
    nn_result <- FNN::get.knnx(
      data = roi_coords[, c("Xt", "Yt")],
      query = unassigned_coords[, c("Xt", "Yt")],
      k = min(20, nrow(roi_coords))
    )

    # Find cells within expand_distance
    close_cells_mask <- apply(nn_result$nn.dist, 1, function(row) any(row < expand_distance))

    if (sum(close_cells_mask) > 0) {
      close_cell_ids <- unassigned_coords$CellID[close_cells_mask]
      data1$ROI[data1$CellID %in% close_cell_ids] <- current_roi
      data1$ROIname[data1$CellID %in% close_cell_ids] <- current_roi_name

      message(sprintf("      Assigned %d additional cells", sum(close_cells_mask)))
    }
  }

  return(data1)
}

#' Convert ROI dataframe to sf object
#'
#' Converts a ROI dataframe with polygon coordinates to an sf object
#' with proper geometry column. Uses the existing parse_point_str function
#' to process coordinate strings.
#'
#' @param roi_data Data frame with ROI definitions containing at minimum:
#'   - Name: ROI name
#'   - type: ROI type (e.g., "Polygon", "Rectangle")
#'   - all_points: String with coordinates "x1,y1 x2,y2 x3,y3 ..."
#' @param scale_factor Scale factor for coordinate conversion (default 1.0)
#'
#' @return sf object with geometry column and ROI metadata, or NULL if no valid ROIs
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert ROI data to sf
#' roi_sf <- roi_df_to_sf(roi_data, scale_factor = 0.65)
#' }
roi_df_to_sf <- function(roi_data, scale_factor = 1.0) {
  if (is.null(roi_data) || nrow(roi_data) == 0) {
    return(NULL)
  }

  # Check required columns

  validate_cell_data_structure(
    roi_data, c("Name", "type", "all_points"), no_missing = TRUE
  )

  point_matrices <- purrr::pmap(
    roi_data,
    \(roi_name, type, all_points, roi_id, ...) {
      parse_point_str(all_points, type, roi_name, roi_id, scale_factor)
    }
  )

  polygons <- purrr::map(point_matrices, \(x) sf::st_polygon(list(x)))
  # browser()

  roi_sf <- sf::st_sf(
    roi_data,
    geometry = sf::st_sfc(polygons)
  )

  valid_polygons <- sf::st_is_valid(roi_sf, reason = TRUE)
  if (!all(valid_polygons == "Valid Geometry")) {
    invalid_idx <- which(valid_polygons != "Valid Geometry")
    warning(
      sprintf(
        "Found %d invalid ROI geometries (%s)\nMaking them valid.",
        length(invalid_idx),
        paste0(paste0("ROI ", invalid_idx, " [", valid_polygons[invalid_idx], "]"), collapse = ", ")
      )
    )
    roi_sf <- sf::st_make_valid(roi_sf)
  }

  message(sprintf("Created sf object with %d ROI geometries", nrow(roi_sf)))

  return(roi_sf)
}

#' Plot ROI sf polygons with ggplot2
#'
#' Creates a visualization of ROI polygons using ggplot2's geom_sf().
#' Each ROI gets a different color and is labeled with its ID at the centroid.
#' Separate facets are created for each slide.
#'
#' @param roi_sf sf object containing ROI polygons, typically output from
#'   roi_df_to_sf(). Must contain a 'slideName' column for faceting
#' @param label_column Character string specifying which column to use for
#'   labels. Default is "Name" but could be "Id" or other identifier columns
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ROI data and convert to sf
#' extdata_path <- system.file("extdata", package = "cycif.importer")
#' roi_dir <- file.path(extdata_path, "example_rois")
#' roi_data <- cycif_load_roi_data(roi_dir, c("LSP11060", "LSP11064"))
#' roi_sf <- roi_df_to_sf(roi_data)
#'
#' # Plot the ROIs
#' plot_roi_sf(roi_sf)
#' }
plot_roi_sf <- function(roi_sf, label_column = "Name") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(paste0("Package 'ggplot2' is required for plotting. ",
                "Please install it with: install.packages('ggplot2')"))
  }

  if (!inherits(roi_sf, "sf")) {
    stop(paste0("roi_sf must be an sf object. Use roi_df_to_sf() to convert ",
                "ROI data to sf format."))
  }

  if (!"slideName" %in% names(roi_sf)) {
    stop(paste0("roi_sf must contain a 'slideName' column for faceting. ",
                "Add this column before plotting."))
  }

  if (!label_column %in% names(roi_sf)) {
    stop(sprintf("Label column '%s' not found in roi_sf. Available columns: %s",
                 label_column, paste(names(roi_sf), collapse = ", ")))
  }

  # Create the plot
  p <- ggplot2::ggplot(roi_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = factor(!!rlang::sym(label_column))),
                     alpha = 0.7,
                     color = "black",
                     size = 0.5) +
    ggplot2::geom_sf_label(ggplot2::aes(label = !!rlang::sym(label_column)),
                           size = 3,
                           alpha = 0.8) +
    ggplot2::facet_wrap(~slideName) +
    ggplot2::scale_fill_viridis_d(name = "ROI") +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    ) +
    ggplot2::labs(
      title = "ROI Polygons by Slide",
      subtitle = sprintf("ROIs labeled by %s", label_column),
      x = NULL,
      y = NULL
    )

  return(p)
}
