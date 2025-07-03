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
    data1 <- assign_rois_to_cells(data1, rois_current_slide, scale_factor)

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

REQUIRED_ROI_VALS <- c("all_points", "Name", "type")

#' Assign ROIs to cells based on polygon coordinates
#'
#' @param data Data frame with cell coordinates
#' @param roi_data Data frame with ROI polygon definitions
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

  # Process each ROI
  for (i in seq_len(nrow(roi_data))) {
    r <- roi_data[i,]

    # Check required columns
    missing_vals <- purrr::map_lgl(
      REQUIRED_ROI_VALS,
      \(x) is.null(r[[x]]) || is.na(r[[x]]) || r[[x]] == ""
    )
    if (sum(missing_vals) > 0) {
      warning(
        sprintf(
          "  Skipping ROI %d (%s): missing required values: %s",
          i, r$Name, paste(REQUIRED_ROI_VALS[missing_vals], collapse = ", ")
        )
      )
      next
    }

    # Parse and assign ROI
    cells_assigned <- assign_single_roi(data, r$all_points, r$type, r$Name, i, scale_factor)
    if (is.null(cells_assigned)) {
      # Warning already issued in assign_single_roi
      next
    }
    data$ROI[cells_assigned] <- i
    data$ROIname[cells_assigned] <- r$Name
    message(sprintf("    ROI %d (%s): %d cells assigned", i, r$Name, sum(cells_assigned)))
  }

  return(data)
}

#' Parse ROI points from string format
#'
#' @param roi_points_str String with ROI points in the format "x1,y1 x2,y2 ..."
#'
#' @return Matrix with parsed points (x, y)
#' @keywords internal
parse_point_str <- function(roi_points_str) {
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
  points_matrix[complete.cases(points_matrix), , drop = FALSE]
}

#' Assign a single ROI to cells
#'
#' @param data Data frame with cell coordinates
#' @param roi_points_str String with polygon points "x1,y1 x2,y2 x3,y3 ..."
#' @param roi_type Type of ROI ("Polyline", "Polygon", "Rectangle")
#' @param roi_name Name of the ROI
#' @param roi_id Numeric ID of the ROI
#' @param scale_factor Scale factor for coordinate conversion
#'
#' @return Logical vector indicating which cells are in the ROI
#' @keywords internal
assign_single_roi <- function(data, roi_points_str, roi_type, roi_name, roi_id, scale_factor) {
  # Remove any rows with NA values
  points_matrix <- parse_point_str(roi_points_str)

  if (roi_type %in% c("Polyline", "Polygon") && nrow(points_matrix) < 3) {
    warning(sprintf("  ROI %d (%s): not enough points to form a polygon. Skipping ROI.", roi_id, roi_name))
    return(NULL)
  }
  if (roi_type == "Rectangle" && nrow(points_matrix) != 4) {
    warning(sprintf("  ROI %d (%s): Rectangle doesn't consist of 4 points. Skipping ROI.", roi_id, roi_name))
    return(NULL)
  }
  if (!roi_type %in% c("Polygon", "Polyline", "Rectangle")) {
    warning(sprintf("  ROI %d (%s): Unsupported type '%s'. Skipping ROI.", roi_id, roi_name, roi_type))
    return(NULL)
  }

  # Apply scale factor (convert from pixels to microns)
  points_matrix <- points_matrix * scale_factor

  # Test which cells are inside this ROI
  inside_flags <- sp::point.in.polygon(
    point.x = data$Xt,
    point.y = data$Yt,
    pol.x = points_matrix[, 1],
    pol.y = points_matrix[, 2]
  )

  # Assign ROI ID and name to cells inside or on boundary (flags >= 1)
  cells_in_roi <- inside_flags >= 1

  return(cells_in_roi)
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
