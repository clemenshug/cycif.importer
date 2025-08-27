#' Resolve overlaps in ROI data by slide
#'
#' Takes ROI data (output from `cycif_load_roi_data()`) and resolves overlapping
#' ROIs by separating them into distinct non-overlapping regions.
#'
#' @param roi_data Data frame with ROI definitions (output from cycif_load_roi_data)
#' @param scale_factor Scale factor for coordinate conversion (default 1.0)
#'
#' @return An sf object with resolved ROI polygons, preserving slideName and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ROI data and resolve overlaps
#' roi_data <- cycif_load_roi_data("/path/to/rois")
#' resolved_rois <- resolve_roi_overlaps(roi_data)
#' }
resolve_roi_overlaps <- function(roi_data, return_format = c("df", "sf")) {
  # Standardize input data to list of dataframes by slide
  roi_by_slide <- standardize_input_data(roi_data)
  return_format <- match.arg(return_format)

  resolved_slides <- purrr::imap(
    roi_by_slide,
    \(x, slide_name) {
      message(sprintf("Processing slide: %s", slide_name))
      slide_sf <- roi_df_to_sf(x, scale_factor = 1)
      resolve_polygon_overlaps(slide_sf)
    }
  )

  if (return_format == "sf") {
    resolved_slides <- dplyr::bind_rows(resolved_slides, .id = "slideName")
  } else if (return_format == "df") {

  }

  resolved_slides
}

#' Resolve overlaps between polygons by splitting them
#'
#' Takes a list of polygons and resolves pairwise overlaps by splitting the
#' overlapping regions. Only handles cases where at most two polygons overlap
#' (no three-way overlaps). The overlaps are resolved iteratively using
#' Voronoi-based splitting lines.
#'
#' @param polygon_sf An sf object containing POLYGON geometries
#' @param max_iterations Maximum number of iterations to prevent infinite loops
#'   (default 100)
#'
#' @return An sf object with overlaps resolved
#' @export
#'
#' @examples
#' \dontrun{
#' # Resolve overlaps in a set of ROI polygons
#' resolved_polygons <- resolve_polygon_overlaps(roi_sf)
#' }
resolve_polygon_overlaps <- function(polygon_sf, max_iterations = 100) {
  # Input validation
  if (!inherits(polygon_sf, "sf")) {
    stop("Input must be an sf object")
  }

  if (nrow(polygon_sf) < 2) {
    message("Less than 2 polygons provided, no overlaps to resolve")
    return(polygon_sf)
  }

  # Ensure all geometries are valid
  polygon_sf <- sf::st_make_valid(polygon_sf)

  # Check that all geometries are polygons
  geom_types <- sf::st_geometry_type(polygon_sf)
  if (!all(geom_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("All geometries must be POLYGON or MULTIPOLYGON")
  }

  message(sprintf(
    "Starting overlap resolution for %d polygons",
    nrow(polygon_sf)
  ))

  # Find all overlapping pairs once
  overlap_pairs <- find_polygon_overlaps(polygon_sf)

  if (length(overlap_pairs) == 0) {
    message("No overlaps found")
    return(polygon_sf)
  }

  message(sprintf("Found %d overlapping pairs to resolve", length(overlap_pairs)))

  # Process each overlapping pair
  for (pair_idx in seq_along(overlap_pairs)) {
    pair_indices <- overlap_pairs[[pair_idx]]

    # Check that each overlap involves exactly two polygons
    if (length(pair_indices) != 2) {
      stop(sprintf(
        "Overlap %d involves %d polygons, but only pairwise overlaps (2 polygons) are supported. Three-way or higher-order overlaps detected.",
        pair_idx, length(pair_indices)
      ))
    }

    i <- pair_indices[1]
    j <- pair_indices[2]

    message(sprintf(
      "Processing pair %d/%d: Resolving overlap between polygons %d and %d",
      pair_idx, length(overlap_pairs), i, j
    ))

    # Resolve this pairwise overlap
    resolved_pair <- resolve_pairwise_overlap(polygon_sf[c(i, j), ])

    # Update the original sf object with resolved polygons
    polygon_sf[i, ]$geometry <- resolved_pair$geometry[1]
    polygon_sf[j, ]$geometry <- resolved_pair$geometry[2]
  }

  message("Overlap resolution complete")

  return(polygon_sf)
}


#' Find all overlapping polygon pairs
#'
#' @param polygon_sf An sf object with polygon geometries
#'
#' @return A list of numeric vectors, each containing indices of overlapping polygon pairs
#' @keywords internal
find_polygon_overlaps <- function(polygon_sf) {
  # Find overlaps using sparse matrix
  overlaps_sparse <- sf::st_overlaps(polygon_sf, sparse = TRUE)

  overlap_pairs <- list()

  # Convert sparse matrix to pairs, avoiding duplicates
  for (i in seq_len(length(overlaps_sparse))) {
    overlapping_indices <- overlaps_sparse[[i]]

    # Only consider indices greater than i to avoid duplicates
    overlapping_indices <- overlapping_indices[overlapping_indices > i]

    for (j in overlapping_indices) {
      overlap_pairs <- append(overlap_pairs, list(c(i, j)))
    }
  }

  return(overlap_pairs)
}


#' Resolve overlap between exactly two polygons
#'
#' @param two_polygon_sf An sf object with exactly 2 polygon geometries
#' @return An sf object with the two polygons after resolving their overlap
#' @keywords internal
resolve_pairwise_overlap <- function(two_polygon_sf) {
  if (nrow(two_polygon_sf) != 2) {
    stop("resolve_pairwise_overlap requires exactly 2 polygons")
  }

  # Find intersection between all polygon pairs
  intersection <- sf::st_intersection(two_polygon_sf)

  if (nrow(intersection) == 0) {
    message("    No intersection found between polygons")
    return(two_polygon_sf)
  }

  # Filter to get the intersection between the two specific polygons
  # Look for intersection with origins indicating both polygons
  target_intersection <- intersection[
    purrr::map_lgl(intersection$origins, \(x) length(x) == 2 && all(x %in% c(1L, 2L))),
  ] %>%
    suppressWarnings(sf::st_cast("POLYGON"))

  if (nrow(target_intersection) == 0) {
    message("    No direct intersection between the two polygons")
    return(two_polygon_sf)
  }


  # Initialize result with original polygons
  result_sf <- two_polygon_sf

  # Iterate through all intersections between the pair
  for (i in seq_len(nrow(target_intersection))) {
    message(sprintf("    Processing intersection %d of %d", i,
                    nrow(target_intersection)))

    # Create splitting line for this specific intersection
    splitting_line <- create_splitting_line(target_intersection[i, ], result_sf)

    if (is.null(splitting_line)) {
      message(sprintf("    Could not create splitting line for intersection %d",
                      i))
      next  # Skip this intersection and continue with the next one
    }

    # Split both polygons and keep the larger pieces
    result_sf[1, ]$geometry <- split_and_keep_largest(result_sf[1, ]$geometry,
                                                       splitting_line)
    result_sf[2, ]$geometry <- split_and_keep_largest(result_sf[2, ]$geometry,
                                                       splitting_line)
  }

  return(result_sf)
}


#' Create a splitting line through overlapping region using Voronoi diagram
#'
#' @param intersection_sf The intersection geometry between two polygons
#' @param two_polygon_sf The two original polygons
#' @return An sf linestring geometry for splitting, or NULL if failed
#' @keywords internal
create_splitting_line <- function(intersection_sf, two_polygon_sf) {
  # Add additional points along polygon boundary for better Voronoi diagram
  intersection_densified <- sf::st_segmentize(intersection_sf, dfMaxLength = 1)

  # Compute touch points only on region of original polygons that are relevant
  # to this intersection
  two_polygons_clipped <- sf::st_intersection(two_polygon_sf, sf::st_buffer(intersection_sf, 1e-6))

  # Find touch points where the two polygon boundaries intersect
  touch_points <- sf::st_intersection(suppressWarnings(sf::st_cast(two_polygons_clipped, "LINESTRING")))
  touch_points <- touch_points[
    purrr::map_lgl(touch_points$origins, \(x) identical(x, c(1L, 2L))),
  ]

  if (nrow(touch_points) == 0) {
    message("      No touch points found")
    return(NULL)
  }

  # Cast to individual points if needed
  touch_pts <- suppressWarnings(sf::st_cast(touch_points, "POINT"))
  if (nrow(touch_pts) < 2) {
    message("      Need at least 2 touch points, found ", nrow(touch_pts))
    return(NULL)
  }

  # Use first two touch points
  start_pt <- touch_pts[1, ]
  end_pt <- touch_pts[2, ]

  # Create Voronoi diagram from intersection points
  voronoi_edges <- sf::st_voronoi(intersection_densified[1, ], bOnlyEdges = TRUE)
  voronoi_lines <- suppressWarnings(sf::st_cast(voronoi_edges, "LINESTRING"))

  # Filter to only interior Voronoi edges
  voronoi_interior <- voronoi_lines[sf::st_within(voronoi_lines, intersection_sf, sparse = FALSE), ]

  if (length(voronoi_interior) == 0) {
    message("      No interior Voronoi edges found")
    return(NULL)
  }

  # Find closest points on Voronoi edges to start and end touch points
  start_nearest_idx <- sf::st_nearest_feature(start_pt, voronoi_interior)
  end_nearest_idx <- sf::st_nearest_feature(end_pt, voronoi_interior)

  closest_to_start <- sf::st_nearest_points(start_pt, voronoi_interior[start_nearest_idx, ])
  closest_to_end <- sf::st_nearest_points(end_pt, voronoi_interior[end_nearest_idx, ])

  # Combine Voronoi edges with connection points for graph creation
  voronoi_with_connections <- dplyr::bind_rows(
    sf::st_sf(geometry = voronoi_interior),
    sf::st_sf(geometry = closest_to_start),
    sf::st_sf(geometry = closest_to_end)
  )

  # Create network for shortest path calculation
  tryCatch({
    voronoi_network <- sfnetworks::as_sfnetwork(
      voronoi_with_connections,
      directed = FALSE,
      length_as_weight = TRUE
    )

    # Find nearest network nodes to start and end points
    start_node_idx <- sf::st_nearest_feature(start_pt, sfnetworks::activate(voronoi_network, "nodes"))
    end_node_idx <- sf::st_nearest_feature(end_pt, voronoi_network)

    # Compute shortest path
    shortest_path_network <- voronoi_network |>
      sfnetworks::activate("edges") |>
      tidygraph::convert(sfnetworks::to_spatial_shortest_paths, from = start_node_idx, to = end_node_idx)

    # Extract path as single linestring
    path_edges <- shortest_path_network |>
      sfnetworks::activate("edges") |>
      sf::st_as_sf() |>
      dplyr::arrange(.tidygraph_edge_index)

    if (nrow(path_edges) == 0) {
      message("      No path found between touch points")
      return(NULL)
    }

    path_geometry <- path_edges$geometry |>
      sf::st_combine() |>
      sf::st_line_merge() |>
      suppressWarnings(sf::st_cast("LINESTRING"))

    # Extend the line slightly to ensure clean splits
    extended_line <- extend_linestring(path_geometry, 1e-6)

    return(sf::st_sfc(extended_line))

  }, error = function(e) {
    message("      Error creating splitting line: ", e$message)
    return(NULL)
  })
}


#' Split a polygon using a linestring and keep the largest piece
#'
#' @param polygon_geom A single polygon geometry
#' @param splitting_line A linestring geometry for splitting
#'
#' @return The largest polygon piece after splitting
#' @keywords internal
split_and_keep_largest <- function(polygon_geom, splitting_line) {
  # Ensure validity before splitting
  polygon_geom <- sf::st_make_valid(polygon_geom)
  split_result <- lwgeom::st_split(polygon_geom, splitting_line)

  # Extract polygon pieces
  pieces <- sf::st_collection_extract(split_result, "POLYGON")

  if (length(pieces) != 2) {
    warning(sprintf("Expected 2 pieces after splitting, found %d. Returning original polygon.", length(pieces)))
    return(polygon_geom)
  }

  # Return the piece with largest area
  pieces[which.max(sf::st_area(pieces))]
}


#' Extend a linestring by a given distance at both ends
#'
#' @param linestring A linestring geometry
#' @param distance Distance to extend at each end
#'
#' @return Extended linestring geometry
#' @keywords internal
extend_linestring <- function(linestring, distance) {
  # Extract coordinate matrix (X,Y)
  coord_matrix <- sf::st_coordinates(linestring)[, c("X", "Y")]

  if (nrow(coord_matrix) < 2) {
    stop("LINESTRING must have at least two vertices")
  }

  # Calculate start segment unit vector (p1→p2)
  start_point <- coord_matrix[1, ]
  second_point <- coord_matrix[2, ]
  start_vector <- second_point - start_point
  start_unit_vector <- start_vector / sqrt(sum(start_vector^2))

  # Calculate end segment unit vector (pn-1→pn)
  penultimate_point <- coord_matrix[nrow(coord_matrix) - 1, ]
  end_point <- coord_matrix[nrow(coord_matrix), ]
  end_vector <- end_point - penultimate_point
  end_unit_vector <- end_vector / sqrt(sum(end_vector^2))

  # Compute new endpoints
  new_start <- start_point - start_unit_vector * distance
  new_end <- end_point + end_unit_vector * distance

  # Assemble extended coordinate list
  extended_coords <- rbind(new_start, coord_matrix, new_end)

  # Return extended linestring geometry
  sf::st_linestring(extended_coords)
}
