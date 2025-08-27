library(cycif.importer)

# Test data: 4 polygons with complex overlaps
create_test_roi_data <- function() {
  csv_data <- paste(
    'Id,Name,Text,type,all_points,X,Y,RadiusX,RadiusY,Width,Height,all_transforms',
    '1,Polygon1,Polygon1,Polygon,"102.86,41.71 26.39,115.83 0.51,312.3 38.15,467.59 211.1,494.65 160.51,365.24 198.15,258.18 325.21,238.18 340.51,153.47 206.39,173.47 230.2,86.41 340.51,88.77 306.39,29.94 238.15,0.53 102.86,41.71",-1,-1,-1,-1,-1,-1,-1',
    '2,Polygon2,Polygon2,Polygon,"225.21,422.88 176.98,440.53 141.68,400.53 118.15,360.53 161.68,322.88 155.8,339.36 189.92,342.88 225.21,422.88",-1,-1,-1,-1,-1,-1,-1',
    '3,Polygon3,Polygon3,Polygon,"265.21,286.41 366.39,247.59 355.8,153.47 340.51,132.3 287.57,132.3 258.15,174.65 306.39,179.36 302.86,209.36 264.04,227.59 247.57,267.59 265.21,286.41",-1,-1,-1,-1,-1,-1,-1',
    '4,Polygon4,Polygon4,Polygon,"272.27,433.47 225.21,437 205.21,409.94 230.2,374.65 230.2,347.59 189.92,361.71 180.51,347.59 200.51,312.3 255.8,329.94 295.8,399.36 272.27,433.47",-1,-1,-1,-1,-1,-1,-1',
    sep = "\n"
  )
  
  roi_data <- read.csv(text = csv_data, stringsAsFactors = FALSE)
  roi_data$slideName <- "test_slide"
  return(roi_data)
}

test_that("resolve_roi_overlaps resolves overlaps in sf format", {
  # Create test data
  roi_data <- create_test_roi_data()
  
  # Convert to sf to check initial overlaps
  roi_sf <- roi_df_to_sf(roi_data)
  initial_overlaps <- sf::st_overlaps(roi_sf, sparse = FALSE)
  expect_true(sum(initial_overlaps, na.rm = TRUE) > 0, "Test data should have overlaps")
  
  # Resolve overlaps
  resolved <- resolve_roi_overlaps(roi_data, return_format = "sf")
  
  # Check result structure
  expect_s3_class(resolved, "sf")
  expect_named(resolved, c("Id", "Name", "Text", "type", "all_points", "X", "Y", 
                          "RadiusX", "RadiusY", "Width", "Height", "all_transforms", 
                          "slideName", "geometry"))
  expect_equal(nrow(resolved), 4)
  expect_equal(resolved$slideName, rep("test_slide", 4))
  
  # Check that overlaps are resolved (check for actual overlapping pairs)
  n_polygons <- nrow(resolved)
  overlap_count <- 0
  for (i in 1:(n_polygons-1)) {
    for (j in (i+1):n_polygons) {
      intersection <- sf::st_intersection(resolved[i,], resolved[j,])
      if (nrow(intersection) > 0) {
        intersection_area <- sf::st_area(intersection)
        if (any(intersection_area > 1e-6)) {
          overlap_count <- overlap_count + 1
        }
      }
    }
  }
  expect_equal(overlap_count, 0, "All meaningful overlaps should be resolved")
})

test_that("resolve_roi_overlaps resolves overlaps in df format", {
  # Create test data
  roi_data <- create_test_roi_data()
  
  # Resolve overlaps
  resolved <- resolve_roi_overlaps(roi_data, return_format = "df")
  
  # Check result structure
  expect_s3_class(resolved, "data.frame")
  expect_named(resolved, c("Id", "Name", "Text", "type", "all_points", "X", "Y", 
                          "RadiusX", "RadiusY", "Width", "Height", "all_transforms", 
                          "slideName"))
  expect_equal(nrow(resolved), 4)
  expect_equal(resolved$slideName, rep("test_slide", 4))
  
  # Check coordinate strings format (allow negative numbers and decimal points)
  expect_true(all(grepl("^[0-9.,-]+( [0-9.,-]+)*$", resolved$all_points)), 
              "all_points should contain valid coordinate strings")
  
  # Check that coordinate strings contain comma-separated pairs
  expect_true(all(grepl(",", resolved$all_points)),
              "Coordinate strings should contain commas")
  
  # Verify we can convert back to sf (round-trip test)
  resolved_sf <- roi_df_to_sf(resolved)
  expect_s3_class(resolved_sf, "sf")
  expect_equal(nrow(resolved_sf), 4)
})

test_that("resolve_roi_overlaps handles single slide data", {
  # Create test data with explicit slideName
  roi_data <- create_test_roi_data()
  roi_data$slideName <- "single_slide"
  
  # Resolve overlaps
  resolved <- resolve_roi_overlaps(roi_data, return_format = "sf")
  
  # Check slide preservation
  expect_equal(unique(resolved$slideName), "single_slide")
  expect_equal(nrow(resolved), 4)
})

test_that("resolve_roi_overlaps handles no overlaps case", {
  # Create simple non-overlapping rectangles
  no_overlap_data <- data.frame(
    Id = 1:2,
    Name = c("Rect1", "Rect2"),
    Text = c("Rect1", "Rect2"),
    type = c("Polygon", "Polygon"),
    all_points = c("0,0 10,0 10,10 0,10 0,0", 
                   "20,20 30,20 30,30 20,30 20,20"),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = c("test_slide", "test_slide"),
    stringsAsFactors = FALSE
  )
  
  # Resolve "overlaps" (should be unchanged)
  resolved <- resolve_roi_overlaps(no_overlap_data, return_format = "sf")
  
  # Check no overlaps before and after
  resolved_overlaps <- sf::st_overlaps(resolved, sparse = FALSE)
  expect_equal(sum(resolved_overlaps, na.rm = TRUE), 0)
  expect_equal(nrow(resolved), 2)
})

test_that("sf_to_roi_df converts sf back to dataframe correctly", {
  # Create test sf data
  roi_data <- create_test_roi_data()
  roi_sf <- roi_df_to_sf(roi_data)
  
  # Convert back to dataframe
  converted_df <- sf_to_roi_df(roi_sf)
  
  # Check structure
  expect_s3_class(converted_df, "data.frame")
  expect_true("all_points" %in% names(converted_df))
  expect_equal(nrow(converted_df), 4)
  
  # Check coordinate string format (allow negative numbers)
  expect_true(all(grepl("^[0-9.,-]+( [0-9.,-]+)*$", converted_df$all_points)),
              "Coordinate strings should be numeric with commas and spaces")
  
  # Check that polygons are closed (first and last points identical)
  for (i in seq_len(nrow(converted_df))) {
    coords <- strsplit(converted_df$all_points[i], " ")[[1]]
    first_coord <- coords[1]
    last_coord <- coords[length(coords)]
    expect_equal(first_coord, last_coord, 
                 info = paste("Polygon", i, "should be closed"))
  }
  
  # Round-trip test: convert back to sf and check validity
  round_trip_sf <- roi_df_to_sf(converted_df)
  expect_s3_class(round_trip_sf, "sf")
  expect_equal(nrow(round_trip_sf), 4)
})

test_that("resolve_roi_overlaps handles multiple slides", {
  # Create test data with multiple slides
  roi_data1 <- create_test_roi_data()
  roi_data1$slideName <- "slide1"
  roi_data1$Id <- 1:4
  
  roi_data2 <- create_test_roi_data()
  roi_data2$slideName <- "slide2"  
  roi_data2$Id <- 5:8
  
  combined_data <- rbind(roi_data1, roi_data2)
  
  # Resolve overlaps
  resolved <- resolve_roi_overlaps(combined_data, return_format = "sf")
  
  # Check that both slides are present
  expect_setequal(unique(resolved$slideName), c("slide1", "slide2"))
  expect_equal(nrow(resolved), 8)
  
  # Check no meaningful overlaps within each slide
  slide1_data <- resolved[resolved$slideName == "slide1", ]
  slide2_data <- resolved[resolved$slideName == "slide2", ]
  
  # Check slide1 overlaps
  slide1_overlap_count <- 0
  n1 <- nrow(slide1_data)
  for (i in 1:(n1-1)) {
    for (j in (i+1):n1) {
      intersection <- sf::st_intersection(slide1_data[i,], slide1_data[j,])
      if (nrow(intersection) > 0) {
        intersection_area <- sf::st_area(intersection)
        if (any(intersection_area > 1e-6)) {
          slide1_overlap_count <- slide1_overlap_count + 1
        }
      }
    }
  }
  
  # Check slide2 overlaps  
  slide2_overlap_count <- 0
  n2 <- nrow(slide2_data)
  for (i in 1:(n2-1)) {
    for (j in (i+1):n2) {
      intersection <- sf::st_intersection(slide2_data[i,], slide2_data[j,])
      if (nrow(intersection) > 0) {
        intersection_area <- sf::st_area(intersection)
        if (any(intersection_area > 1e-6)) {
          slide2_overlap_count <- slide2_overlap_count + 1
        }
      }
    }
  }
  
  expect_equal(slide1_overlap_count, 0)
  expect_equal(slide2_overlap_count, 0)
})