library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("cycif_assign_rois assigns ROI ids", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 50)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  expect_type(assigned, "list")
  expect_true(all(purrr::map_lgl(assigned, ~"ROI" %in% names(.x))))
  expect_true(any(assigned[[1]]$ROI > 0))
})

test_that("cycif_assign_rois works without ROI data", {
  data_dir <- file.path(extdata, "example_quants")
  cells <- cycif_load_cell_data(data_dir)
  cells <- list(LSP11060 = head(cells$LSP11060, 10))
  res <- cycif_assign_rois(cells, NULL)
  expect_true(all(res$LSP11060$ROI == 0))
  expect_true(all(res$LSP11060$ROIname == "none"))
})

test_that("cycif_assign_rois uses roi_priority for overlapping ROIs", {
  # Create test cell data with cells at specific coordinates
  test_cells <- data.frame(
    CellID = 1:4,
    X_centroid = c(5, 8, 25, 35),  # Will be converted to Xt (* 0.65)
    Y_centroid = c(5, 8, 25, 35),  # Will be converted to Yt (* 0.65)  
    slideName = "test_slide"
  )
  
  # Create overlapping ROI data (coordinates will be * scale_factor = 0.65)
  # HighPriority: (0,0) to (10,10) -> (0,0) to (6.5,6.5) - should contain cell 1 at (3.25,3.25)
  # LowPriority: (0,0) to (15,15) -> (0,0) to (9.75,9.75) - should contain cells 1,2 
  # MediumPriority: (5,5) to (20,20) -> (3.25,3.25) to (13,13) - should contain cells 1,2
  test_rois <- data.frame(
    Id = 1:3,
    Name = c("HighPriority", "LowPriority", "MediumPriority"),
    Text = c("HighPriority", "LowPriority", "MediumPriority"),
    type = c("Polygon", "Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",
      "0,0 15,0 15,15 0,15 0,0", 
      "5,5 20,5 20,20 5,20 5,5"
    ),
    X = c(-1, -1, -1),
    Y = c(-1, -1, -1),
    RadiusX = c(-1, -1, -1),
    RadiusY = c(-1, -1, -1),
    Width = c(-1, -1, -1),
    Height = c(-1, -1, -1),
    all_transforms = c(-1, -1, -1),
    slideName = "test_slide"
  )
  
  # Test with priority: HighPriority > MediumPriority > LowPriority
  priority <- c("HighPriority", "MediumPriority", "LowPriority")
  
  assigned <- cycif_assign_rois(
    cell_data = test_cells, 
    roi_data = test_rois, 
    expand_distance = 0,
    roi_priority = priority
  )
  
  result <- assigned$test_slide
  
  # Cell 1 at (5,5) -> (3.25,3.25) is in all three ROIs
  # Should be assigned to HighPriority (highest priority)
  cell1_roi <- result$ROIname[result$CellID == 1]
  expect_equal(cell1_roi, "HighPriority")
  
  # Cell 2 at (8,8) -> (5.2,5.2) is in all three ROIs
  # Should be assigned to HighPriority (highest priority)
  cell2_roi <- result$ROIname[result$CellID == 2]
  expect_equal(cell2_roi, "HighPriority")
  
  # Cell 3 at (25,25) -> (16.25,16.25) is not in any ROI
  cell3_roi <- result$ROIname[result$CellID == 3]
  expect_equal(cell3_roi, "none")
  
  # Cell 4 at (35,35) -> (22.75,22.75) is not in any ROI
  cell4_roi <- result$ROIname[result$CellID == 4]
  expect_equal(cell4_roi, "none")
})

test_that("cycif_assign_rois priority system works with different overlap patterns", {
  # Create test with more specific overlap patterns
  test_cells <- data.frame(
    CellID = 1:3,
    X_centroid = c(12, 18, 7),  # Scaled: (7.8, 11.7, 4.55)
    Y_centroid = c(12, 7, 18),  # Scaled: (7.8, 4.55, 11.7)
    slideName = "test_slide"
  )
  
  # Create three non-nested overlapping ROIs
  test_rois <- data.frame(
    Id = 1:3,
    Name = c("HighPriority", "MediumPriority", "LowPriority"),
    Text = c("HighPriority", "MediumPriority", "LowPriority"),
    type = c("Polygon", "Polygon", "Polygon"),
    all_points = c(
      "10,10 15,10 15,15 10,15 10,10",  # (6.5,6.5) to (9.75,9.75) - should contain cell 1
      "15,5 25,5 25,15 15,15 15,5",     # (9.75,3.25) to (16.25,9.75) - should contain cells 1,2  
      "5,15 15,15 15,25 5,25 5,15"      # (3.25,9.75) to (9.75,16.25) - should contain cells 1,3
    ),
    X = c(-1, -1, -1),
    Y = c(-1, -1, -1),
    RadiusX = c(-1, -1, -1),
    RadiusY = c(-1, -1, -1),
    Width = c(-1, -1, -1),
    Height = c(-1, -1, -1),
    all_transforms = c(-1, -1, -1),
    slideName = "test_slide"
  )
  
  priority <- c("HighPriority", "MediumPriority", "LowPriority")
  
  assigned <- cycif_assign_rois(
    cell_data = test_cells, 
    roi_data = test_rois, 
    expand_distance = 0,
    roi_priority = priority
  )
  
  result <- assigned$test_slide
  
  # Cell 1 at (12,12) -> (7.8,7.8) is in all three ROIs - should get HighPriority
  cell1_roi <- result$ROIname[result$CellID == 1]
  expect_equal(cell1_roi, "HighPriority")
  
  # Cell 2 at (18,7) -> (11.7,4.55) is only in MediumPriority - should get MediumPriority
  cell2_roi <- result$ROIname[result$CellID == 2]
  expect_equal(cell2_roi, "MediumPriority")
  
  # Cell 3 at (7,18) -> (4.55,11.7) is only in LowPriority - should get LowPriority
  cell3_roi <- result$ROIname[result$CellID == 3]
  expect_equal(cell3_roi, "LowPriority")
})

test_that("cycif_assign_rois falls back to first ROI when roi_priority is NULL", {
  # Create test cell data
  test_cells <- data.frame(
    CellID = 1,
    X_centroid = 5,
    Y_centroid = 5,
    slideName = "test_slide"
  )
  
  # Create overlapping ROI data (same point in multiple ROIs)
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("ROI1", "ROI2"),
    Text = c("ROI1", "ROI2"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",
      "0,0 15,0 15,15 0,15 0,0"
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  # Test without roi_priority (should assign first ROI)
  assigned <- cycif_assign_rois(
    cell_data = test_cells, 
    roi_data = test_rois, 
    expand_distance = 0
  )
  
  result <- assigned$test_slide
  
  # Cell should be assigned to first ROI in the intersection list
  # (behavior depends on sf::st_within ordering)
  expect_true(result$ROIname[1] %in% c("ROI1", "ROI2"))
  expect_true(result$ROI[1] > 0)
})

test_that("analyze_roi_overlaps detects no overlaps correctly", {
  # Create non-overlapping ROI data
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("ROI1", "ROI2"),
    Text = c("ROI1", "ROI2"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",      # ROI1: (0,0) to (10,10)
      "20,20 30,20 30,30 20,30 20,20" # ROI2: (20,20) to (30,30) - no overlap
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  analysis <- analyze_roi_overlaps(test_rois, message_level = "error")
  
  expect_equal(nrow(analysis$summary), 1)
  expect_equal(analysis$summary$overlapping_pairs, 0)
  expect_equal(analysis$summary$small_overlaps, 0)
  expect_equal(analysis$summary$large_overlaps, 0)
  expect_equal(analysis$summary$multiway_overlaps, 0)
  expect_equal(analysis$summary$containment_cases, 0)
  expect_equal(analysis$summary$recommendation, "No overlaps detected - proceed with standard ROI assignment")
  
  expect_equal(nrow(analysis$details), 0)
})

test_that("analyze_roi_overlaps detects small overlaps correctly", {
  # Create ROI data with small overlap
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("ROI1", "ROI2"),
    Text = c("ROI1", "ROI2"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",     # ROI1: (0,0) to (10,10)
      "8,8 15,8 15,15 8,15 8,8"      # ROI2: (8,8) to (15,15) - small overlap
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  analysis <- analyze_roi_overlaps(test_rois, message_level = "error")
  
  expect_equal(nrow(analysis$summary), 1)
  expect_equal(analysis$summary$overlapping_pairs, 1)
  expect_equal(analysis$summary$small_overlaps, 1)
  expect_equal(analysis$summary$large_overlaps, 0)
  expect_equal(analysis$summary$multiway_overlaps, 0)
  expect_equal(analysis$summary$containment_cases, 0)
  expect_equal(analysis$summary$recommendation, "Run resolve_roi_overlaps() to automatically split overlapping regions")
  
  expect_equal(nrow(analysis$details), 1)
  expect_equal(analysis$details$overlap_type, "small")
  expect_equal(analysis$details$recommendation, "Run resolve_roi_overlaps() to automatically split overlapping regions")
})

test_that("analyze_roi_overlaps detects large overlaps correctly", {
  # Create ROI data with large overlap (>50% of one ROI)
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("Small", "Large"),
    Text = c("Small", "Large"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",     # Small: (0,0) to (10,10) - area 100
      "2,2 15,2 15,15 2,15 2,2"      # Large: (2,2) to (15,15) - overlap (2,2)-(10,10) = 64, which is >50% of small ROI
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  analysis <- analyze_roi_overlaps(test_rois, message_level = "error")
  
  expect_equal(nrow(analysis$summary), 1)
  expect_equal(analysis$summary$overlapping_pairs, 1)
  expect_equal(analysis$summary$small_overlaps, 0)
  expect_equal(analysis$summary$large_overlaps, 1)
  expect_equal(analysis$summary$multiway_overlaps, 0)
  expect_equal(analysis$summary$containment_cases, 0)
  expect_equal(analysis$summary$recommendation, "Provide roi_priority vector to resolve conflicts")
  
  expect_equal(nrow(analysis$details), 1)
  expect_equal(analysis$details$overlap_type, "large")
  expect_equal(analysis$details$recommendation, "Provide roi_priority vector to specify which ROI takes precedence")
})

test_that("analyze_roi_overlaps detects containment correctly", {
  # Create ROI data with complete containment
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("Inner", "Outer"),
    Text = c("Inner", "Outer"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "5,5 8,5 8,8 5,8 5,5",         # Inner: (5,5) to (8,8) - small ROI inside larger
      "0,0 15,0 15,15 0,15 0,0"      # Outer: (0,0) to (15,15) - large ROI containing smaller
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  analysis <- analyze_roi_overlaps(test_rois, message_level = "error")
  
  expect_equal(nrow(analysis$summary), 1)
  expect_equal(analysis$summary$overlapping_pairs, 1)
  expect_equal(analysis$summary$small_overlaps, 0)
  expect_equal(analysis$summary$large_overlaps, 0)
  expect_equal(analysis$summary$multiway_overlaps, 0)
  expect_equal(analysis$summary$containment_cases, 1)
  expect_equal(analysis$summary$recommendation, "Provide roi_priority vector to resolve conflicts")
  
  expect_equal(nrow(analysis$details), 1)
  expect_equal(analysis$details$overlap_type, "containment")
  expect_equal(analysis$details$recommendation, "Provide roi_priority vector to handle nested ROI hierarchy (inner vs outer ROI precedence)")
})

test_that("cycif_assign_rois with check_overlaps warns about issues", {
  # Create test data with large overlaps that will be critical
  test_cells <- data.frame(
    CellID = 1,
    X_centroid = 7,
    Y_centroid = 7,
    slideName = "test_slide"
  )
  
  test_rois <- data.frame(
    Id = 1:2,
    Name = c("ROI1", "ROI2"),
    Text = c("ROI1", "ROI2"),
    type = c("Polygon", "Polygon"),
    all_points = c(
      "0,0 10,0 10,10 0,10 0,0",     # ROI1: (0,0) to (10,10)
      "2,2 15,2 15,15 2,15 2,2"      # ROI2: (2,2) to (15,15) - creates >50% overlap
    ),
    X = c(-1, -1),
    Y = c(-1, -1),
    RadiusX = c(-1, -1),
    RadiusY = c(-1, -1),
    Width = c(-1, -1),
    Height = c(-1, -1),
    all_transforms = c(-1, -1),
    slideName = "test_slide"
  )
  
  # Should stop with error when check_overlaps=TRUE and no roi_priority provided
  expect_error(
    cycif_assign_rois(
      cell_data = test_cells, 
      roi_data = test_rois, 
      expand_distance = 0,
      check_overlaps = TRUE
    ),
    "Critical ROI overlaps detected"
  )
  
  # Should work when roi_priority is provided (may produce messages but not errors)
  expect_no_error(
    cycif_assign_rois(
      cell_data = test_cells, 
      roi_data = test_rois, 
      expand_distance = 0,
      check_overlaps = TRUE,
      roi_priority = c("ROI1", "ROI2")
    )
  )
})
