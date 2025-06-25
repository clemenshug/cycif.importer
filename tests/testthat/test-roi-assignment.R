test_that("ROI assignment works with example data", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")

  # Load test data
  cell_data <- cycif_load_cell_data(data_dir)
  roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))

  # Assign ROIs
  cell_data_roi <- cycif_assign_rois(
    cell_data,
    roi_data = roi_data,
    scale_factor = 0.65,
    expand_distance = 50
  )

  expect_type(cell_data_roi, "list")
  expect_equal(names(cell_data_roi), names(cell_data))

  # Check first slide
  first_slide <- cell_data_roi[[1]]
  expect_s3_class(first_slide, "data.frame")

  # Check that ROI columns were added
  expect_true("ROI" %in% names(first_slide))
  expect_true("ROIname" %in% names(first_slide))

  # Check that some cells are assigned to ROIs
  roi_assignments <- table(first_slide$ROI)
  expect_true(length(roi_assignments) > 1)  # Should have at least ROI 0 (unassigned) and others

  # Check that ROI names are assigned correctly
  roi_cells <- first_slide[first_slide$ROI > 0, ]
  if (nrow(roi_cells) > 0) {
    expect_true(all(roi_cells$ROIname != "none"))
  }

  # Check that unassigned cells have correct ROI info
  unassigned_cells <- first_slide[first_slide$ROI == 0, ]
  if (nrow(unassigned_cells) > 0) {
    expect_true(all(unassigned_cells$ROIname == "none"))
  }
})

test_that("ROI assignment works without expansion", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")

  cell_data <- cycif_load_cell_data(data_dir)
  roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))

  # Assign ROIs without expansion
  cell_data_roi <- cycif_assign_rois(
    cell_data,
    roi_data = roi_data,
    scale_factor = 0.65,
    expand_distance = 0  # No expansion
  )

  expect_type(cell_data_roi, "list")
  first_slide <- cell_data_roi[[1]]
  expect_true("ROI" %in% names(first_slide))
  expect_true("ROIname" %in% names(first_slide))
})

test_that("coordinate standardization works", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:5,
    X_centroid = c(100, 200, 300, 400, 500),
    Y_centroid = c(150, 250, 350, 450, 550),
    slideName = "test"
  )

  # Test standardize_coordinates
  std_data <- standardize_coordinates(test_data, scale_factor = 1.0)

  expect_s3_class(std_data, "data.frame")
  expect_true("Xt" %in% names(std_data))
  expect_true("Yt" %in% names(std_data))

  # Check that coordinates are transformed
  expect_equal(std_data$Xt, test_data$X_centroid)
  expect_equal(std_data$Yt, test_data$Y_centroid)

  # Test with different scale factor
  std_data2 <- standardize_coordinates(test_data, scale_factor = 2.0)
  expect_equal(std_data2$Xt, test_data$X_centroid * 2.0)
  expect_equal(std_data2$Yt, test_data$Y_centroid * 2.0)
})

test_that("assign_single_roi works correctly", {
  # Create test cells
  test_cells <- data.frame(
    CellID = 1:4,
    Xt = c(10, 20, 100, 200),  # First two inside, last two outside
    Yt = c(10, 20, 100, 200)
  )

  # Create simple square ROI (0,0) to (50,50)
  test_roi <- data.frame(
    Id = 1,
    Name = "TestROI",
    all_points = "0,0 50,0 50,50 0,50"
  )

  assigned <- assign_single_roi(test_cells, test_roi, expand_distance = 0)

  expect_s3_class(assigned, "data.frame")
  expect_true("ROI" %in% names(assigned))
  expect_true("ROIname" %in% names(assigned))

  # Check assignments
  expect_equal(assigned$ROI[1:2], c(1, 1))  # Inside ROI
  expect_equal(assigned$ROI[3:4], c(0, 0))  # Outside ROI
  expect_equal(assigned$ROIname[1:2], c("TestROI", "TestROI"))
  expect_equal(assigned$ROIname[3:4], c("none", "none"))
})
