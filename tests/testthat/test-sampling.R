test_that("sampling works with all_cells mode", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:100,
    X_centroid = runif(100),
    Y_centroid = runif(100),
    ROI = c(rep(0, 50), rep(1, 30), rep(2, 20)),  # 50 unassigned, 30 in ROI1, 20 in ROI2
    ROIname = c(rep("none", 50), rep("ROI1", 30), rep("ROI2", 20)),
    slideName = rep("test", 100)
  )

  # Test sampling
  sampled <- cycif_sample(test_data, sample_size = 30, sampling_mode = "all_cells")

  expect_s3_class(sampled, "data.frame")
  expect_equal(nrow(sampled), 30)
  expect_true("slideName" %in% names(sampled))
})

test_that("sampling works with roi_only mode", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:100,
    X_centroid = runif(100),
    Y_centroid = runif(100),
    ROI = c(rep(0, 50), rep(1, 30), rep(2, 20)),
    ROIname = c(rep("none", 50), rep("ROI1", 30), rep("ROI2", 20)),
    slideName = rep("test", 100)
  )

  # Test ROI-only sampling
  sampled <- cycif_sample(test_data, sample_size = 25, sampling_mode = "roi_only")

  expect_s3_class(sampled, "data.frame")
  expect_true(nrow(sampled) <= 25)
  expect_true(all(sampled$ROI > 0))  # No unassigned cells
  expect_true(all(sampled$ROIname != "none"))
})

test_that("sampling works with multiple slides", {
  # Create test data for multiple slides
  test_data <- list(
    slide1 = data.frame(
      CellID = 1:50,
      X_centroid = runif(50),
      Y_centroid = runif(50),
      ROI = rep(1, 50),
      ROIname = rep("ROI1", 50),
      slideName = rep("slide1", 50)
    ),
    slide2 = data.frame(
      CellID = 1:80,
      X_centroid = runif(80),
      Y_centroid = runif(80),
      ROI = rep(1, 80),
      ROIname = rep("ROI1", 80),
      slideName = rep("slide2", 80)
    )
  )

  # Test sampling
  sampled <- cycif_sample(test_data, sample_size = 60, sampling_mode = "all_cells")

  expect_s3_class(sampled, "data.frame")
  expect_equal(nrow(sampled), 60)

  # Check that both slides are represented
  slide_counts <- table(sampled$slideName)
  expect_true(length(slide_counts) == 2)
  expect_true("slide1" %in% names(slide_counts))
  expect_true("slide2" %in% names(slide_counts))
})

test_that("sampling respects sample size limits", {
  # Create small test dataset
  test_data <- data.frame(
    CellID = 1:10,
    X_centroid = runif(10),
    Y_centroid = runif(10),
    ROI = rep(1, 10),
    ROIname = rep("ROI1", 10),
    slideName = rep("test", 10)
  )

  # Request more samples than available
  sampled <- cycif_sample(test_data, sample_size = 20, sampling_mode = "all_cells")

  expect_s3_class(sampled, "data.frame")
  expect_equal(nrow(sampled), 10)  # Should get all available cells
})

test_that("sampling with NULL sample_size returns all data", {
  test_data <- data.frame(
    CellID = 1:50,
    X_centroid = runif(50),
    Y_centroid = runif(50),
    ROI = rep(1, 50),
    ROIname = rep("ROI1", 50),
    slideName = rep("test", 50)
  )

  sampled <- cycif_sample(test_data, sample_size = NULL, sampling_mode = "all_cells")

  expect_s3_class(sampled, "data.frame")
  expect_equal(nrow(sampled), 50)
})

test_that("sampling works with example data", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")

  # Load and process data
  cell_data <- cycif_load_cell_data(data_dir)
  roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))
  cell_data_roi <- cycif_assign_rois(cell_data, roi_data)

  # Test sampling
  sampled <- cycif_sample(cell_data_roi, sample_size = 1000, sampling_mode = "all_cells")

  expect_s3_class(sampled, "data.frame")
  expect_true(nrow(sampled) <= 1000)
  expect_true("slideName" %in% names(sampled))

  # Test ROI-only sampling
  sampled_roi <- cycif_sample(cell_data_roi, sample_size = 500, sampling_mode = "roi_only")

  expect_s3_class(sampled_roi, "data.frame")
  expect_true(all(sampled_roi$ROI > 0))
})
