test_that("pipeline components work with standardized data", {
  # Create minimal test data
  test_cells <- data.frame(
    CellID = 1:100,
    X_centroid = runif(100, 0, 1000),
    Y_centroid = runif(100, 0, 1000),
    Area = runif(100, 10, 100),
    CD8a = rnorm(100, 2, 1),
    CD4 = rnorm(100, 2, 1),
    PanCK = rnorm(100, 3, 1),
    slideName = "test_slide"
  )

  gate_data <- data.frame(
    slideName = "test_slide",
    CD8a = 2,  # Use linear threshold for test
    CD4 = 2,
    PanCK = 3
  )

  # Test data standardization
  std_data <- standardize_input_data(test_cells)
  expect_type(std_data, "list")
  expect_true("test_slide" %in% names(std_data))

  # Test coordinate standardization
  coords_data <- standardize_coordinates(test_cells)
  expect_true("Xt" %in% names(coords_data))
  expect_true("Yt" %in% names(coords_data))

  # Test gating
  gated_data <- apply_single_gates(test_cells, gate_data)
  expect_true("CD8ap" %in% names(gated_data))
  expect_true("CD4p" %in% names(gated_data))
  expect_true("PanCKp" %in% names(gated_data))

  # Test sampling
  sampled_data <- cycif_sample(gated_data, sample_size = 50)
  expect_s3_class(sampled_data, "data.frame")
  expect_lte(nrow(sampled_data), 50)
  expect_true("slideName" %in% names(sampled_data))

  # Test summarization
  results <- cycif_summarize(sampled_data)
  expect_type(results, "list")
  expect_true("by_roi" %in% names(results))
  expect_s3_class(results$by_roi, "data.frame")
})

test_that("sampling modes work correctly", {
  # Create test data with ROI assignments
  test_cells <- data.frame(
    CellID = 1:20,
    X_centroid = runif(20, 0, 1000),
    Y_centroid = runif(20, 0, 1000),
    ROI = c(rep(0, 10), rep(1, 10)),  # Half unassigned, half in ROI
    ROIname = c(rep("none", 10), rep("ROI_1", 10)),
    slideName = "test_slide"
  )

  # Test all_cells mode
  sampled_all <- cycif_sample(test_cells, sample_size = 15, sampling_mode = "all_cells")
  expect_lte(nrow(sampled_all), 15)

  # Test roi_only mode
  sampled_roi <- cycif_sample(test_cells, sample_size = 15, sampling_mode = "roi_only")
  expect_lte(nrow(sampled_roi), 10)  # Max 10 ROI cells available
  expect_true(all(sampled_roi$ROIname != "none"))
})

test_that("pipeline handles edge cases", {
  # Test with single cell
  single_cell <- data.frame(
    CellID = 1,
    X_centroid = 500,
    Y_centroid = 500,
    ROI = 0,
    ROIname = "none",
    slideName = "test"
  )

  sampled <- cycif_sample(single_cell, sample_size = 10, sampling_mode = "all_cells")
  expect_equal(nrow(sampled), 1)

  # Test with empty data
  empty_data <- data.frame(
    CellID = integer(0),
    X_centroid = numeric(0),
    Y_centroid = numeric(0),
    ROI = integer(0),
    ROIname = character(0),
    slideName = character(0)
  )

  sampled_empty <- cycif_sample(empty_data, sample_size = 10, sampling_mode = "all_cells")
  expect_equal(nrow(sampled_empty), 0)
})

test_that("pipeline preserves data integrity", {
  # Create test data with known properties
  test_data <- data.frame(
    CellID = 1:50,
    X_centroid = runif(50, 0, 1000),
    Y_centroid = runif(50, 0, 1000),
    ROI = c(rep(0, 25), rep(1, 25)),
    ROIname = c(rep("none", 25), rep("ROI1", 25)),
    CD8a = c(rep(10, 25), rep(1, 25)),  # First 25 high, last 25 low
    slideName = "test"
  )

  # Apply gating
  gate_data <- data.frame(slideName = "test", CD8a = 5)
  gated <- apply_single_gates(test_data, gate_data)

  # Check that gating worked correctly
  expect_equal(sum(gated$CD8ap), 25)  # First 25 should be positive
  expect_equal(sum(!gated$CD8ap), 25)  # Last 25 should be negative

  # Test that sampling preserves proportions approximately
  sampled <- cycif_sample(gated, sample_size = 40, sampling_mode = "all_cells")
  roi_proportion <- sum(sampled$ROI > 0) / nrow(sampled)
  expect_true(roi_proportion > 0.3 && roi_proportion < 0.7)  # Should be around 0.5
})
