test_that("pipeline functions handle basic workflow", {
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

  # Test Stage 1: Import and Gate
  gated_data <- cycif_import_and_gate(test_cells)

  expect_type(gated_data, "list")
  expect_true("test_slide" %in% names(gated_data))
  expect_true("Xt" %in% names(gated_data$test_slide))
  expect_true("Yt" %in% names(gated_data$test_slide))
  expect_true("ROI" %in% names(gated_data$test_slide))
  expect_true("ROIname" %in% names(gated_data$test_slide))

  # Test Stage 2: Sample
  sampled_data <- cycif_sample(gated_data, sample_size = 50)

  expect_s3_class(sampled_data, "data.frame")
  expect_lte(nrow(sampled_data), 50)
  expect_true("slideName" %in% names(sampled_data))

  # Test Stage 3: Summarize
  results <- cycif_summarize(sampled_data)

  expect_type(results, "list")
  expect_true("roi_summaries" %in% names(results))
  expect_true("sampled_cells" %in% names(results))
  expect_true("stratified_summaries" %in% names(results))
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
