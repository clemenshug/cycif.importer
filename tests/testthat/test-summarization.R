test_that("summarization works with basic data", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:20,
    slideName = rep(c("slide1", "slide2"), each = 10),
    ROI = rep(c(1, 2), 10),
    ROIname = rep(c("ROI1", "ROI2"), 10),
    CD8ap = rep(c(TRUE, FALSE), 10),
    PanCKp = rep(c(TRUE, TRUE, FALSE, FALSE), 5)
  )

  # Test summarization
  results <- cycif_summarize(test_data)

  expect_type(results, "list")
  expect_true("by_roi" %in% names(results))
  expect_true("by_slide" %in% names(results))
  expect_true("by_roi_type" %in% names(results))

  # Check by_roi summary
  by_roi <- results$by_roi
  expect_s3_class(by_roi, "data.frame")
  expect_true("slideName" %in% names(by_roi))
  expect_true("ROI" %in% names(by_roi))
  expect_true("ROIname" %in% names(by_roi))
  expect_true("n_cells" %in% names(by_roi))

  # Check by_slide summary
  by_slide <- results$by_slide
  expect_s3_class(by_slide, "data.frame")
  expect_true("slideName" %in% names(by_slide))
  expect_true("n_cells" %in% names(by_slide))
  expect_equal(nrow(by_slide), 2)  # Two slides
})

test_that("stratified summaries work", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:40,
    slideName = rep("test_slide", 40),
    ROI = rep(c(1, 2), each = 20),
    ROIname = rep(c("ROI1", "ROI2"), each = 20),
    PanCKp = c(rep(TRUE, 10), rep(FALSE, 10), rep(TRUE, 15), rep(FALSE, 5)),
    CD8ap = rep(c(TRUE, FALSE), 20)
  )

  # Define filters
  filters <- list(
    "panCK+" = ~PanCKp,
    "panCK-" = ~!PanCKp,
    "CD8+" = ~CD8ap
  )

  # Test stratified summaries
  results <- cycif_summarize(test_data, summary_filters = filters)

  expect_type(results, "list")

  # Check that filter-specific summaries exist
  expect_true("by_roi_panCK+" %in% names(results))
  expect_true("by_roi_panCK-" %in% names(results))
  expect_true("by_roi_CD8+" %in% names(results))

  # Check structure
  panck_pos <- results$`by_roi_panCK+`
  expect_s3_class(panck_pos, "data.frame")
  expect_true("n_cells" %in% names(panck_pos))

  # Check counts make sense
  expect_true(all(panck_pos$n_cells >= 0))
})

test_that("create_slide_roi_summary works", {
  # Create test data
  test_data <- data.frame(
    slideName = rep(c("slide1", "slide2"), each = 15),
    ROI = rep(c(1, 2, 3), 10),
    ROIname = rep(c("ROI1", "ROI2", "ROI3"), 10),
    Area = runif(30, 10, 100),
    CD8ap = rep(c(TRUE, FALSE), 15)
  )

  # Test summary creation
  summary <- create_slide_roi_summary(test_data, group_vars = c("slideName", "ROI", "ROIname"))

  expect_s3_class(summary, "data.frame")
  expect_true("slideName" %in% names(summary))
  expect_true("ROI" %in% names(summary))
  expect_true("ROIname" %in% names(summary))
  expect_true("n_cells" %in% names(summary))
  expect_true("mean_Area" %in% names(summary))
  expect_true("sum_CD8ap" %in% names(summary))

  # Check that we have the right number of groups
  expect_equal(nrow(summary), 6)  # 2 slides × 3 ROIs
})

test_that("create_stratified_summaries works", {
  # Create test data
  test_data <- data.frame(
    slideName = rep("test", 20),
    ROI = rep(c(1, 2), each = 10),
    ROIname = rep(c("ROI1", "ROI2"), each = 10),
    PanCKp = c(rep(TRUE, 5), rep(FALSE, 5), rep(TRUE, 8), rep(FALSE, 2))
  )

  # Define groups and filters
  groups <- list(
    by_roi = c("slideName", "ROI", "ROIname"),
    by_slide = c("slideName")
  )

  filters <- list(
    "panCK+" = ~PanCKp,
    "panCK-" = ~!PanCKp
  )

  # Create stratified summaries
  results <- create_stratified_summaries(test_data, groups, filters)

  expect_type(results, "list")
  expect_equal(length(results), 4)  # 2 groups × 2 filters

  # Check names
  expected_names <- c("by_roi_panCK+", "by_roi_panCK-", "by_slide_panCK+", "by_slide_panCK-")
  expect_setequal(names(results), expected_names)

  # Check structure
  expect_s3_class(results$`by_roi_panCK+`, "data.frame")
  expect_true("n_cells" %in% names(results$`by_roi_panCK+`))
})

test_that("summarization works with example data", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")
  gate_file <- file.path(extdata_path, "example_gates.csv")

  # Load and process data
  cell_data <- cycif_load_cell_data(data_dir)
  roi_data <- cycif_load_roi_data(roi_dir, names(cell_data))
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)

  cell_data_roi <- cycif_assign_rois(cell_data, roi_data)
  gated_data <- cycif_apply_gates(cell_data_roi, gates)
  sampled_data <- cycif_sample(gated_data, sample_size = 1000)

  # Test summarization
  results <- cycif_summarize(sampled_data)

  expect_type(results, "list")
  expect_true("by_roi" %in% names(results))
  expect_true("by_slide" %in% names(results))

  # Check that summaries have data
  expect_true(nrow(results$by_slide) > 0)
  expect_true(nrow(results$by_roi) > 0)
})
