test_that("metadata addition works correctly", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:10,
    slideName = rep(c("slide1", "slide2"), each = 5),
    ROI = rep(c(1, 2), 5),
    ROIname = rep(c("ROI1", "ROI2"), 5)
  )

  # Create slide metadata
  slide_metadata <- data.frame(
    slideName = c("slide1", "slide2"),
    condition = c("control", "treatment"),
    batch = c("batch1", "batch1")
  )

  # Create ROI metadata
  roi_metadata <- data.frame(
    slideName = c("slide1", "slide1", "slide2", "slide2"),
    ROIname = c("ROI1", "ROI2", "ROI1", "ROI2"),
    tissue_type = c("tumor", "stroma", "tumor", "stroma")
  )

  # Test slide metadata addition
  with_slide_meta <- add_slide_metadata(test_data, slide_metadata)

  expect_s3_class(with_slide_meta, "data.frame")
  expect_true("condition" %in% names(with_slide_meta))
  expect_true("batch" %in% names(with_slide_meta))
  expect_equal(with_slide_meta$condition[1:5], rep("control", 5))
  expect_equal(with_slide_meta$condition[6:10], rep("treatment", 5))

  # Test ROI metadata addition
  with_roi_meta <- add_roi_metadata(test_data, roi_metadata)

  expect_s3_class(with_roi_meta, "data.frame")
  expect_true("tissue_type" %in% names(with_roi_meta))

  # Test both together
  with_both_meta <- test_data %>%
    add_slide_metadata(slide_metadata) %>%
    add_roi_metadata(roi_metadata)

  expect_s3_class(with_both_meta, "data.frame")
  expect_true("condition" %in% names(with_both_meta))
  expect_true("tissue_type" %in% names(with_both_meta))
})

test_that("metadata addition handles missing data", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:6,
    slideName = rep(c("slide1", "slide2", "slide3"), each = 2),
    ROIname = rep(c("ROI1", "ROI2"), 3)
  )

  # Create incomplete metadata (missing slide3)
  slide_metadata <- data.frame(
    slideName = c("slide1", "slide2"),
    condition = c("control", "treatment")
  )

  # Test with missing metadata
  with_meta <- add_slide_metadata(test_data, slide_metadata)

  expect_s3_class(with_meta, "data.frame")
  expect_true("condition" %in% names(with_meta))

  # Check that missing values are NA
  expect_equal(with_meta$condition[1:2], rep("control", 2))
  expect_equal(with_meta$condition[3:4], rep("treatment", 2))
  expect_true(all(is.na(with_meta$condition[5:6])))
})

test_that("metadata addition with NULL metadata", {
  test_data <- data.frame(
    CellID = 1:5,
    slideName = rep("slide1", 5)
  )

  # Test with NULL metadata
  result_slide <- add_slide_metadata(test_data, NULL)
  result_roi <- add_roi_metadata(test_data, NULL)

  expect_identical(result_slide, test_data)
  expect_identical(result_roi, test_data)
})

test_that("clean_formula_label works correctly", {
  expect_equal(clean_formula_label("~PanCKp"), "PanCKp")
  expect_equal(clean_formula_label("~!PanCKp"), "not_PanCKp")
  expect_equal(clean_formula_label("~CD8ap & PanCKp"), "CD8ap_and_PanCKp")
  expect_equal(clean_formula_label("~CD8ap | PanCKp"), "CD8ap_or_PanCKp")
  expect_equal(clean_formula_label("~(CD8ap | CD4p) & PanCKp"), "CD8ap_or_CD4p_and_PanCKp")
})

test_that("validation functions work", {
  # Valid data
  valid_data <- data.frame(
    CellID = 1:5,
    X_centroid = runif(5),
    Y_centroid = runif(5),
    slideName = rep("test", 5)
  )

  expect_silent(validate_cell_data_structure(valid_data))

  # Invalid data - missing required columns
  invalid_data <- data.frame(
    CellID = 1:5,
    X_centroid = runif(5)
    # Missing Y_centroid and slideName
  )

  expect_error(validate_cell_data_structure(invalid_data), "Missing required columns")
})
