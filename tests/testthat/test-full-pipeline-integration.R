test_that("complete pipeline works with example data", {
  skip_if_not_installed("readr")

  # Get paths to example data
  extdata_path <- system.file("extdata", package = "cycif.importer")
  expect_true(dir.exists(extdata_path), "Example data directory exists")

  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")
  gate_file <- file.path(extdata_path, "example_gates.csv")

  # Verify example files exist
  expect_true(dir.exists(data_dir), "Example quantification data directory exists")
  expect_true(dir.exists(roi_dir), "Example ROI directory exists")
  expect_true(file.exists(gate_file), "Example gates file exists")

  # Create temporary output directory
  output_dir <- tempdir()

  # Run complete pipeline
  results <- cycif_pipeline(
    data_dir = data_dir,
    roi_dir = roi_dir,
    gate_table_path = gate_file,
    output_dir = output_dir,
    sample_size = 1000,  # Small sample for testing
    sampling_mode = "all_cells"
  )

  # Test structure of results
  expect_type(results, "list")
  expect_true("all_cells" %in% names(results))
  expect_true("sampled_data" %in% names(results))
  expect_true("by_roi" %in% names(results))
  expect_true("by_slide" %in% names(results))
  expect_true("by_roi_type" %in% names(results))

  # Test all_cells data
  expect_type(results$all_cells, "list")
  expect_true(length(results$all_cells) > 0)

  # Check that slides are present
  slides <- names(results$all_cells)
  expect_true(length(slides) >= 2)  # Should have LSP11060 and LSP11064

  # Test first slide data structure
  first_slide <- results$all_cells[[1]]
  expect_s3_class(first_slide, "data.frame")

  # Check required columns exist
  required_cols <- c("CellID", "X_centroid", "Y_centroid", "slideName", "ROI", "ROIname")
  expect_true(all(required_cols %in% names(first_slide)))

  # Check that gating columns were added (should have 'p' suffix for positive gates)
  gate_cols <- grep("p$", names(first_slide), value = TRUE)
  expect_true(length(gate_cols) > 0)

  # Test sampled data
  expect_s3_class(results$sampled_data, "data.frame")
  expect_true(nrow(results$sampled_data) <= 1000)  # Should respect sample size
  expect_true("slideName" %in% names(results$sampled_data))

  # Test summary data
  expect_s3_class(results$by_roi, "data.frame")
  expect_s3_class(results$by_slide, "data.frame")
  expect_s3_class(results$by_roi_type, "data.frame")

  # Check summary structure
  expect_true("slideName" %in% names(results$by_slide))
  expect_true("ROI" %in% names(results$by_roi))
  expect_true("ROIname" %in% names(results$by_roi_type))

  # Check that counts were calculated
  expect_true("n_cells" %in% names(results$by_roi))
  expect_true("n_cells" %in% names(results$by_slide))
})

test_that("pipeline works with roi_only sampling", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")
  gate_file <- file.path(extdata_path, "example_gates.csv")
  output_dir <- tempdir()

  results <- cycif_pipeline(
    data_dir = data_dir,
    roi_dir = roi_dir,
    gate_table_path = gate_file,
    output_dir = output_dir,
    sample_size = 500,
    sampling_mode = "roi_only"
  )

  # All sampled cells should be in ROIs (not ROI 0 or "none")
  expect_true(all(results$sampled_data$ROI > 0))
  expect_true(all(results$sampled_data$ROIname != "none"))
})

test_that("pipeline works without gates", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")
  output_dir <- tempdir()

  results <- cycif_pipeline(
    data_dir = data_dir,
    roi_dir = roi_dir,
    gate_table_path = NULL,  # No gates
    output_dir = output_dir,
    sample_size = 500
  )

  expect_type(results, "list")
  expect_true("sampled_data" %in% names(results))
})

test_that("pipeline works with slide filter", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  roi_dir <- file.path(extdata_path, "example_rois")
  output_dir <- tempdir()

  results <- cycif_pipeline(
    data_dir = data_dir,
    roi_dir = roi_dir,
    output_dir = output_dir,
    sample_size = 500,
    slide_filter = "LSP11060"  # Only process one slide
  )

  # Should only have one slide
  expect_equal(length(results$all_cells), 1)
  expect_equal(names(results$all_cells), "LSP11060")
  expect_true(all(results$sampled_data$slideName == "LSP11060"))
})

test_that("pipeline validates input directories", {
  expect_error(
    cycif_pipeline(
      data_dir = "/nonexistent/path",
      output_dir = tempdir()
    ),
    "Data directory does not exist"
  )

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")

  expect_error(
    cycif_pipeline(
      data_dir = data_dir,
      roi_dir = "/nonexistent/path",
      output_dir = tempdir()
    ),
    "ROI directory.*does not exist"
  )

  expect_error(
    cycif_pipeline(
      data_dir = data_dir,
      gate_table_path = "/nonexistent/file.csv",
      output_dir = tempdir()
    ),
    "Gate table file.*does not exist"
  )
})
