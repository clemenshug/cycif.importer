test_that("cycif_load_cell_data works with example data", {
  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")

  expect_true(dir.exists(data_dir))

  # Load all cell data
  cell_data <- cycif_load_cell_data(data_dir)

  expect_type(cell_data, "list")
  expect_length(cell_data, 2)

  # Check slide names
  slide_names <- names(cell_data)
  expect_true("LSP11060" %in% slide_names)
  expect_true("LSP11064" %in% slide_names)

  # Check data structure for first slide
  first_slide <- cell_data[[1]]
  expect_type(first_slide, "data.frame")
  expect_equal(nrow(first_slide), 10000)

  # Check required columns
  required_cols <- c("CellID", "X_centroid", "Y_centroid")
  expect_true(all(required_cols %in% names(first_slide)))

  # Check slideName was added
  expect_true("slideName" %in% names(first_slide))
  expect_true(all(first_slide$slideName == names(cell_data)[1]))
})

test_that("cycif_load_cell_data works with slide filter", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")

  # Load only specific slide
  cell_data <- cycif_load_cell_data(data_dir, slide_filter = "LSP11060")

  expect_type(cell_data, "list")
  expect_equal(length(cell_data), 1)
  expect_equal(names(cell_data), "LSP11060")
})

test_that("cycif_load_cell_data handles errors correctly", {
  # Non-existent directory
  expect_error(
    cycif_load_cell_data("/nonexistent/path"),
    "No --unmicst_cell.csv files found"
  )

  # Non-existent slide filter
  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")

  expect_error(
    cycif_load_cell_data(data_dir, slide_filter = "NonExistentSlide"),
    "No files found matching slide filter"
  )
})

test_that("cycif_load_roi_data works with example data", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  roi_dir <- file.path(extdata_path, "example_rois")

  # Load ROI data for available slides
  slide_names <- c("LSP11060", "LSP11064")
  roi_data <- cycif_load_roi_data(roi_dir, slide_names)

  expect_type(roi_data, "list")
  expect_equal(length(roi_data), 2)
  expect_equal(names(roi_data), slide_names)

  # Check ROI data structure
  first_roi <- roi_data[[1]]
  expect_s3_class(first_roi, "data.frame")
  expect_true(nrow(first_roi) > 0)

  # Check required columns
  required_cols <- c("Id", "Name", "all_points")
  expect_true(all(required_cols %in% names(first_roi)))
})

test_that("gate loading works correctly", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  gate_file <- file.path(extdata_path, "example_gates.csv")

  expect_true(file.exists(gate_file))

  # Load gates
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)

  expect_s3_class(gates, "data.frame")
  expect_true("slideName" %in% names(gates))
  expect_true(nrow(gates) >= 2)  # Should have gates for both slides

  # Check that slides match our example data
  expect_true("LSP11060" %in% gates$slideName)
  expect_true("LSP11064" %in% gates$slideName)

  # Check that marker columns exist (should be numeric values)
  marker_cols <- setdiff(names(gates), "slideName")
  expect_true(length(marker_cols) > 0)
  expect_true(all(sapply(gates[marker_cols], is.numeric)))
})
