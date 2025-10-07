library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

# Tests for data and ROI loading functions

test_that("cycif_load_cell_data loads example data", {
  data_dir <- file.path(extdata, "example_quants")
  cells <- cycif_load_cell_data(data_dir)
  expect_type(cells, "list")
  expect_setequal(names(cells), c("LSP11060", "LSP11064"))
  expect_true(all(purrr::map_lgl(cells, is.data.frame)))
  expect_true(all(purrr::map_lgl(cells, ~"slideName" %in% names(.x))))
})

test_that("cycif_load_cell_data respects slide_filter", {
  data_dir <- file.path(extdata, "example_quants")
  one <- cycif_load_cell_data(data_dir, slide_filter = "LSP11060")
  expect_equal(names(one), "LSP11060")
})

test_that("cycif_load_roi_data loads ROI data", {
  roi_dir <- file.path(extdata, "example_rois")
  rois <- cycif_load_roi_data(roi_dir)
  expect_s3_class(rois, "data.frame")
  expect_setequal(unique(rois$slideName), c("LSP11060", "LSP11064"))
  expect_true(all(c("slideName", "Name", "all_points") %in% names(rois)))
})

test_that("custom file patterns work for cell data", {
  # Create temp directory with custom file naming
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Copy and rename files to custom pattern
  file.copy(
    file.path(extdata, "example_quants/LSP11060--unmicst_cell.csv.gz"),
    file.path(temp_dir, "Sample_LSP11060_cells.csv.gz")
  )
  file.copy(
    file.path(extdata, "example_quants/LSP11064--unmicst_cell.csv.gz"),
    file.path(temp_dir, "Sample_LSP11064_cells.csv.gz")
  )

  # Test custom pattern
  cells <- cycif_load_cell_data(temp_dir, file_pattern = "Sample_*_cells.csv.gz")
  expect_type(cells, "list")
  expect_setequal(names(cells), c("LSP11060", "LSP11064"))
  expect_true(all(purrr::map_lgl(cells, ~"slideName" %in% names(.x))))

  unlink(temp_dir, recursive = TRUE)
})

test_that("custom file patterns work for ROI data", {
  # Create temp directory with custom file naming
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Copy and rename files to custom pattern
  file.copy(
    file.path(extdata, "example_rois/LSP11060-rois.csv"),
    file.path(temp_dir, "Slide_LSP11060_roi.csv")
  )
  file.copy(
    file.path(extdata, "example_rois/LSP11064-rois.csv"),
    file.path(temp_dir, "Slide_LSP11064_roi.csv")
  )

  # Test custom pattern
  rois <- cycif_load_roi_data(temp_dir, file_pattern = "Slide_*_roi.csv")
  expect_s3_class(rois, "data.frame")
  expect_setequal(unique(rois$slideName), c("LSP11060", "LSP11064"))

  unlink(temp_dir, recursive = TRUE)
})

test_that("pattern without * throws error", {
  temp_dir <- tempfile()
  dir.create(temp_dir)

  expect_error(
    cycif_load_cell_data(temp_dir, file_pattern = "cells.csv"),
    "must contain at least one '\\*'"
  )

  unlink(temp_dir, recursive = TRUE)
})

test_that("glob wildcards work correctly", {
  # Create temp directory with files using ? wildcard
  temp_dir <- tempfile()
  dir.create(temp_dir)

  # Create test files
  file.copy(
    file.path(extdata, "example_quants/LSP11060--unmicst_cell.csv.gz"),
    file.path(temp_dir, "Sample_LSP11060_v1.csv.gz")
  )

  # Test ? wildcard (single character)
  cells <- cycif_load_cell_data(temp_dir, file_pattern = "Sample_*_v?.csv.gz")
  expect_type(cells, "list")
  expect_equal(names(cells), "LSP11060")

  unlink(temp_dir, recursive = TRUE)
})
