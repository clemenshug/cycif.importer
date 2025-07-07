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
