library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("cycif_assign_rois assigns ROI ids", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 50)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  expect_type(assigned, "list")
  expect_true(all(purrr::map_lgl(assigned, ~"ROI" %in% names(.x))))
  expect_true(any(assigned[[1]]$ROI > 0))
})

test_that("cycif_assign_rois works without ROI data", {
  data_dir <- file.path(extdata, "example_quants")
  cells <- cycif_load_cell_data(data_dir)
  cells <- list(LSP11060 = head(cells$LSP11060, 10))
  res <- cycif_assign_rois(cells, NULL)
  expect_true(all(res$LSP11060$ROI == 0))
  expect_true(all(res$LSP11060$ROIname == "none"))
})
