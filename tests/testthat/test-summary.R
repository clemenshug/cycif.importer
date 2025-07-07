library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("cycif_summarize aggregates by group", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  gate_file <- file.path(extdata, "example_gates.csv")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 30)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)
  gated <- cycif_apply_gates(assigned, gates, double_gates = get_default_double_gates())
  sampled <- cycif_sample(gated, sample_size = NULL)
  summ <- cycif_summarize(sampled,
                         summary_filters = list(`CD8+` = ~CD8ap),
                         summary_groups = list(by_slide = "slideName"))
  expect_true("by_slide__CD8+" %in% names(summ))
  df <- summ[["by_slide__CD8+"]]
  expect_true(all(c("slideName", "cell_count") %in% names(df)))
  expect_true(nrow(df) >= 1)
})
