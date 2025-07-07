library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("pipeline writes expected files", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  gate_file <- file.path(extdata, "example_gates.csv")
  out <- tempfile()
  dir.create(out)
  cycif_pipeline(
    data_dir = data_dir,
    roi_dir = roi_dir,
    gate_table_path = gate_file,
    output_dir = out,
    sample_size = 10,
    sampling_mode = "all_cells",
    expand_distance = 0
  )
  files <- list.files(out)
  expect_true("sampled_cells.csv.gz" %in% files)
  expect_true(any(grepl("^summary_by_slide__", files)))
})
