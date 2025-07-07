library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("cycif_apply_gates adds gate columns", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  gate_file <- file.path(extdata, "example_gates.csv")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 30)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)
  gated <- cycif_apply_gates(assigned, gates, double_gates = get_default_double_gates())
  slide1 <- gated$LSP11060
  expect_true("PanCKp" %in% names(slide1))
  expect_type(slide1$PanCKp, "logical")
  thr <- exp(gates$PanCK[gates$slideName == "LSP11060"])
  expect_equal(slide1$PanCKp[1], slide1$PanCK[1] > thr)
  expect_true("CD8apCD103p" %in% names(slide1))
})

test_that("cycif_marker_combinations works and notes missing columns", {
  df <- data.frame(Ap = c(TRUE, FALSE), Bp = c(TRUE, TRUE))
  combos <- list(~Ap & Bp, missing = ~Ap & Cp)
  res <- cycif_marker_combinations(list(test = df), combos)
  out <- res[[1]]
  expect_true("Ap_and_Bp" %in% names(out))
  expect_false("missing" %in% names(out))
  expect_equal(attr(res, "missing_combinations"), "missing")
  expect_equal(attr(res, "missing_vars"), "Cp")
})
