test_that("gating works with example data", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")
  gate_file <- file.path(extdata_path, "example_gates.csv")

  # Load data
  cell_data <- cycif_load_cell_data(data_dir)
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)

  # Apply gates
  gated_data <- cycif_apply_gates(cell_data, gates)

  expect_type(gated_data, "list")
  expect_equal(names(gated_data), names(cell_data))

  # Check first slide
  first_slide <- gated_data[[1]]
  expect_s3_class(first_slide, "data.frame")

  # Check that gate columns were added
  gate_cols <- grep("p$", names(first_slide), value = TRUE)
  expect_true(length(gate_cols) > 0)

  # Check that gate columns are logical
  for (col in gate_cols) {
    expect_type(first_slide[[col]], "logical")
  }

  # Check that some cells are positive and some negative for each marker
  for (col in gate_cols) {
    gate_values <- first_slide[[col]]
    expect_true(sum(gate_values) > 0)  # Some positive
    expect_true(sum(!gate_values) > 0)  # Some negative
  }
})

test_that("gating works without gate table", {
  skip_if_not_installed("readr")

  extdata_path <- system.file("extdata", package = "cycif.importer")
  data_dir <- file.path(extdata_path, "example_quants")

  cell_data <- cycif_load_cell_data(data_dir)

  # Apply gates without gate table (should use default thresholds)
  gated_data <- cycif_apply_gates(cell_data, gate_thresholds = NULL)

  expect_type(gated_data, "list")
  first_slide <- gated_data[[1]]

  # Should still have some gate columns
  gate_cols <- grep("p$", names(first_slide), value = TRUE)
  expect_true(length(gate_cols) > 0)
})

test_that("single gates work correctly", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:10,
    CD8a = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    PanCK = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20),
    slideName = "test"
  )

  # Create gates
  gates <- data.frame(
    slideName = "test",
    CD8a = 5.5,  # Threshold
    PanCK = 11   # Threshold
  )

  # Apply single gates
  result <- apply_single_gates(test_data, gates)

  expect_s3_class(result, "data.frame")
  expect_true("CD8ap" %in% names(result))
  expect_true("PanCKp" %in% names(result))

  # Check gate results
  expect_equal(result$CD8ap, test_data$CD8a > 5.5)
  expect_equal(result$PanCKp, test_data$PanCK > 11)
})

test_that("double gates work correctly", {
  # Create test data with gate columns
  test_data <- data.frame(
    CellID = 1:8,
    CD8ap = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    CD4p = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    PanCKp = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    slideName = "test"
  )

  # Define double gates
  double_gates <- data.frame(
    marker1 = c("CD8a", "CD4"),
    marker2 = c("PanCK", "PanCK")
  )

  # Apply double gates
  result <- apply_double_gates(test_data, double_gates)

  expect_s3_class(result, "data.frame")
  expect_true("CD8ap_PanCKp" %in% names(result))
  expect_true("CD4p_PanCKp" %in% names(result))

  # Check results
  expect_equal(result$CD8ap_PanCKp, test_data$CD8ap & test_data$PanCKp)
  expect_equal(result$CD4p_PanCKp, test_data$CD4p & test_data$PanCKp)
})

test_that("default double gates are available", {
  double_gates <- get_default_double_gates()

  expect_s3_class(double_gates, "data.frame")
  expect_true("marker1" %in% names(double_gates))
  expect_true("marker2" %in% names(double_gates))
  expect_true(nrow(double_gates) > 0)
})

test_that("convert_logical_to_numeric works", {
  test_data <- data.frame(
    CellID = 1:3,
    CD8ap = c(TRUE, FALSE, TRUE),
    CD4p = c(FALSE, TRUE, FALSE),
    Area = c(100, 200, 300)
  )

  result <- convert_logical_to_numeric(test_data)

  expect_s3_class(result, "data.frame")
  expect_type(result$CD8ap, "double")
  expect_type(result$CD4p, "double")
  expect_type(result$Area, "double")  # Should remain numeric

  expect_equal(result$CD8ap, c(1, 0, 1))
  expect_equal(result$CD4p, c(0, 1, 0))
})
