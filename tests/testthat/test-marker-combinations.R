test_that("standardize_input_data works correctly", {

  # Test with single dataframe without slideName
  df1 <- data.frame(x = 1:3, y = 4:6)
  result1 <- standardize_input_data(df1)
  expect_type(result1, "list")
  expect_length(result1, 1)
  expect_named(result1, "slide1")

  # Test with single dataframe with slideName
  df2 <- data.frame(x = 1:6, y = 7:12, slideName = rep(c("A", "B"), each = 3))
  result2 <- standardize_input_data(df2)
  expect_type(result2, "list")
  expect_length(result2, 2)
  expect_named(result2, c("A", "B"))
  expect_equal(nrow(result2$A), 3)
  expect_equal(nrow(result2$B), 3)

  # Test with list of dataframes
  list_df <- list(slide1 = df1, slide2 = df1)
  result3 <- standardize_input_data(list_df)
  expect_identical(result3, list_df)

  # Test with NULL input
  result4 <- standardize_input_data(NULL)
  expect_type(result4, "list")
  expect_length(result4, 0)

  # Test error case
  expect_error(standardize_input_data("invalid_input"))
})

test_that("marker combinations work correctly", {
  # Create test data
  test_data <- data.frame(
    CellID = 1:10,
    CD8ap = c(rep(TRUE, 5), rep(FALSE, 5)),
    CD4p = c(rep(FALSE, 5), rep(TRUE, 5)),
    Ki67p = rep(c(TRUE, FALSE), 5),
    slideName = "test_slide"
  )

  # Test basic marker combinations
  combos <- list(
    activated_cd8 = ~CD8ap & Ki67p,
    activated_cd4 = ~CD4p & Ki67p
  )

  result <- calculate_marker_combinations(test_data, combos)

  expect_true("activated_cd8" %in% names(result$test_slide))
  expect_true("activated_cd4" %in% names(result$test_slide))

  # Check logical results
  expect_equal(sum(result$test_slide$activated_cd8), 3)  # CD8+ & Ki67+
  expect_equal(sum(result$test_slide$activated_cd4), 2)  # CD4+ & Ki67+
})

test_that("get_default_marker_combinations returns expected structure", {
  defaults <- get_default_marker_combinations()

  expect_type(defaults, "list")
  expect_gt(length(defaults), 0)
  expect_true(all(sapply(defaults, inherits, "formula")))

  # Check for some expected combinations
  expect_true("CD8apCD103n" %in% names(defaults))
  expect_true("IFNP" %in% names(defaults))
})
