# Test that the dataset is a tibble
test_that("'bottom_temp_model_gridded' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::bottom_temp_model_gridded))
})

# Test that the dataset has acceptable column names
test_that("'bottom_temp_model_gridded' has acceptable column names", {
  expect_in(
    colnames(ecodata::bottom_temp_model_gridded),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
