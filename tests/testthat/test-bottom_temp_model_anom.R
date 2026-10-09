# Test that the dataset is a tibble
test_that("'bottom_temp_model_anom' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::bottom_temp_model_anom))
})

# Test that the dataset has acceptable column names
test_that("'bottom_temp_model_anom' has acceptable column names", {
  expect_in(
    colnames(ecodata::bottom_temp_model_anom),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
