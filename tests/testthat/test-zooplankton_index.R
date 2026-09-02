# Test that the dataset is a tibble
test_that("'zooplankton_index' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zooplankton_index))
})

# Test that the dataset has acceptable column names
test_that("'zooplankton_index' has acceptable column names", {
  expect_in(
    colnames(ecodata::zooplankton_index),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
