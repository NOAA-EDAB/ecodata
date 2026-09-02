# Test that the dataset is a tibble
test_that("'heatwave_year' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::heatwave_year))
})

# Test that the dataset has acceptable column names
test_that("'heatwave_year' has acceptable column names", {
  expect_in(
    colnames(ecodata::heatwave_year),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
