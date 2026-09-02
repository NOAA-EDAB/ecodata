# Test that the dataset is a tibble
test_that("'wind_revenue' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wind_revenue))
})

# Test that the dataset has acceptable column names
test_that("'wind_revenue' has acceptable column names", {
  expect_in(
    colnames(ecodata::wind_revenue),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
