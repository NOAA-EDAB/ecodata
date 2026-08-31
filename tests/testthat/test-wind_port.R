# Test that the dataset is a tibble
test_that("'wind_port' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wind_port))
})

# Test that the dataset has acceptable column names
test_that("'wind_port' has acceptable column names", {
  expect_in(
    colnames(ecodata::wind_port),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
