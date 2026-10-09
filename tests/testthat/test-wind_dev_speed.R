# Test that the dataset is a tibble
test_that("'wind_dev_speed' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wind_dev_speed))
})

# Test that the dataset has acceptable column names
test_that("'wind_dev_speed' has acceptable column names", {
  expect_in(
    colnames(ecodata::wind_dev_speed),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
