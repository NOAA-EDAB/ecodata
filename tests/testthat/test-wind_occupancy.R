# Test that the dataset is a tibble
test_that("'wind_occupancy' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wind_occupancy))
})

# Test that the dataset has acceptable column names
test_that("'wind_occupancy' has acceptable column names", {
  expect_in(
    colnames(ecodata::wind_occupancy),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
