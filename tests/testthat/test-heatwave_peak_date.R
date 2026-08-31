# Test that the dataset is a tibble
test_that("'heatwave_peak_date' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::heatwave_peak_date))
})

# Test that the dataset has acceptable column names
test_that("'heatwave_peak_date' has acceptable column names", {
  expect_in(
    colnames(ecodata::heatwave_peak_date),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
