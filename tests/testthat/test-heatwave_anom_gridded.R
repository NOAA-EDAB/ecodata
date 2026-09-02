# Test that the dataset is a tibble
test_that("'heatwave_anom_gridded' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::heatwave_anom_gridded))
})

# Test that the dataset has acceptable column names
test_that("'heatwave_anom_gridded' has acceptable column names", {
  expect_in(
    colnames(ecodata::heatwave_anom_gridded),
    c("Time", "Var", "Value", "EPU", "Units", "Latitude", "Longitude")
  )
})
