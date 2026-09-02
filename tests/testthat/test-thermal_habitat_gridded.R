# Test that the dataset is a tibble
test_that("'thermal_habitat_gridded' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::thermal_habitat_gridded))
})

# Test that the dataset has acceptable column names
test_that("'thermal_habitat_gridded' has acceptable column names", {
  expect_in(
    colnames(ecodata::thermal_habitat_gridded),
    c("Time", "Var", "Value", "EPU", "Units", "Longitude", "Latitude")
  )
})
