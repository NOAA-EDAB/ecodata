# Test that the dataset is a tibble
test_that("'thermal_habitat_area_annual' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::thermal_habitat_area_annual))
})

# Test that the dataset has acceptable column names
test_that("'thermal_habitat_area_annual' has acceptable column names", {
  expect_in(
    colnames(ecodata::thermal_habitat_area_annual),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
