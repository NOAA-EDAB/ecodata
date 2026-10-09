# Test that the dataset is a tibble
test_that("'thermal_habitat_area' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::thermal_habitat_area))
})

# Test that the dataset has acceptable column names
test_that("'thermal_habitat_area' has acceptable column names", {
  expect_in(
    colnames(ecodata::thermal_habitat_area),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
