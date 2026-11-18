# Test that the dataset is a tibble
test_that("'thermal_suitability' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::thermal_suitability))
})

# Test that the dataset has acceptable column names
test_that("'thermal_suitability' has acceptable column names", {
  expect_in(
    colnames(ecodata::thermal_suitability),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})

# Test that the dataset contains both Council reports and no missing values
test_that("'thermal_suitability' has both reports and no missing values", {
  expect_setequal(unique(ecodata::thermal_suitability$EPU), c("NE", "MA"))
  expect_false(any(is.na(ecodata::thermal_suitability$Value)))
})