# Test that the dataset is a tibble
test_that("'surface_temp_mom6' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::surface_temp_mom6))
})

# Test that the dataset has acceptable column names
test_that("'surface_temp_mom6' has acceptable column names", {
  expect_in(
    colnames(ecodata::surface_temp_mom6),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
