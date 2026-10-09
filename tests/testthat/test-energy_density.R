# Test that the dataset is a tibble
test_that("'energy_density' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::energy_density))
})

# Test that the dataset has acceptable column names
test_that("'energy_density' has acceptable column names", {
  expect_in(
    colnames(ecodata::energy_density),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
