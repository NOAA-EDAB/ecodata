# Test that the dataset is a tibble
test_that("'hp_density' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hp_density))
})

# Test that the dataset has acceptable column names
test_that("'hp_density' has acceptable column names", {
  expect_in(
    colnames(ecodata::hp_density),
    c("Time", "Var", "Value", "EPU", "Units", "Latitude", "Longitude")
  )
})
