# Test that the dataset is a tibble
test_that("'preyfield_energy' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::preyfield_energy))
})

# Test that the dataset has acceptable column names
test_that("'preyfield_energy' has acceptable column names", {
  expect_in(
    colnames(ecodata::preyfield_energy),
    c("Time", "Var", "Value", "EPU", "Units", "Latitude", "Longitude")
  )
})
