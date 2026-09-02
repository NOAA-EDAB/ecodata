# Test that the dataset is a tibble
test_that("'dissolved_oxygen' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::dissolved_oxygen))
})

# Test that the dataset has acceptable column names
test_that("'dissolved_oxygen' has acceptable column names", {
  expect_in(
    colnames(ecodata::dissolved_oxygen),
    c("Time", "Var", "Value", "EPU", "Units", "Latitude", "Longitude")
  )
})
