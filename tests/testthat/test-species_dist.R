# Test that the dataset is a tibble
test_that("'species_dist' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::species_dist))
})

# Test that the dataset has acceptable column names
test_that("'species_dist' has acceptable column names", {
  expect_in(
    colnames(ecodata::species_dist),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
