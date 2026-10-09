# Test that the dataset is a tibble
test_that("'species_groupings' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::species_groupings))
})

# Test that the dataset has acceptable column names
test_that("'species_groupings' has acceptable column names", {
  expect_in(
    colnames(ecodata::species_groupings),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
