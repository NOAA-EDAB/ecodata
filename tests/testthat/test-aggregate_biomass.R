# Test that the dataset is a tibble
test_that("'aggregate_biomass' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::aggregate_biomass))
})

# Test that the dataset has acceptable column names
test_that("'aggregate_biomass' has acceptable column names", {
  expect_in(
    colnames(ecodata::aggregate_biomass),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
