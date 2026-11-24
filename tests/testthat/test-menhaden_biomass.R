# Test that the dataset is a tibble
test_that("'menhaden_biomass' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::menhaden_biomass))
})

# Test that the dataset has acceptable column names
test_that("'menhaden_biomass' has acceptable column names", {
  expect_in(
    colnames(ecodata::menhaden_biomass),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
