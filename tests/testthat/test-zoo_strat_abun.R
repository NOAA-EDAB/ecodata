# Test that the dataset is a tibble
test_that("'zoo_strat_abun' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zoo_strat_abun))
})

# Test that the dataset has acceptable column names
test_that("'zoo_strat_abun' has acceptable column names", {
  expect_in(
    colnames(ecodata::zoo_strat_abun),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
