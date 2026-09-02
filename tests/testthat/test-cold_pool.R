# Test that the dataset is a tibble
test_that("'cold_pool' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::cold_pool))
})

# Test that the dataset has acceptable column names
test_that("'cold_pool' has acceptable column names", {
  expect_in(
    colnames(ecodata::cold_pool),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
