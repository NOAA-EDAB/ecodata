# Test that the dataset is a tibble
test_that("'recdat' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::recdat))
})

# Test that the dataset has acceptable column names
test_that("'recdat' has acceptable column names", {
  expect_in(
    colnames(ecodata::recdat),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
