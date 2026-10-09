# Test that the dataset is a tibble
test_that("'comdat' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::comdat))
})

# Test that the dataset has acceptable column names
test_that("'comdat' has acceptable column names", {
  expect_in(
    colnames(ecodata::comdat),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
