# Test that the dataset is a tibble
test_that("'comdat_profit' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::comdat_profit))
})

# Test that the dataset has acceptable column names
test_that("'comdat_profit' has acceptable column names", {
  expect_in(
    colnames(ecodata::comdat_profit),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
