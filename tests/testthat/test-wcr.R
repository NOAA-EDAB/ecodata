# Test that the dataset is a tibble
test_that("'wcr' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wcr))
})

# Test that the dataset has acceptable column names
test_that("'wcr' has acceptable column names", {
  expect_in(
    colnames(ecodata::wcr),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
