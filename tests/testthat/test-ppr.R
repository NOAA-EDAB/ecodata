# Test that the dataset is a tibble
test_that("'ppr' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ppr))
})

# Test that the dataset has acceptable column names
test_that("'ppr' has acceptable column names", {
  expect_in(
    colnames(ecodata::ppr),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
