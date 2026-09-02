# Test that the dataset is a tibble
test_that("'trans_dates' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::trans_dates))
})

# Test that the dataset has acceptable column names
test_that("'trans_dates' has acceptable column names", {
  expect_in(
    colnames(ecodata::trans_dates),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
