# Test that the dataset is a tibble
test_that("'wbts_zoo' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wbts_zoo))
})

# Test that the dataset has acceptable column names
test_that("'wbts_zoo' has acceptable column names", {
  expect_in(
    colnames(ecodata::wbts_zoo),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
