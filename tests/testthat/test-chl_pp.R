# Test that the dataset is a tibble
test_that("'chl_pp' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::chl_pp))
})

# Test that the dataset has acceptable column names
test_that("'chl_pp' has acceptable column names", {
  expect_in(
    colnames(ecodata::chl_pp),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
