# Test that the dataset is a tibble
test_that("'stock_status' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::stock_status))
})

# Test that the dataset has acceptable column names
test_that("'stock_status' has acceptable column names", {
  expect_in(
    colnames(ecodata::stock_status),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
