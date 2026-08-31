# Test that the dataset is a tibble
test_that("'hms_stock_status' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hms_stock_status))
})

# Test that the dataset has acceptable column names
test_that("'hms_stock_status' has acceptable column names", {
  expect_in(
    colnames(ecodata::hms_stock_status),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
