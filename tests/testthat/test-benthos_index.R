# Test that the dataset is a tibble
test_that("'benthos_index' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::benthos_index))
})

# Test that the dataset has acceptable column names
test_that("'benthos_index' has acceptable column names", {
  expect_in(
    colnames(ecodata::benthos_index),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
