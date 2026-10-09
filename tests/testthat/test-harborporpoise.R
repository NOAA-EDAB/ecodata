# Test that the dataset is a tibble
test_that("'harborporpoise' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::harborporpoise))
})

# Test that the dataset has acceptable column names
test_that("'harborporpoise' has acceptable column names", {
  expect_in(
    colnames(ecodata::harborporpoise),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
