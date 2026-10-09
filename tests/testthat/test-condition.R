# Test that the dataset is a tibble
test_that("'condition' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::condition))
})

# Test that the dataset has acceptable column names
test_that("'condition' has acceptable column names", {
  expect_in(
    colnames(ecodata::condition),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
