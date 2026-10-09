# Test that the dataset is a tibble
test_that("'blue_runner' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::blue_runner))
})

# Test that the dataset has acceptable column names
test_that("'blue_runner' has acceptable column names", {
  expect_in(
    colnames(ecodata::blue_runner),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
