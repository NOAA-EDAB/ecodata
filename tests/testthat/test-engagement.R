# Test that the dataset is a tibble
test_that("'engagement' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::engagement))
})

# Test that the dataset has acceptable column names
test_that("'engagement' has acceptable column names", {
  expect_in(
    colnames(ecodata::engagement),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
