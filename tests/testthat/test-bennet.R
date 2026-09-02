# Test that the dataset is a tibble
test_that("'bennet' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::bennet))
})

# Test that the dataset has acceptable column names
test_that("'bennet' has acceptable column names", {
  expect_in(
    colnames(ecodata::bennet),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
