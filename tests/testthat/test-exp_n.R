# Test that the dataset is a tibble
test_that("'exp_n' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::exp_n))
})

# Test that the dataset has acceptable column names
test_that("'exp_n' has acceptable column names", {
  expect_in(
    colnames(ecodata::exp_n),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
