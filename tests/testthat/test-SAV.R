# Test that the dataset is a tibble
test_that("'SAV' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::SAV))
})

# Test that the dataset has acceptable column names
test_that("'SAV' has acceptable column names", {
  expect_in(
    colnames(ecodata::SAV),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
