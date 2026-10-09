# Test that the dataset is a tibble
test_that("'advection' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::advection))
})

# Test that the dataset has acceptable column names
test_that("'advection' has acceptable column names", {
  expect_in(
    colnames(ecodata::advection),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
