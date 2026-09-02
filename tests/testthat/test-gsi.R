# Test that the dataset is a tibble
test_that("'gsi' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::gsi))
})

# Test that the dataset has acceptable column names
test_that("'gsi' has acceptable column names", {
  expect_in(
    colnames(ecodata::gsi),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
