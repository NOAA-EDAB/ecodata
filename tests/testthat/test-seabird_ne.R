# Test that the dataset is a tibble
test_that("'seabird_ne' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::seabird_ne))
})

# Test that the dataset has acceptable column names
test_that("'seabird_ne' has acceptable column names", {
  expect_in(
    colnames(ecodata::seabird_ne),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
