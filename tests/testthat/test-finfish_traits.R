# Test that the dataset is a tibble
test_that("'finfish_traits' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::finfish_traits))
})

# Test that the dataset has acceptable column names
test_that("'finfish_traits' has acceptable column names", {
  expect_in(
    colnames(ecodata::finfish_traits),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
