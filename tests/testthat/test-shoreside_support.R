# Test that the dataset is a tibble
test_that("'shoreside_support' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::shoreside_support))
})

# Test that the dataset has acceptable column names
test_that("'shoreside_support' has acceptable column names", {
  expect_in(
    colnames(ecodata::shoreside_support),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
