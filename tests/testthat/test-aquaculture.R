# Test that the dataset is a tibble
test_that("'aquaculture' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::aquaculture))
})

# Test that the dataset has acceptable column names
test_that("'aquaculture' has acceptable column names", {
  expect_in(
    colnames(ecodata::aquaculture),
    c("Time", "Var", "Value", "Region", "Units")
  )
})
