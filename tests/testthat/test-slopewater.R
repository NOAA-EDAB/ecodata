# Test that the dataset is a tibble
test_that("'slopewater' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::slopewater))
})

# Test that the dataset has acceptable column names
test_that("'slopewater' has acceptable column names", {
  expect_in(
    colnames(ecodata::slopewater),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
