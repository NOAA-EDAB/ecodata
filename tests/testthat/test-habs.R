# Test that the dataset is a tibble
test_that("'habs' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::habs))
})

# Test that the dataset has acceptable column names
test_that("'habs' has acceptable column names", {
  expect_in(
    colnames(ecodata::habs),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
