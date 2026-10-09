# Test that the dataset is a tibble
test_that("'effective_sweptarea' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::effective_sweptarea))
})

# Test that the dataset has acceptable column names
test_that("'effective_sweptarea' has acceptable column names", {
  expect_in(
    colnames(ecodata::effective_sweptarea),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
