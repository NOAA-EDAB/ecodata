# Test that the dataset is a tibble
test_that("'commercial_div' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::commercial_div))
})

# Test that the dataset has acceptable column names
test_that("'commercial_div' has acceptable column names", {
  expect_in(
    colnames(ecodata::commercial_div),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
