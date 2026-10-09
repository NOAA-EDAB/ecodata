# Test that the dataset is a tibble
test_that("'hms_category' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hms_category))
})

# Test that the dataset has acceptable column names
test_that("'hms_category' has acceptable column names", {
  expect_in(
    colnames(ecodata::hms_category),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
