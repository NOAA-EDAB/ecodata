# Test that the dataset is a tibble
test_that("'long_line_survey' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::long_line_survey))
})

# Test that the dataset has acceptable column names
test_that("'long_line_survey' has acceptable column names", {
  expect_in(
    colnames(ecodata::long_line_survey),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
