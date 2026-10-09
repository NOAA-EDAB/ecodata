# Test that the dataset is a tibble
test_that("'hms_landings' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hms_landings))
})

# Test that the dataset has acceptable column names
test_that("'hms_landings' has acceptable column names", {
  expect_in(
    colnames(ecodata::hms_landings),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
