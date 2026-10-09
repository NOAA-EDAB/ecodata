# Test that the dataset is a tibble
test_that("'hms_cpue' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hms_cpue))
})

# Test that the dataset has acceptable column names
test_that("'hms_cpue' has acceptable column names", {
  expect_in(
    colnames(ecodata::hms_cpue),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
