# Test that the dataset is a tibble
test_that("'lps_sharks' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::lps_sharks))
})

# Test that the dataset has acceptable column names
test_that("'lps_sharks' has acceptable column names", {
  expect_in(
    colnames(ecodata::lps_sharks),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
