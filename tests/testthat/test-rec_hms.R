# Test that the dataset is a tibble
test_that("'rec_hms' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::rec_hms))
})

# Test that the dataset has acceptable column names
test_that("'rec_hms' has acceptable column names", {
  expect_in(
    colnames(ecodata::rec_hms),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
