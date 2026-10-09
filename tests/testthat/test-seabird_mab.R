# Test that the dataset is a tibble
test_that("'seabird_mab' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::seabird_mab))
})

# Test that the dataset has acceptable column names
test_that("'seabird_mab' has acceptable column names", {
  expect_in(
    colnames(ecodata::seabird_mab),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
