# Test that the dataset is a tibble
test_that("'wea_landings_rev' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wea_landings_rev))
})

# Test that the dataset has acceptable column names
test_that("'wea_landings_rev' has acceptable column names", {
  expect_in(
    colnames(ecodata::wea_landings_rev),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
