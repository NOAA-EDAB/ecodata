# Test that the dataset is a tibble
test_that("'wea_sound' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::wea_sound))
})

# Test that the dataset has acceptable column names
test_that("'wea_sound' has acceptable column names", {
  expect_in(
    colnames(ecodata::wea_sound),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
