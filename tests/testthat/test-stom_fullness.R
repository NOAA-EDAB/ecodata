# Test that the dataset is a tibble
test_that("'stom_fullness' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::stom_fullness))
})

# Test that the dataset has acceptable column names
test_that("'stom_fullness' has acceptable column names", {
  expect_in(
    colnames(ecodata::stom_fullness),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
