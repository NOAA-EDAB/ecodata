# Test that the dataset is a tibble
test_that("'bottom_temp_glorys' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::bottom_temp_glorys))
})

# Test that the dataset has acceptable column names
test_that("'bottom_temp_glorys' has acceptable column names", {
  expect_in(
    colnames(ecodata::bottom_temp_glorys),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
