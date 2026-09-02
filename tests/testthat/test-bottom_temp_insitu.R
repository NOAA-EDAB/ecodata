# Test that the dataset is a tibble
test_that("'bottom_temp_insitu' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::bottom_temp_insitu))
})

# Test that the dataset has acceptable column names
test_that("'bottom_temp_insitu' has acceptable column names", {
  expect_in(
    colnames(ecodata::bottom_temp_insitu),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
