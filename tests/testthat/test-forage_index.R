# Test that the dataset is a tibble
test_that("'forage_index' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::forage_index))
})

# Test that the dataset has acceptable column names
test_that("'forage_index' has acceptable column names", {
  expect_in(
    colnames(ecodata::forage_index),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
