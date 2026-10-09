# Test that the dataset is a tibble
test_that("'grayseal' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::grayseal))
})

# Test that the dataset has acceptable column names
test_that("'grayseal' has acceptable column names", {
  expect_in(
    colnames(ecodata::grayseal),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
