# Test that the dataset is a tibble
test_that("'gom_salmon' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::gom_salmon))
})

# Test that the dataset has acceptable column names
test_that("'gom_salmon' has acceptable column names", {
  expect_in(
    colnames(ecodata::gom_salmon),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
