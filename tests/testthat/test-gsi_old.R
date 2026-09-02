# Test that the dataset is a tibble
test_that("'gsi_old' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::gsi_old))
})

# Test that the dataset has acceptable column names
test_that("'gsi_old' has acceptable column names", {
  expect_in(
    colnames(ecodata::gsi_old),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
