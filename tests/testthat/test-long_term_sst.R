# Test that the dataset is a tibble
test_that("'long_term_sst' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::long_term_sst))
})

# Test that the dataset has acceptable column names
test_that("'long_term_sst' has acceptable column names", {
  expect_in(
    colnames(ecodata::long_term_sst),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
