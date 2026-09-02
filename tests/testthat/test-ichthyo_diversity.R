# Test that the dataset is a tibble
test_that("'ichthyo_diversity' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ichthyo_diversity))
})

# Test that the dataset has acceptable column names
test_that("'ichthyo_diversity' has acceptable column names", {
  expect_in(
    colnames(ecodata::ichthyo_diversity),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
