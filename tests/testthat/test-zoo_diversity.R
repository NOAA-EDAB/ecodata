# Test that the dataset is a tibble
test_that("'zoo_diversity' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zoo_diversity))
})

# Test that the dataset has acceptable column names
test_that("'zoo_diversity' has acceptable column names", {
  expect_in(
    colnames(ecodata::zoo_diversity),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
