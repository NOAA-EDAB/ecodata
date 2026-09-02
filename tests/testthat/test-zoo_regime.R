# Test that the dataset is a tibble
test_that("'zoo_regime' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zoo_regime))
})

# Test that the dataset has acceptable column names
test_that("'zoo_regime' has acceptable column names", {
  expect_in(
    colnames(ecodata::zoo_regime),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
