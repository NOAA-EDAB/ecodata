# Test that the dataset is a tibble
test_that("'zoo_community' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zoo_community))
})

# Test that the dataset has acceptable column names
test_that("'zoo_community' has acceptable column names", {
  expect_in(
    colnames(ecodata::zoo_community),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
