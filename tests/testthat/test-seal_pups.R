# Test that the dataset is a tibble
test_that("'seal_pups' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::seal_pups))
})

# Test that the dataset has acceptable column names
test_that("'seal_pups' has acceptable column names", {
  expect_in(
    colnames(ecodata::seal_pups),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
