# Test that the dataset is a tibble
test_that("'abc_acl' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::abc_acl))
})

# Test that the dataset has acceptable column names
test_that("'abc_acl' has acceptable column names", {
  expect_in(
    colnames(ecodata::abc_acl),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
