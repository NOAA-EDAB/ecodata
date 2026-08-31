# Test that the dataset is a tibble
test_that("'community_risks' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::community_risks))
})

# Test that the dataset has acceptable column names
test_that("'community_risks' has acceptable column names", {
  expect_in(
    colnames(ecodata::community_risks),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
