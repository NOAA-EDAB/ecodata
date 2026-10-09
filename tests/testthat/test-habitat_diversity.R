# Test that the dataset is a tibble
test_that("'habitat_diversity' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::habitat_diversity))
})

# Test that the dataset has acceptable column names
test_that("'habitat_diversity' has acceptable column names", {
  expect_in(
    colnames(ecodata::habitat_diversity),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
