# Test that the dataset is a tibble
test_that("'crew_survey' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::crew_survey))
})

# Test that the dataset has acceptable column names
test_that("'crew_survey' has acceptable column names", {
  expect_in(
    colnames(ecodata::crew_survey),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
