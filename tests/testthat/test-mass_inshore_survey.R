# Test that the dataset is a tibble
test_that("'mass_inshore_survey' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::mass_inshore_survey))
})

# Test that the dataset has acceptable column names
test_that("'mass_inshore_survey' has acceptable column names", {
  expect_in(
    colnames(ecodata::mass_inshore_survey),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
