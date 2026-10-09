# Test that the dataset is a tibble
test_that("'ne_inshore_survey_species' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ne_inshore_survey_species))
})

# Test that the dataset has acceptable column names
test_that("'ne_inshore_survey_species' has acceptable column names", {
  expect_in(
    colnames(ecodata::ne_inshore_survey_species),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
