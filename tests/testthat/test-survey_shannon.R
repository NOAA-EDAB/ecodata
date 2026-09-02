# Test that the dataset is a tibble
test_that("'survey_shannon' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::survey_shannon))
})

# Test that the dataset has acceptable column names
test_that("'survey_shannon' has acceptable column names", {
  expect_in(
    colnames(ecodata::survey_shannon),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
