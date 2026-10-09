# Test that the dataset is a tibble
test_that("'narw' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::narw))
})

# Test that the dataset has acceptable column names
test_that("'narw' has acceptable column names", {
  expect_in(
    colnames(ecodata::narw),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
