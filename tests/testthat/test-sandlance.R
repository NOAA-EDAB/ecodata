# Test that the dataset is a tibble
test_that("'sandlance' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::sandlance))
})

# Test that the dataset has acceptable column names
test_that("'sandlance' has acceptable column names", {
  expect_in(
    colnames(ecodata::sandlance),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
