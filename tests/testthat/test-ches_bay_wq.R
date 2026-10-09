# Test that the dataset is a tibble
test_that("'ches_bay_wq' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ches_bay_wq))
})

# Test that the dataset has acceptable column names
test_that("'ches_bay_wq' has acceptable column names", {
  expect_in(
    colnames(ecodata::ches_bay_wq),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
