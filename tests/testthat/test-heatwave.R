# Test that the dataset is a tibble
test_that("'heatwave' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::heatwave))
})

# Test that the dataset has acceptable column names
test_that("'heatwave' has acceptable column names", {
  expect_in(
    colnames(ecodata::heatwave),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
