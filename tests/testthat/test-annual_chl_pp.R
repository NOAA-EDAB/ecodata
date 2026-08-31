# Test that the dataset is a tibble
test_that("'annual_chl_pp' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::annual_chl_pp))
})

# Test that the dataset has acceptable column names
test_that("'annual_chl_pp' has acceptable column names", {
  expect_in(
    colnames(ecodata::annual_chl_pp),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
