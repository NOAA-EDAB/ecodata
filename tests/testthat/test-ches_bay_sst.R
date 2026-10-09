# Test that the dataset is a tibble
test_that("'ches_bay_sst' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ches_bay_sst))
})

# Test that the dataset has acceptable column names
test_that("'ches_bay_sst' has acceptable column names", {
  expect_in(
    colnames(ecodata::ches_bay_sst),
    c("Time", "Var", "Value", "Latitude", "Longitude", "Units")
  )
})
