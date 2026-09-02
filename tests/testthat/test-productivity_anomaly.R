# Test that the dataset is a tibble
test_that("'productivity_anomaly' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::productivity_anomaly))
})

# Test that the dataset has acceptable column names
test_that("'productivity_anomaly' has acceptable column names", {
  expect_in(
    colnames(ecodata::productivity_anomaly),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
