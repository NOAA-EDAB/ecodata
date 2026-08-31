# Test that the dataset is a tibble
test_that("'forage_anomaly' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::forage_anomaly))
})

# Test that the dataset has acceptable column names
test_that("'forage_anomaly' has acceptable column names", {
  expect_in(
    colnames(ecodata::forage_anomaly),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
