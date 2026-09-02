# Test that the dataset is a tibble
test_that("'spawn_timing' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::spawn_timing))
})

# Test that the dataset has acceptable column names
test_that("'spawn_timing' has acceptable column names", {
  expect_in(
    colnames(ecodata::spawn_timing),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
