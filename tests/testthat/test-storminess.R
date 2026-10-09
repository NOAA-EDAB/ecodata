# Test that the dataset is a tibble
test_that("'storminess' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::storminess))
})

# Test that the dataset has acceptable column names
test_that("'storminess' has acceptable column names", {
  expect_in(
    colnames(ecodata::storminess),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
