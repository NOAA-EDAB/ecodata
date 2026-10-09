# Test that the dataset is a tibble
test_that("'calanus_stage' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::calanus_stage))
})

# Test that the dataset has acceptable column names
test_that("'calanus_stage' has acceptable column names", {
  expect_in(
    colnames(ecodata::calanus_stage),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
