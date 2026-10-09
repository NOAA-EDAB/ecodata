# Test that the dataset is a tibble
test_that("'cetacean_dist' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::cetacean_dist))
})

# Test that the dataset has acceptable column names
test_that("'cetacean_dist' has acceptable column names", {
  expect_in(
    colnames(ecodata::cetacean_dist),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
