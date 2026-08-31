# Test that the dataset is a tibble
test_that("'cetacean_acoustic' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::cetacean_acoustic))
})

# Test that the dataset has acceptable column names
test_that("'cetacean_acoustic' has acceptable column names", {
  expect_in(
    colnames(ecodata::cetacean_acoustic),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
