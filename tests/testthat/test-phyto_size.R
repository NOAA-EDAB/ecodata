# Test that the dataset is a tibble
test_that("'phyto_size' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::phyto_size))
})

# Test that the dataset has acceptable column names
test_that("'phyto_size' has acceptable column names", {
  expect_in(
    colnames(ecodata::phyto_size),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
