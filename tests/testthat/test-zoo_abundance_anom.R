# Test that the dataset is a tibble
test_that("'zoo_abundance_anom' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::zoo_abundance_anom))
})

# Test that the dataset has acceptable column names
test_that("'zoo_abundance_anom' has acceptable column names", {
  expect_in(
    colnames(ecodata::zoo_abundance_anom),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
