# Test that the dataset is a tibble
test_that("'habitat_occupancy' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::habitat_occupancy))
})

# Test that the dataset has acceptable column names
test_that("'habitat_occupancy' has acceptable column names", {
  expect_in(
    colnames(ecodata::habitat_occupancy),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
