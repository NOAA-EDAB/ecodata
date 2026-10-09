# Test that the dataset is a tibble
test_that("'hudson_river_flow' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::hudson_river_flow))
})

# Test that the dataset has acceptable column names
test_that("'hudson_river_flow' has acceptable column names", {
  expect_in(
    colnames(ecodata::hudson_river_flow),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
