# Test that the dataset is a tibble
test_that("'port_activity' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::port_activity))
})

# Test that the dataset has acceptable column names
test_that("'port_activity' has acceptable column names", {
  expect_in(
    colnames(ecodata::port_activity),
    c("Time", "City", "State", "Var", "Value", "EPU", "Units")
  )
})
