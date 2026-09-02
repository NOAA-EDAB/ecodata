# Test that the dataset is a tibble
test_that("'ch_bay_sal' is a tibble", {
  expect_true(tibble::is_tibble(ecodata::ch_bay_sal))
})

# Test that the dataset has acceptable column names
test_that("'ch_bay_sal' has acceptable column names", {
  expect_in(
    colnames(ecodata::ch_bay_sal),
    c("Time", "Var", "Value", "EPU", "Units")
  )
})
