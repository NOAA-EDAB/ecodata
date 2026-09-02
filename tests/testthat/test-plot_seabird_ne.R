# Test that the plot function has working default arguments
test_that("'plot_seabird_ne' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_seabird_ne()))
})

# Test that the plot function creates an object
test_that("'plot_seabird_ne' creates an object", {
  expect_type(invisible(ecodata::plot_seabird_ne()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_seabird_ne' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_seabird_ne)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_seabird_ne)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_seabird_ne' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "seabird_ne",
    print = FALSE
  ))
})
