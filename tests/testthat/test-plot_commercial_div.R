# Test that the plot function has working default arguments
test_that("'plot_commercial_div' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_commercial_div()))
})

# Test that the plot function creates an object
test_that("'plot_commercial_div' creates an object", {
  expect_type(invisible(ecodata::plot_commercial_div()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_commercial_div' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_commercial_div)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_commercial_div)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_commercial_div' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "commercial_div",
    print = FALSE
  ))
})
