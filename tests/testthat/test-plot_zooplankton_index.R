# Test that the plot function has working default arguments
test_that("'plot_zooplankton_index' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_zooplankton_index()))
})

# Test that the plot function creates an object
test_that("'plot_zooplankton_index' creates an object", {
  expect_type(invisible(ecodata::plot_zooplankton_index()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_zooplankton_index' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_zooplankton_index)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_zooplankton_index)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_zooplankton_index' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "zooplankton_index",
    print = FALSE
  ))
})
