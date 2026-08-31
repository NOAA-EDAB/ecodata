# Test that the plot function has working default arguments
test_that("'plot_wind_revenue' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_wind_revenue()))
})

# Test that the plot function creates an object
test_that("'plot_wind_revenue' creates an object", {
  expect_type(invisible(ecodata::plot_wind_revenue(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_wind_revenue' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_wind_revenue)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_wind_revenue)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_wind_revenue' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "wind_revenue",
    print = FALSE
  ))
})
