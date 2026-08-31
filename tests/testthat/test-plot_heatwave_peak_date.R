# Test that the plot function has working default arguments
test_that("'plot_heatwave_peak_date' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_heatwave_peak_date()))
})

# Test that the plot function creates an object
test_that("'plot_heatwave_peak_date' creates an object", {
  expect_type(invisible(ecodata::plot_heatwave_peak_date(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_heatwave_peak_date' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_heatwave_peak_date)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_heatwave_peak_date)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_heatwave_peak_date' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "heatwave_peak_date",
    print = FALSE
  ))
})
