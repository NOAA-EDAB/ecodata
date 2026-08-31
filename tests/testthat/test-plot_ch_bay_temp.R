# Test that the plot function has working default arguments
test_that("'plot_ch_bay_temp' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_ch_bay_temp()))
})

# Test that the plot function creates an object
test_that("'plot_ch_bay_temp' creates an object", {
  expect_type(invisible(ecodata::plot_ch_bay_temp()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_ch_bay_temp' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_ch_bay_temp)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_ch_bay_temp)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_ch_bay_temp' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "ch_bay_temp",
    print = FALSE
  ))
})
