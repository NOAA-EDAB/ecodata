# Test that the plot function has working default arguments
test_that("'plot_bottom_temp_model_gridded' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_bottom_temp_model_gridded()))
})

# Test that the plot function creates an object
test_that("'plot_bottom_temp_model_gridded' creates an object", {
  expect_type(invisible(ecodata::plot_bottom_temp_model_gridded(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_bottom_temp_model_gridded' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_bottom_temp_model_gridded)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_bottom_temp_model_gridded)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_bottom_temp_model_gridded' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "bottom_temp_model_gridded",
    print = FALSE
  ))
})
