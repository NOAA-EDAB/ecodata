# Test that the plot function has working default arguments
test_that("'plot_seasonal_oisst_anom' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_seasonal_oisst_anom()))
})

# Test that the plot function creates an object
test_that("'plot_seasonal_oisst_anom' creates an object", {
  expect_type(invisible(ecodata::plot_seasonal_oisst_anom()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_seasonal_oisst_anom' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_seasonal_oisst_anom)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_seasonal_oisst_anom)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_seasonal_oisst_anom' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "seasonal_oisst_anom",
    print = FALSE
  ))
})
