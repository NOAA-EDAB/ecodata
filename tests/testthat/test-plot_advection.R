# Test that the plot function has working default arguments
test_that("'plot_advection' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_advection()))
})

# Test that the plot function creates an object
test_that("'plot_advection' creates an object", {
  expect_type(invisible(ecodata::plot_advection(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_advection' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_advection)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_advection)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_advection' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "advection",
    print = FALSE
  ))
})
