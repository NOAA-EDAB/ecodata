# Test that the plot function has working default arguments
test_that("'plot_observed_sharks' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_observed_sharks()))
})

# Test that the plot function creates an object
test_that("'plot_observed_sharks' creates an object", {
  expect_type(invisible(ecodata::plot_observed_sharks(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_observed_sharks' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_observed_sharks)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_observed_sharks)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_observed_sharks' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "observed_sharks",
    print = FALSE
  ))
})
