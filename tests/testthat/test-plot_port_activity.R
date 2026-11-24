# Test that the plot function has working default arguments
test_that("'plot_port_activity' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_port_activity()))
})

# Test that the plot function creates an object
test_that("'plot_port_activity' creates an object", {
  expect_type(invisible(ecodata::plot_port_activity()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_port_activity' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_port_activity)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_port_activity)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_port_activity' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "port_activity",
    print = FALSE
  ))
})
