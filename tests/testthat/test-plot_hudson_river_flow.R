# Test that the plot function has working default arguments
test_that("'plot_hudson_river_flow' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_hudson_river_flow()))
})

# Test that the plot function creates an object
test_that("'plot_hudson_river_flow' creates an object", {
  expect_type(invisible(ecodata::plot_hudson_river_flow()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_hudson_river_flow' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_hudson_river_flow)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_hudson_river_flow)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_hudson_river_flow' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "hudson_river_flow",
    print = FALSE
  ))
})
