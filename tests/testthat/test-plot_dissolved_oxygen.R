# Test that the plot function has working default arguments
test_that("'plot_dissolved_oxygen' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_dissolved_oxygen()))
})

# Test that the plot function creates an object
test_that("'plot_dissolved_oxygen' creates an object", {
  expect_type(invisible(ecodata::plot_dissolved_oxygen()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_dissolved_oxygen' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_dissolved_oxygen)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_dissolved_oxygen)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_dissolved_oxygen' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "dissolved_oxygen",
    print = FALSE
  ))
})
