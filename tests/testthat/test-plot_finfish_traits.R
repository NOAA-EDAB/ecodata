# Test that the plot function has working default arguments
test_that("'plot_finfish_traits' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_finfish_traits()))
})

# Test that the plot function creates an object
test_that("'plot_finfish_traits' creates an object", {
  expect_type(invisible(ecodata::plot_finfish_traits()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_finfish_traits' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_finfish_traits)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_finfish_traits)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_finfish_traits' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "finfish_traits",
    print = FALSE
  ))
})
