# Test that the plot function has working default arguments
test_that("'plot_recdat' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_recdat()))
})

# Test that the plot function creates an object
test_that("'plot_recdat' creates an object", {
  expect_type(invisible(ecodata::plot_recdat()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_recdat' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_recdat)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_recdat)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_recdat' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "recdat",
    print = FALSE
  ))
})
