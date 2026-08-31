# Test that the plot function has working default arguments
test_that("'plot_comdat_profit' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_comdat_profit()))
})

# Test that the plot function creates an object
test_that("'plot_comdat_profit' creates an object", {
  expect_type(invisible(ecodata::plot_comdat_profit(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_comdat_profit' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_comdat_profit)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_comdat_profit)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_comdat_profit' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "comdat_profit",
    print = FALSE
  ))
})
