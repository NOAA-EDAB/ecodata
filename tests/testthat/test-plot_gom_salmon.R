# Test that the plot function has working default arguments
test_that("'plot_gom_salmon' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_gom_salmon()))
})

# Test that the plot function creates an object
test_that("'plot_gom_salmon' creates an object", {
  expect_type(invisible(ecodata::plot_gom_salmon()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_gom_salmon' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_gom_salmon)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_gom_salmon)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_gom_salmon' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "gom_salmon",
    print = FALSE
  ))
})
