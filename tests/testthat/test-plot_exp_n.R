# Test that the plot function has working default arguments
test_that("'plot_exp_n' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_exp_n()))
})

# Test that the plot function creates an object
test_that("'plot_exp_n' creates an object", {
  expect_type(invisible(ecodata::plot_exp_n()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_exp_n' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_exp_n)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_exp_n)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_exp_n' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "exp_n",
    print = FALSE
  ))
})
