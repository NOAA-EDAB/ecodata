# Test that the plot function has working default arguments
test_that("'plot_long_term_sst' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_long_term_sst()))
})

# Test that the plot function creates an object
test_that("'plot_long_term_sst' creates an object", {
  expect_type(invisible(ecodata::plot_long_term_sst()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_long_term_sst' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_long_term_sst)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_long_term_sst)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_long_term_sst' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "long_term_sst",
    print = FALSE
  ))
})
