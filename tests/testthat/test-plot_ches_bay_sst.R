# Test that the plot function has working default arguments
test_that("'plot_ches_bay_sst' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_ches_bay_sst()))
})

# Test that the plot function creates an object
test_that("'plot_ches_bay_sst' creates an object", {
  expect_type(invisible(ecodata::plot_ches_bay_sst()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_ches_bay_sst' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_ches_bay_sst)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_ches_bay_sst)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_ches_bay_sst' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "ches_bay_sst",
    print = FALSE
  ))
})
