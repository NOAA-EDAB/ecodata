# Test that the plot function has working default arguments
test_that("'plot_wbts_zoo' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_wbts_zoo()))
})

# Test that the plot function creates an object
test_that("'plot_wbts_zoo' creates an object", {
  expect_type(invisible(ecodata::plot_wbts_zoo()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_wbts_zoo' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_wbts_zoo)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_wbts_zoo)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_wbts_zoo' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "wbts_zoo",
    print = FALSE
  ))
})
