# Test that the plot function has working default arguments
test_that("'plot_trans_dates' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_trans_dates()))
})

# Test that the plot function creates an object
test_that("'plot_trans_dates' creates an object", {
  expect_type(invisible(ecodata::plot_trans_dates()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_trans_dates' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_trans_dates)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_trans_dates)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_trans_dates' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "trans_dates",
    print = FALSE
  ))
})
