# Test that the plot function has working default arguments
test_that("'plot_stock_status' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_stock_status()))
})

# Test that the plot function creates an object
test_that("'plot_stock_status' creates an object", {
  expect_type(invisible(ecodata::plot_stock_status(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_stock_status' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_stock_status)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_stock_status)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_stock_status' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "stock_status",
    print = FALSE
  ))
})
