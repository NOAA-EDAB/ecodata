# Test that the plot function has working default arguments
test_that("'plot_gsi' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_gsi()))
})

# Test that the plot function creates an object
test_that("'plot_gsi' creates an object", {
  expect_type(invisible(ecodata::plot_gsi(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_gsi' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_gsi)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_gsi)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_gsi' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "gsi",
    print = FALSE
  ))
})
