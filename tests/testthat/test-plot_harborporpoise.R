# Test that the plot function has working default arguments
test_that("'plot_harborporpoise' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_harborporpoise()))
})

# Test that the plot function creates an object
test_that("'plot_harborporpoise' creates an object", {
  expect_type(invisible(ecodata::plot_harborporpoise()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_harborporpoise' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_harborporpoise)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_harborporpoise)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_harborporpoise' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "harborporpoise",
    print = FALSE
  ))
})
