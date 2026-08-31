# Test that the plot function has working default arguments
test_that("'plot_storminess' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_storminess()))
})

# Test that the plot function creates an object
test_that("'plot_storminess' creates an object", {
  expect_type(invisible(ecodata::plot_storminess()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_storminess' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_storminess)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_storminess)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_storminess' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "storminess",
    print = FALSE
  ))
})
