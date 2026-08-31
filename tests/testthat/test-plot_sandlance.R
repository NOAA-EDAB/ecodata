# Test that the plot function has working default arguments
test_that("'plot_sandlance' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_sandlance()))
})

# Test that the plot function creates an object
test_that("'plot_sandlance' creates an object", {
  expect_type(invisible(ecodata::plot_sandlance(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_sandlance' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_sandlance)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_sandlance)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_sandlance' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "sandlance",
    print = FALSE
  ))
})
