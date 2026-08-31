# Test that the plot function has working default arguments
test_that("'plot_grayseal' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_grayseal()))
})

# Test that the plot function creates an object
test_that("'plot_grayseal' creates an object", {
  expect_type(invisible(ecodata::plot_grayseal()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_grayseal' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_grayseal)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_grayseal)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_grayseal' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "grayseal",
    print = FALSE
  ))
})
