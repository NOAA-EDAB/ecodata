# Test that the plot function has working default arguments
test_that("'plot_zoo_community' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_zoo_community()))
})

# Test that the plot function creates an object
test_that("'plot_zoo_community' creates an object", {
  expect_type(invisible(ecodata::plot_zoo_community(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_zoo_community' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_zoo_community)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_zoo_community)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_zoo_community' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "zoo_community",
    print = FALSE
  ))
})
