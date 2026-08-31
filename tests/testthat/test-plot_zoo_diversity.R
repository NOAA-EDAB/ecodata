# Test that the plot function has working default arguments
test_that("'plot_zoo_diversity' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_zoo_diversity()))
})

# Test that the plot function creates an object
test_that("'plot_zoo_diversity' creates an object", {
  expect_type(invisible(ecodata::plot_zoo_diversity(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_zoo_diversity' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_zoo_diversity)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_zoo_diversity)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_zoo_diversity' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "zoo_diversity",
    print = FALSE
  ))
})
