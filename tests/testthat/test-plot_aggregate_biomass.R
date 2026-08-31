# Test that the plot function has working default arguments
test_that("'plot_aggregate_biomass' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_aggregate_biomass()))
})

# Test that the plot function creates an object
test_that("'plot_aggregate_biomass' creates an object", {
  expect_type(invisible(ecodata::plot_aggregate_biomass()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_aggregate_biomass' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_aggregate_biomass)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_aggregate_biomass)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_aggregate_biomass' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "aggregate_biomass",
    print = FALSE
  ))
})
