# Test that the plot function has working default arguments
test_that("'plot_species_groupings' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_species_groupings()))
})

# Test that the plot function creates an object
test_that("'plot_species_groupings' creates an object", {
  expect_type(invisible(ecodata::plot_species_groupings()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_species_groupings' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_species_groupings)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_species_groupings)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_species_groupings' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "species_groupings",
    print = FALSE
  ))
})
