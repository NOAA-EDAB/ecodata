# Test that the plot function has working default arguments
test_that("'plot_energy_density' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_energy_density()))
})

# Test that the plot function creates an object
test_that("'plot_energy_density' creates an object", {
  expect_type(invisible(ecodata::plot_energy_density()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_energy_density' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_energy_density)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_energy_density)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_energy_density' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "energy_density",
    print = FALSE
  ))
})
