# Test that the plot function has working default arguments
test_that("'plot_zoo_strat_abun' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_zoo_strat_abun()))
})

# Test that the plot function creates an object
test_that("'plot_zoo_strat_abun' creates an object", {
  expect_type(invisible(ecodata::plot_zoo_strat_abun(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_zoo_strat_abun' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_zoo_strat_abun)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_zoo_strat_abun)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_zoo_strat_abun' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "zoo_strat_abun",
    print = FALSE
  ))
})
