# Test that the plot function has working default arguments
test_that("'plot_seabird_mab' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_seabird_mab()))
})

# Test that the plot function creates an object
test_that("'plot_seabird_mab' creates an object", {
  expect_type(invisible(ecodata::plot_seabird_mab()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_seabird_mab' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_seabird_mab)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_seabird_mab)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_seabird_mab' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "seabird_mab",
    print = FALSE
  ))
})
