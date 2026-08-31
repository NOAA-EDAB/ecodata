# Test that the plot function has working default arguments
test_that("'plot_hms_landings' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_hms_landings()))
})

# Test that the plot function creates an object
test_that("'plot_hms_landings' creates an object", {
  expect_type(invisible(ecodata::plot_hms_landings(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_hms_landings' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_hms_landings)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_hms_landings)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_hms_landings' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "hms_landings",
    print = FALSE
  ))
})
