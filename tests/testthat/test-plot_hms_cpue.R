# Test that the plot function has working default arguments
test_that("'plot_hms_cpue' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_hms_cpue()))
})

# Test that the plot function creates an object
test_that("'plot_hms_cpue' creates an object", {
  expect_type(invisible(ecodata::plot_hms_cpue()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_hms_cpue' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_hms_cpue)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_hms_cpue)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_hms_cpue' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "hms_cpue",
    print = FALSE
  ))
})
