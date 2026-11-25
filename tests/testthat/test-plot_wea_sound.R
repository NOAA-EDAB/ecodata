# Test that the plot function has working default arguments
test_that("'plot_wea_sound' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_wea_sound()))
})

# Test that the plot function creates an object
test_that("'plot_wea_sound' creates an object", {
  expect_type(invisible(ecodata::plot_wea_sound()), "object")
})

# Test that the plot function has user-defined attributes
test_that("'plot_wea_sound' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_wea_sound)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_wea_sound)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_wea_sound' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "wea_sound",
    print = FALSE
  ))
})
