# Test that the plot function has working default arguments
test_that("'plot_cetacean_dist' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_cetacean_dist()))
})

# Test that the plot function creates an object
test_that("'plot_cetacean_dist' creates an object", {
  expect_type(invisible(ecodata::plot_cetacean_dist()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_cetacean_dist' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_cetacean_dist)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_cetacean_dist)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_cetacean_dist' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "cetacean_dist",
    print = FALSE
  ))
})
