# Test that the plot function has working default arguments
test_that("'plot_forage_anomaly' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_forage_anomaly()))
})

# Test that the plot function creates an object
test_that("'plot_forage_anomaly' creates an object", {
  expect_type(invisible(ecodata::plot_forage_anomaly()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_forage_anomaly' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_forage_anomaly)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_forage_anomaly)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_forage_anomaly' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "forage_anomaly",
    print = FALSE
  ))
})
