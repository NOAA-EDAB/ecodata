# Test that the plot function has working default arguments
test_that("'plot_rec_hms' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_rec_hms()))
})

# Test that the plot function creates an object
test_that("'plot_rec_hms' creates an object", {
  expect_type(invisible(ecodata::plot_rec_hms(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_rec_hms' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_rec_hms)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_rec_hms)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_rec_hms' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "rec_hms",
    print = FALSE
  ))
})
