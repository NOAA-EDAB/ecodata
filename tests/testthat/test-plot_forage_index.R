# Test that the plot function has working default arguments
test_that("'plot_forage_index' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_forage_index()))
})

# Test that the plot function creates an object
test_that("'plot_forage_index' creates an object", {
  expect_type(invisible(ecodata::plot_forage_index(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_forage_index' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_forage_index)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_forage_index)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_forage_index' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "forage_index",
    print = FALSE
  ))
})
