# Test that the plot function has working default arguments
test_that("'plot_nao' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_nao()))
})

# Test that the plot function creates an object
test_that("'plot_nao' creates an object", {
  expect_type(invisible(ecodata::plot_nao(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_nao' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_nao)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_nao)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_nao' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "nao",
    print = FALSE
  ))
})
