# Test that the plot function has working default arguments
test_that("'plot_ichthyo_diversity' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_ichthyo_diversity()))
})

# Test that the plot function creates an object
test_that("'plot_ichthyo_diversity' creates an object", {
  expect_type(invisible(ecodata::plot_ichthyo_diversity()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_ichthyo_diversity' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_ichthyo_diversity)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_ichthyo_diversity)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_ichthyo_diversity' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "ichthyo_diversity",
    print = FALSE
  ))
})
