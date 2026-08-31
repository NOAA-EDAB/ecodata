# Test that the plot function has working default arguments
test_that("'plot_habitat_diversity' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_habitat_diversity()))
})

# Test that the plot function creates an object
test_that("'plot_habitat_diversity' creates an object", {
  expect_type(invisible(ecodata::plot_habitat_diversity(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_habitat_diversity' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_habitat_diversity)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_habitat_diversity)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_habitat_diversity' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "habitat_diversity",
    print = FALSE
  ))
})
