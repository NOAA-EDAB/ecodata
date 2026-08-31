# Test that the plot function has working default arguments
test_that("'plot_seal_pups' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_seal_pups()))
})

# Test that the plot function creates an object
test_that("'plot_seal_pups' creates an object", {
  expect_type(invisible(ecodata::plot_seal_pups()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_seal_pups' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_seal_pups)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_seal_pups)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_seal_pups' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "seal_pups",
    print = FALSE
  ))
})
