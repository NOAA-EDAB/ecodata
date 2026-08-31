# Test that the plot function has working default arguments
test_that("'plot_abc_acl' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_abc_acl()))
})

# Test that the plot function creates an object
test_that("'plot_abc_acl' creates an object", {
  expect_type(invisible(ecodata::plot_abc_acl()), "object")
})

# Test that the plot function has user-defined arguments
test_that("'plot_abc_acl' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_abc_acl)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_abc_acl)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_abc_acl' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "abc_acl",
    print = FALSE
  ))
})
