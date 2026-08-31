# Test that the plot function has working default arguments
test_that("'plot_calanus_stage' has valid default arguments", {
  expect_no_error(invisible(ecodata::plot_calanus_stage()))
})

# Test that the plot function creates an object
test_that("'plot_calanus_stage' creates an object", {
  expect_type(invisible(ecodata::plot_calanus_stage(), "object"))
})

# Test that the plot function has user-defined arguments
test_that("'plot_calanus_stage' has user-defined attributes", {
  expect_gte(length(attributes(ecodata::plot_calanus_stage)), 1)
  expect_no_match(
    names(attributes(ecodata::plot_calanus_stage)),
    "srcref",
    all = FALSE
  )
})

# Test that the plot function has valid argument combinations
test_that("'plot_calanus_stage' all argument combinations work", {
  expect_no_error(ecodata::create_all_plots(
    ecodata_name = "calanus_stage",
    print = FALSE
  ))
})
