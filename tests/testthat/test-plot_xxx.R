#' Test for all plot function. Make sure they dont error

# List all of the plot functions
functions_to_test <- ls("package:ecodata", pattern = "plot")

options(warn = -1)
# loop through all functions and run test
for (fname in functions_to_test) {
  f <- get(fname)

  if (fname %in% c("plot_setup", "plot_function_template")) {
    next
  }

  test_that(paste0("Plotting function, ", fname, ". Does it throw an error?"), {
    p <- f()
    print(fname)
    expect_type(p, "list")
  })
}

# Also NULL and character

options(warn = 0)
