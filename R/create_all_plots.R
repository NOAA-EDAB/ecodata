#' Create all variations of an ecodata plot function
#'
#' This function can flexibly plot all possible variations given the arguments and attributes of an ecodata plot function
#' This function can also print all of the code needed to run all possible variations
#' This may be useful for reviewing data submissions, creating catalog pages, automating QA/QC protocols and comparing against other ecodata versions.
#'
#' @param ecodata_name string. the data name of the target ecodata dataset or plot function (default = NULL)
#' @param write_only boolean. should the function output the plotting code, as text, for all plot variations (default = FALSE)
#' @param n numeric. number of data points for short term trend (default = 0)
#'
#' @return list containing plots when write_only = FALSE and text when write_only = TRUE. plots are also exported to the IDE plot pane.
#'
#' @export

create_all_plots <- function(ecodata_name = NULL, write_only = FALSE, n = 0) {
  # Define plot function syntax based on user input
  function_name <- paste0("ecodata::plot_", ecodata_name)

  # Parse the argument names (e.g. "report", "varName", etc.) of the plot function
  all_arguments <- names(formals(eval(parse(
    text = function_name
  ))))

  # Create expanded grid of the attributes listed in the plot function
  # This produces a data table of every combination of arguments
  all_attributes <- expand.grid(
    attributes(eval(parse(
      text = function_name
    ))),
    stringsAsFactors = F
  )

  # Initialize a new data table based on the expanded grid of all atrributes
  # This data table will be processed such that it contains only unique, valid argument combinations
  all_combinations <- all_attributes

  # Remove unused "srcref" attributes, which exist in some plot functions
  if ("srcref" %in% colnames(all_combinations)) {
    all_combinations <- all_combinations |>
      dplyr::select(!srcref)
  }

  # For plot functions that include an "EPU" argument:
  # Remove invalid combinations of "report" and "EPU"
  if ("EPU" %in% all_arguments) {
    all_combinations <- all_combinations |>
      dplyr::filter(!(report == "NewEngland" & EPU == "MAB")) |>
      dplyr::filter(!(report == "MidAtlantic" & EPU == "GOM")) |>
      dplyr::filter(!(report == "MidAtlantic" & EPU == "GB"))
  }

  # Each row of 'all_combinations' now contains a set of valid arguments
  # Remove duplicates using 'dplyr::distinct()'
  # Mutate each cell with a combination of the column name (i.e. argument name)
  # and the cell value (i.e. argument value). For example, "report = 'MidAtlantic'"
  all_combinations <- all_combinations |>
    dplyr::distinct() |>
    dplyr::mutate(dplyr::across(
      everything(),
      ~ paste0(dplyr::cur_column(), " = '", .x, "'")
    ))

  # Read across each row of 'all_combinations'
  # Create one string per row, which is written exactly as plot functions are used
  # Store as a list called 'argument_strings'
  # Each element of this list now contains a valid, unique set of arguments
  # that can be passed to the plot function as a string
  argument_strings <- apply(all_combinations, 1, paste, collapse = ", ")

  # Check for the presence of the 'n' argument
  # If it is used, append the argument string with the 'n' argument
  # using the 'n' argument supplied by the user of this function
  # This allows the user of this function to toggle on/off the short term trend
  if ("n" %in% all_arguments) {
    short_term_string <- paste0(", n = ", n)
  } else {
    short_term_string <- NULL
  }

  # Initialize an empty list that will be used to store the function results
  plotting_results <- list()

  # Iterate over the 'argument_strings' list for each element it contains
  # In each iteration, the list element (i.e. valid, unique set of arguments)
  # is concatenated with the 'short_term_string' (if 'n' is used as an argument)
  # This final argument string is then concatenated with the plot function name
  # and formatted exactly as required by the plot function
  for (i in 1:length(argument_strings)) {
    function_call <- paste0(
      function_name,
      "(",
      argument_strings[i],
      short_term_string,
      ")"
    )

    # IF 'write_only' is TRUE, each iteration outputs the 'function_call' string
    # to a list called 'plotting_results'
    # IF 'write_only' is FALSE, each iteration outputs the resulting plot instead
    if (write_only == TRUE) {
      plotting_results[[i]] <- function_call
    } else {
      plotting_results[[i]] <- eval(parse(text = function_call))
    }

    print(plotting_results[[i]])
  }

  # The function returns the 'plotting_results' list to the user
  # If stored as a session variable, all combinations of plotting code ('write_only' = TRUE)
  # or the plots themselves ('write_only' = FALSE) can be recalled from the user's IDE environment
  return(invisible(plotting_results))
}
