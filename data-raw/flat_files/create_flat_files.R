#' Save all data frames from an R package as individual CSV files,
#' skipping simple feature (sf) objects.
#'
#' This function identifies all publicly available data objects within a specified
#' R package. For each data object that is a data frame (and not a simple feature
#' object), it saves the data frame as a CSV file in the designated output directory.
#'
#' @param package_name A character string specifying the name of the R package.
#' @param output_dir A character string specifying the path to the directory
#'   where the CSV files will be saved. The directory will be created if it
#'   does not exist.
#' @return A character vector of the file paths of the successfully saved CSV files.
#'   Prints messages indicating progress and any skipped objects.
#' @export
#' @examples
#' \dontrun{
#' # To test with sf objects, you might need a package that exports them via data().
#' # For demonstration, let's assume 'somepackage_with_sf' exists and has sf data.
#' # If you have a package like 'sf' installed, it has example data:
#' # install.packages("sf")
#' # save_package_data_as_csv("sf", file.path(tempdir(), "sf_data_csv"))
#' # Note: The 'sf' package itself doesn't export sf objects directly via data(),
#' #       but its internal data might contain them.
#'
#' # Example with 'datasets' package (no sf objects, behaves as before)
#' temp_dir <- file.path(tempdir(), "datasets_csv")
#' saved_files <- save_package_data_as_csv("datasets", temp_dir)
#' if (!is.null(saved_files)) {
#'   print(paste("Saved files:", saved_files))
#'   # Optionally, clean up
#'   # unlink(temp_dir, recursive = TRUE)
#' }
#' }
save_package_data_as_csv <- function(package_name, output_dir) {
  # 1. Input Validation and Setup
  if (!is.character(package_name) || length(package_name) != 1) {
    stop("`package_name` must be a single character string.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1) {
    stop("`output_dir` must be a single character string.")
  }

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    message(paste("Creating output directory:", output_dir))
    dir.create(output_dir, recursive = TRUE)
  }

  # Check if the package is installed
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(paste("Package '", package_name, "' is not installed. Please install it first.", sep = ""))
  }

  message(paste("Attempting to extract data from package:", package_name))

  # List to store paths of successfully saved files
  saved_file_paths <- c()
  skipped_objects_count <- 0

  # 2. Get Data Objects from the Package
  data_list <- tryCatch(
    {
      utils::data(package = package_name)$results
    },
    error = function(e) {
      message(paste("Could not list data objects for package '", package_name, "': ", e$message, sep = ""))
      return(NULL)
    }
  )

  if (is.null(data_list) || nrow(data_list) == 0) {
    message(paste("No data objects found in package '", package_name, "' or unable to retrieve them.", sep = ""))
    return(invisible(NULL))
  }

  message(paste("Found", nrow(data_list), "potential data objects."))

  # 3. Iterate and Save Data
  for (i in seq_len(nrow(data_list))) {
    data_name <- data_list[i, "Item"]
    data_title <- data_list[i, "Title"] # For better logging

    message(paste0("Processing '", data_name, "' (", data_title, ")"))

    # Create a new environment to load data into, to avoid polluting global environment
    temp_env <- new.env()

    # Load the data object
    load_success <- tryCatch(
      {
        utils::data(list = data_name, package = package_name, envir = temp_env)
        data_object <- get(data_name, envir = temp_env)

        TRUE
      },
      error = function(e) {
        warning(paste0("Could not load data object '", data_name, "': ", e$message))
        FALSE
      }
    )

    if (load_success) {
      data_object <- get(data_name, envir = temp_env)

      # **NEW CHECK: Skip if it's an 'sf' (simple feature) object**
      if (inherits(data_object, "sf")) {
        message(paste0("  -> Skipped '", data_name, "' because it is a simple feature (sf) object."))
        skipped_objects_count <- skipped_objects_count + 1
      } else if (is.data.frame(data_object)) {
        # Original check for data frame
        file_name <- paste0(data_name, ".csv")
        file_path <- file.path(output_dir, file_name)

        tryCatch(
          {
            write.csv(data_object, file = file_path, row.names = FALSE)
            message(paste0("  -> Successfully saved '", data_name, "' to ", file_path))
            saved_file_paths <- c(saved_file_paths, file_path)
          },
          error = function(e) {
            warning(paste0("  -> Failed to save '", data_name, "' as CSV: ", e$message))
            skipped_objects_count <- skipped_objects_count + 1
          }
        )
      } else {
        message(paste0("  -> Skipped '", data_name, "' because it is not a data frame (it's a ", class(data_object)[1], ")."))
        skipped_objects_count <- skipped_objects_count + 1
      }
    } else {
      skipped_objects_count <- skipped_objects_count + 1
    }
  }

  message(paste("\nProcess complete. Successfully saved", length(saved_file_paths), "CSV files."))
  if (skipped_objects_count > 0) {
    message(paste("Skipped", skipped_objects_count, "object(s) (either not data frames, sf objects, or failed to load)."))
  }

  if (length(saved_file_paths) > 0) {
    message(paste("Files saved in:", output_dir))
  }

  return(invisible(saved_file_paths)) # Return paths invisibly for programmatic use
}

save_package_data_as_csv(
  package = "ecodata",
  output_dir = here::here("data-raw/flat_files")
)
