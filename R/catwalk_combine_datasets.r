#' Combine multiple CatwalkDataset objects into a single object
#'
#' @param datasets A list of CatwalkDataset objects to combine
#'
#' @return A new CatwalkDataset object containing merged data from all input datasets
#' @export
#'
#' @examples
catwalk_combine_datasets <- function(datasets) {
  # Create empty variables to store merged data, metadata, and removed variables
  time_point_levels <- factor()
  removed_variables <- character()
  datanames <- character()
  data <- list()
  metadata <- data.frame()
  # Loop through each input dataset
  for (x in datasets) {
    # Check that it is a valid CatwalkDataset object
    if (!inherits(x, "CatwalkDataset")) {
      stop("Every entry in the list must be a CatwalkDataset object")
    }

    # Collect unique timepoints and dataset names
    levels <- c(levels, unique(x@metadata$time_point))
    datanames <- unique(c(datanames, names(x@data)))

    # Merge metadata
    if (ncol(metadata) > 0) {
      metadata <- full_join(metadata, x@metadata)
    } else {
      metadata <- x@metadata
    }
  }

  # Loop through each dataset name
  for (name in datanames) {
    data_temp <- data.frame()

    # Loop through each input dataset again
    for (x in datasets) {
      # Check if the current dataset has the current name
      if (name %in% names(x@data)) {
        # Join data with existing merged data
        if (ncol(data_temp) > 0) {
          uncommon_cols <- c(setdiff(colnames(data_temp), colnames(x@data[[name]])), setdiff(colnames(x@data[[name]]), colnames(data_temp)))
          removed_variables <- unique(c(removed_variables, uncommon_cols))
          if (name == "data_raw") {
            data_temp <- dplyr::full_join(data_temp, x@data[[name]], by = "animal_factor")
          } else {
            data_temp <- dplyr::full_join(data_temp, x@data[[name]])
          }
          data_temp <- data_temp  %>% dplyr::select(!uncommon_cols)
        } else {
          data_temp <- x@data[[name]]
        }
      }
    }

    # Add merged data
    data <- c(data, list(data_temp))
  }

  # Give merged data names based on input dataset names
  names(data) <- datanames

  # Convert time_point column to factor
  if (!is.numeric(x@data$data_clean$time_point)) {
    x@data$data_clean$time_point <- factor(x@data$data_clean$time_point, levels = levels)
  }

  # Create a new CatwalkDataset object with merged data, metadata, and removed variables
  mergedcatwalkdataset <- new("CatwalkDataset",
                              data = data,
                              metadata = metadata,
                              removed_variables = removed_variables)

  # Ensure that the formulas attribute matches the input datasets
  mergedcatwalkdataset@formulas <- unique(unlist(lapply(datasets, function(x) x@formulas)))

  # Return the new CatwalkDataset object
  mergedcatwalkdataset
}
formals(catwalk_combine_datasets) <- list(datasets = "list")
