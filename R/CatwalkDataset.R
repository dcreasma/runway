#' @export
setClass(
  "CatwalkDataset",
  representation(
    metadata = "data.frame",
    data = "list",
    removed_variables = "character",
    formulas = "character",
    models = "list"
  ),
  prototype = list(
    metadata = data.frame(),
    data = list(),
    removed_variables = character(),
    formulas = character(),
    models = list()
  )
)


#' Create a new CatwalkDataset object from a file or dataframe
#' @param data A path to a spreadsheet or a dataframe containing the raw data
#' @param excluded_vars A character vector of variable names to exclude from the analysis
#' @param excluded_group A character vector of group names to exclude from the analysis
#' @param remove_null_vars A boolean indicating whether to remove variables containing only "-" values
#' @param remove_zero_vars A boolean indicating whether to remove variables containing only 0s
#' @param group_var A string specifying the name of the group variable in the input data
#' @param animal_var A string specifying the name of the animal ID variable in the input data
#' @param experiment A string indicating the name of the experiment (optional)
#' @param timepoint A string indicating the timepoint of the experiment (optional)
#' @param sheet An integer indicating the sheet number of the input data file (if data is a file with multiple sheets)
#' @return A CatwalkDataset object containing the processed data and metadata
#' @export
CatwalkDataset <- function(data,
                           excluded_vars = NULL,
                           excluded_group = NULL,
                           remove_null_vars = T,
                           remove_zero_vars = T,
                           group_var = "group",
                           animal_var = "animal",
                           experiment = "none given",
                           timepoint = "none given",
                           sheet = 1) {
  #if the input for data_raw is a character, clean_catwalk_data treats it like a file name and imports the file
  if (is.character(data)) {
    numsheets <- length(data)
    # If there is only one sheet, import it using rio::import and give the columns unique names
    ifelse(
      numsheets == 1,
      {
        data_raw <- rio::import(data, which = sheet)
        names(data_raw) <- make.names(names(data_raw), unique = TRUE)
      },
      # If there are multiple sheets, store them as a list of dataframes
      {
        datasets <- list()
      }
    )
  }
  # If the input for data is already a dataframe, use it directly
  else if (is.data.frame(data)) {
    data_raw <- data
    numsheets <- 1
  }
  else {
    stop("'data' argument not a dataframe or path to spreadsheet.")
  }
  # Process each sheet of data (if there are multiple)
  for (i in 1:numsheets) {

    # If there are multiple sheets, import the ith sheet
    if (numsheets > 1) {
      data_raw <- rio::import(data[i], which = sheet)
      names(data_raw) <- make.names(names(data_raw), unique = TRUE)
    }

    # Collect experimental metadata (experiment name, timepoint)
    metadata <-
      data.frame(c(experiment[i], timepoint[i]),
                 row.names = c("experiment", "time_point"))

    # Select only variables that are on list of standard catwalk variables
    data_clean <-
      data_raw %>% dplyr::select_if(names(.) %in% catwalk_var_names())

    # Identify any variables in the inputted file that are not on the list of standard catwalk variables
    unk_vars <-
      names(data_raw %>% dplyr::select_if(!(names(.) %in% catwalk_var_names())))
    if (!rlang::is_empty(unk_vars)) {
      print("The following variables are not in the list of standard catwalk variables: ")
      print(unk_vars)
    }

    # Identify any standard catwalk variables that are not in the inputted file
    missing_vars <-
      catwalk_var_names()[!(catwalk_var_names() %in% names(data_raw))]
    if (!rlang::is_empty(missing_vars))
    {
      print("The following expected variables were not found in the inputted file: ")
      print(missing_vars)
    }

    # Identify any variables containing "-" values
    null_vars <- NULL
    if (remove_null_vars) {
      cols <- colSums(mapply('==', '-',  data_raw))
      null_vars <- colnames(data_raw[, which(cols != 0)])
    }

    # Identify any variables containing only NAs
    na_vars <- NULL
    if (remove_null_vars) {
      cols <- colSums(is.na(data_raw)) == nrow(data_raw)
      na_vars <- colnames(data_raw)[cols]
    }

    # Identify any variables containing only 0s
    zero_vars <- NULL
    if (remove_zero_vars) {
      zero_vars <- data_raw  %>% dplyr::select_if(is.numeric)
      zero_vars <-
        names(zero_vars[, colSums(zero_vars, na.rm = TRUE) == 0])
    }

    # Remove any excluded variables
    vars_to_remove <-
      unique(c(excluded_vars, null_vars, zero_vars, na_vars))
    data_clean <-
      data_clean %>% dplyr::select(-dplyr::matches(vars_to_remove))

    # Identify the animal ID variable
    animal <-
      factor(detect_duplicate_vars(data = data_raw, var_name = animal_var))

    # Add/replaces raw_data animal variable with the cleaned one (necessary to prevent type issues when joining datasets together)
    data_raw$animal_factor <- animal

    # Identify the group variable
    group <-
      factor(detect_duplicate_vars(data = data_raw, var_name = group_var))

    # Compile clean data with animal and group variables
    data_clean <-
      data.frame(animal, group, data_clean)  %>%
      dplyr::mutate(time_point = as.factor(timepoint[i]), .after = group) %>%
      dplyr::mutate(experiment = as.factor(experiment[i]), .after = group)

    # Remove any animals who fall in the excluded group
    if (!is.null(excluded_group)) {
      data_clean <- data_clean %>% dplyr::filter(group != excluded_group)
    }

    # Create the processed data as a list containing the metadata, cleaned data, and any variables removed
    newcatwalkdataset <- new(
      "CatwalkDataset",
      metadata = metadata,
      data = list(data_clean = data_clean, data_raw = data_raw),
      removed_variables = c(vars_to_remove)
    )

    # If there are multiple sheets, combine the datasets into a single CatwalkDataset object
    if (numsheets > 1) {
      datasets <- append(datasets, list(newcatwalkdataset))
      newcatwalkdataset <- catwalk_combine_datasets(datasets)
    }
  }

  newcatwalkdataset
}


#' Checks if a data set contains more than one variable containing a given string
#'
#' @param data A dataframe to check for duplicated variables.
#' @param var_name A string to look for in the dataframe's variable names.
#'
#' @return The only variable containing a given string (if only one) or the variable selected by the user if multiple variables contain the same string.
detect_duplicate_vars <- function(data, var_name) {
  # check if any column name matches the pattern provided in `var_name`
  if (any(stringr::str_detect(names(data), stringr::regex(var_name, ignore_case = T)))) {
    # subset the dataframe to only include columns that match the pattern in `var_name`
    var <- data %>% dplyr::select(dplyr::contains(var_name))
    # if there is more than one column that matches the pattern, prompt the user to select which column to use
    if (ncol(var) > 1) {
      print(paste("Multiple potential", var_name, "variables detected, select which one you want to use"))
      print(names(var))
      n <- as.integer(readline())
      var <- var[, n]
    }
    else {
      # if there is only one column that matches the pattern, use that column
      var <- var[, 1]
    }
    var
  }
  else {
    # if no columns match the pattern in `var_name`, throw an error
    stop(paste("No variable containing the string ", var_name, "found in inputted dataframe. Please check input data and query string."))
  }
}



#'
#' Add a formula to a CatwalkDataset object
#' @param catwalkdataset A CatwalkDataset object to which the formula will be added
#' @param formula A character string specifying the formula to be added
#' @return A CatwalkDataset object with the new formula added
#' @export
add_formula <- function(catwalkdataset, formula){
  # Check if the formula is already attached to the dataset
  if (formula %in% catwalkdataset@formulas){
    warning("Formula is already attached to this dataset, no changes to dataset were made")
    return(catwalkdataset)
  }

  # Add the formula to the dataset's formulas attribute
  catwalkdataset@formulas <- c(catwalkdataset@formulas, formula)

  # Return the updated dataset object
  catwalkdataset
}

#' Extract a specific dataset from a CatwalkDataset object
#' @param catwalkdataset A CatwalkDataset object containing the desired dataset
#' @param dataname A character string specifying the name of the dataset to extract (default: "data_norm")
#' @return A data.frame containing the specified dataset from the CatwalkDataset object
#' @export
catwalk_get_data_from_dataset <- function(catwalkdataset, dataname = c("data_norm","data_clean")){
  # Check if the specified dataset name is valid
  for (name in dataname){
    if(name %in% names(catwalkdataset@data)){
      dataname <- name
      break
    }
  }

  # Throw an error if the specified dataset name is not found in the CatwalkDataset object
  if(!dataname %in% names(catwalkdataset@data)){
    stop(paste0("No data with the name '", dataname, "' was found in this dataset."))
  }

  # Extract the specified dataset from the CatwalkDataset object
  data <- catwalkdataset@data[[dataname]]

  # Return the extracted dataset as a data.frame
  data
}
