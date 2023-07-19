#' Normalize catwalk data to a specified timepoint
#'
#' @param catwalkdataset A catwalk dataset object to be normalized
#' @param normtimepoint The timepoint to normalize to
#' @param dataname The name of the data to be normalized within the catwalk dataset
#' @param keep_unnormed Whether to keep unnormalized variables in the output dataset
#' @param keep_norm_time_point Whether to keep the normalization timepoint in the output dataset
#'
#' @return A catwalk dataset object with a new 'data_norm' list element containing the normalized data
#' @export
#' @importFrom dplyr %>%
#' @examples
catwalk_normalize <-
  function (catwalkdataset, normtimepoint, dataname = "data_clean", keep_unnormed = T, keep_norm_time_point = T) {
    # Check that the specified data name is in the catwalk dataset
    if (!dataname %in% names(catwalkdataset@data)){
      stop(paste0("Could not find data named '", dataname, "' in catwalk dataset provided"))
    }

    # Extract the data to be normalized
    data <- catwalkdataset@data[[dataname]]

    # Calculate the means of each variable for each group for the specified normalization timepoint
    means <- catwalk_group_means(data %>% dplyr::filter(time_point == normtimepoint))

    # Remove any variables with all 0 values from the means dataframe
    zero_vals <- colnames(means)[col(means)[which(means == 0)]]
    means <- means %>% dplyr::select(!zero_vals)

    # Remove the normalization timepoint from the dataset if requested
    if(!keep_norm_time_point){
      data <- data %>% filter(time_point != normtimepoint)
    }

    # Initialize output variables
    data_norm <- NULL
    unnormed_vars <- c()

    # Loop through each group and normalize the data
    for (x in means$group) {
      df <- data %>% dplyr::filter(group == x) %>% dplyr::select()
      for (name in colnames(data)) {
        if (name %in% colnames(means) & name != "group") {
          mean <- as.numeric(means %>% dplyr::filter(group == x) %>% dplyr::select(name))
          var <- data %>% dplyr::filter(group == x) %>% dplyr::select(name) / mean * 100
          df <- cbind(df, var)
        }
        else {
          # Keep unnormalized variables if requested
          unnormed_vars <- c(unnormed_vars, name)
          if(keep_unnormed){
            var <- data %>% dplyr::filter(group == x) %>% dplyr::select(name)
            df <- cbind(df, var)
          }
        }
      }
      # Combine the normalized and unnormalized data into a single dataframe
      data_norm <- rbind(data_norm,df)
    }

    # Print a message for any variables that couldn't be normalized
    if (length(unnormed_vars)>0){
      message = ifelse(keep_unnormed, "returning unnormalized values", "and were removed")
      print(paste0("The variable '", unnormed_vars, "' could not be normalized, ", message))
    }

    # Add the normalized data to the catwalk dataset object and return it
    catwalkdataset@data[["data_norm"]] <- data_norm
    catwalkdataset


  }

#' Collects the means for each variable in a catwalk data set for each unique group
#'
#' @param data catwalk data to measure the means of.
#'
#' @return a dataframe of means.
#' @export
#'
#' @examples
#' @importFrom dplyr %>%
catwalk_group_means <- function (data) {
  # group the data by unique group
  means <- data %>%
    dplyr::group_by(group) %>%
    # select all numeric variables and exclude "animal" and "time_point" columns
    dplyr::select(!c("animal", "time_point")) %>%
    dplyr::select_if(is.numeric) %>%
    # calculate the mean for each numeric variable for each group
    dplyr::summarise_all(mean, na.rm = TRUE)
}
