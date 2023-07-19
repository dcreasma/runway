#' Performs PCA on a CatwalkDataset
#'
#' @param catwalkdataset CatwalkDataset object to perform PCA on
#' @param ncomp number of principal components to keep
#'
#' @return the updated CatwalkDataset object containing PCA scores and raw PCA output
#' @param dataname name of the dataset to use for PCA
#' @export
#' @importFrom dplyr %>%
#' @examples
catwalk_pca <- function(catwalkdataset, ncomp = 1, dataname = c("data_norm", "data_clean")) {
  # Extract numeric data from the CatwalkDataset object and remove unnecessary columns
  data <- catwalk_get_data_from_dataset(catwalkdataset, dataname = dataname)
  data_temp_1 <-
    data %>%
    dplyr::ungroup() %>%
    dplyr::select(!(c("animal", "group", "time_point"))) %>%
    dplyr::select_if(is.numeric)
  # Remove variables with zero variance or containing 'NA' values
  data_for_pca <-
    data_temp_1[, colSums(data_temp_1, na.rm = T) != 0]
  data_for_pca <-
    data_for_pca[, colSums(is.na(data_for_pca)) == 0]
  print("The following variables were removed because they had zero variance (all same value) or contained 'NA' values: ")
  print(names(data_temp_1)[!(names(data_temp_1) %in% names(data_for_pca))])

  # Perform PCA on the data and extract PCA scores
  pca_raw <- stats::prcomp(data_for_pca, scale. = T, center = T)
  pca_scores <- data.frame(pca_raw$x[, 1:ncomp])
  prefix <- "PC"
  suffix <- seq(1:ncomp)
  PCA_names <- paste(prefix, suffix, sep = "")

  # Add PCA scores to the CatwalkDataset object
  pca_scores <-
    data.frame(data$'animal', data$'group', data$'time_point', pca_scores)
  names(pca_scores) <- c("animal", "group", "time_point", PCA_names)
  catwalkdataset@data[["data_pca"]] <- pca_scores

  # Return the updated CatwalkDataset object
  catwalkdataset
}
