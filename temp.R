data_to_norm_to <- catwalk_preinj_data
data_to_norm <- catwalk_2wks_postTX_data

means <- catwalk_group_means(data_to_norm_to)
zero_vals <- colnames(means)[col(means)[which(means == 0)]]
means <- means %>% select(!zero_vals)
data <- NULL
for (x in means$group) {
  n <- 3
  df <- NULL
  df <- data_to_norm %>% filter(group == x) %>% select(animal, group, time_point)
  unnormed_vars <- c()
  for (name in colnames(data_to_norm)) {
    if (name %in% colnames(means)) {
      mean <-
        as.numeric(means %>% filter(group == x) %>% select(name))
      var <-
        data_to_norm %>% filter(group == x) %>% select(name) / mean * 100
      df <- cbind(df, var)
    }
    else {
      unnormed_vars <- c(unnormed_vars, name)
      var <-
        data_to_norm %>% filter(group == x) %>% select(name)
    }

  }
  print(paste0("The variables ", unnormed_vars, " could not be normalized, returning unnormalized results"))
  data <- rbind(data,df)
}
data
