#' Performs aov modeling on the dependent variables in a given dataset, using the relationship of the independent variables as dictated by the designated formula
#'
#' @param catwalkdataset A CatwalkDataset
#' @param formula A CatwalkDataset
#' @param dataname A character, or character vector, containing the name of the data within the CatwalkDataset to be used for the analysis.
#' If a vector of names is passed, the function will use the first name found in the CatwalkDataset
#' @param exc_vars A character vector of any variable names to be excluded from the analysis
#' @param cutoff The value at which p-values lower than this number will be considered "statistically significant"
#' @param make_default A boolean that will designate the output of this analysis as the default output to be reference for any future visualization or follow-up analyses
#' @param allow_duplicates A boolean that designates whether or not to allow the creation of two sets of models using the same data, formula, and cutoff within the same CatwalkDataset
#' @return A CatwalkDataset object containing the newly generated models
#' @export
catwalk_anovas <-
  function(catwalkdataset,
           formula = 1,
           dataname = c("data_norm", "data_clean"),
           exc_vars = character(),
           cutoff = 0.05,
           make_default = F,
           allow_duplicates = F) {
    data <-
      catwalk_get_data_from_dataset(catwalkdataset, dataname = dataname)

    for (name in dataname) {
      if (name %in% names(catwalkdataset@data)) {
        data_used <- name
        break
      }
    }

    if (inherits(formula, "numeric")) {
      formula <- catwalkdataset@formulas[formula]
    }
    if (is.na(formula)) {
      stop("formula not found at location provided, or no default formula available")
    }

    for (model in catwalkdataset@models) {
      if (!allow_duplicates &&
          data_used == model$data_used &&
          formula == model$formula_used &&
          cutoff == model$cutoff) {
        warning(
          "Warning: Models using this combination of formula, cuttoff, and dataset exist already within this CatwalkDataset, execution ceased before duplicate models were generated. Set the argument 'allow_duplicates' = TRUE to bypass this."
        )
        return(catwalkdataset)
      }


    }

    variable_names <- data %>%
      dplyr::select(!c("group", "time_point", "animal", "experiment")) %>%
      dplyr::select_if(is.numeric) %>% colnames()
    aov_models = list()
    for (name in variable_names) {
      formula_temp <-  as.formula(paste0(name, formula))
      aovmodel <- aov(formula = formula_temp, data = data)
      aov_models <- c(aov_models, list(aovmodel))
    }
    aov_summaries <-
      aov_models %>% purrr::map( ~ summary(.x)[[1]]) %>% purrr::map( ~ fix_names(.x))
    names(aov_summaries) <- variable_names
    names(aov_models) <- variable_names

    hits <- list()
    all_pvals <- data.frame()
    factornames <- rownames(aov_summaries[[1]])
    factornames <- factornames[1:length(factornames) - 1]
    for (factorname in factornames) {
      pvals <- numeric()
      for (summary in aov_summaries) {
        row = which(rownames(summary) == factorname)
        pvals = c(pvals, summary[row, 5])
      }
      new_pvals <-
        data.frame(pval = pvals) %>% dplyr::mutate(factor = factorname, parameter = names(aov_summaries))
      new_hits <-
        list(new_pvals %>% dplyr::filter(pval < cutoff) %>% dplyr::pull(parameter))
      hits <- append(hits, new_hits)
      all_pvals <- rbind(all_pvals, new_pvals) %>%
        dplyr::mutate(parameter = forcats::fct_reorder2(parameter, factor, pval, .fun = forcats::first2))
    }
    names(hits) <- c(factornames)
    all_pvals <-
      all_pvals %>% dplyr::mutate(factor = factor(factor, levels = factornames))
    sig_pvals <- all_pvals
    for (parameter_name in names(aov_summaries)) {
      temp <- all_pvals %>% dplyr::filter(parameter == parameter_name)
      if (!any(temp$pval < cutoff)) {
        sig_pvals <-
          sig_pvals %>% dplyr::filter(parameter != parameter_name)
      }
    }

    new_model <-
      list(
        list(
          data_used = data_used,
          formula_used = formula,
          cutoff = cutoff,
          models = aov_models,
          summaries = aov_summaries,
          all_pvals = all_pvals,
          sig_pvals = sig_pvals,
          hits = hits
        )
      )
    if (make_default) {
      catwalkdataset@models <-
        append(catwalkdataset@models, new_model, after = 0)
    }
    else {
      catwalkdataset@models <- append(catwalkdataset@models, new_model)
    }
    catwalkdataset
  }

#' Fixes row names of a data frame by trimming leading/trailing whitespace
#'
#' @param data A data frame
#' @return A data frame with fixed row names
fix_names <- function(data) {
  # Use stringr::str_trim to remove leading/trailing whitespace from row names
  rownames(data) <- rownames(data) %>% stringr::str_trim()
  # Return the data frame with fixed row names
  data
}

#' Define a new transformation for use with ggplot2 that maps values to the sigmoidal scale
#'
#' This function defines a new transformation called "sigmoidal" that maps values to the sigmoidal scale.
#'
#' @param a The slope parameter for the sigmoidal transformation
#' @param cutoff The x-value at which the sigmoidal transformation transitions from flat to steep
#'
#' @return A new transformation for use with ggplot2 that maps values to the sigmoidal scale
#' @export
#' @import scales
#'
sigmoidal_trans <- function(a = 40, cutoff = 0.05) {
  scales::trans_new("sigmoidal",
                    # The forward transformation maps x-values to y-values on the sigmoidal scale
                    function(x)
                      1 / (1 + exp(1 * a * (x - cutoff))),
                    # The reverse transformation maps y-values back to x-values on the original scale
                    function(x)
                      - 1 / a * (log(x) - log(1 - x)) + cutoff)
}

#' Create a heatmap of significant p-values from an ANOVA model
#'
#' @param catwalkdataset A `catwalkdataset` object containing the data and ANOVA models.
#' @param anova_results An integer specifying the index of the ANOVA model to use for the heatmap.
#' @return A `ggplot` object representing the heatmap.
#' @export
#' @import ggplot2
#' @import A86


catwalk_heatmap_aov <-
  function(catwalkdataset, anova_results = 1) {
    # extract data from the ANOVA results of the given index
    data_anova_results <- catwalkdataset@models[[anova_results]]

    # get the cutoff value from the ANOVA results
    cutoff <- data_anova_results$cutoff

    # extract the significant p-values from the ANOVA results
    data_to_graph <- data_anova_results$sig_pvals

    # create a heatmap plot with significant p-values
    heatmap <-
      ggplot2::ggplot(data_to_graph, aes(x = factor, y = parameter, fill = pval)) +
      ggplot2::geom_tile() +
      # set color scale using sigmoidal transformation
      ggplot2::scale_fill_gradient(
        high = "red",
        low = "blue",
        trans = "sigmoidal",
        # set color scale breaks to cutoff and multiples of cutoff
        breaks = c(cutoff / 5, cutoff, cutoff * 2, .99)
      ) +
      # apply custom theme from the theme_A86() function
      A86::theme_A86() +
      # set y-axis text size to 6
      ggplot2::theme(axis.text.y = element_text(size = 6))

    # return the heatmap plot
    heatmap
  }
#' Generate a Venn diagram from significant hits from an ANOVA model.
#'
#' @param catwalkdataset A CatWalk dataset object.
#' @param anova_results Index of the ANOVA model in the `catwalkdataset@models` list to use. Default is 1.
#' @param subset A function that will be applied to the hit list to determine which factors/combinations of factors to include in the graph. Default is NULL, which will include all hits.
#' @export
#' @return A ggvenn object representing the Venn diagram.
#' @import ggplot2
#' @import ggvenn


catwalk_venn <- function(catwalkdataset,
                         anova_results = 1,
                         subset = NULL) {
  # extract significant factors from ANOVA results
  hits <- catwalkdataset@models[[anova_results]]$hits

  # subset factors if subset argument provided
  if (!is.null(subset)) {
    hits <- hits[subset()]
  }

  # issue warning if more than 4 factors to be plotted
  if (length(hits) > 4) {
    warning(
      "Results have more than 4 factors/combinations of factors, will only graph first 4. Use the 'subset' argument to manually decide which factors to include in graph"
    )
  }

  # create Venn diagram using ggvenn
  venn <- ggvenn(hits) +
    theme(panel.background = element_rect(
      fill = "white",
      colour = "white",
      size = 0.5,
      linetype = "solid"
    ))
  venn
}




#' Embed data in 2D space using Uniform Manifold Approximation and Projection (UMAP) and visualize it.
#'
#' @param catwalkdataset A CatWalkDataset object.
#' @param dataname A string specifying the name of the data set to be used in the analysis. Default is "data_norm".
#' @param umap_config A list of configuration parameters for the UMAP algorithm, see `umap.defaults` in the umap package for a full list of available parameters.
#' @param return A string indicating the output format. Valid options are "plot" (default), "data", or "all".
#' @param color A string indicating the name of the column in the data set to be used for color-coding points in the plot.
#' default is for all points to be colored black.
#'
#' @return If \code{return} is "plot" (default), returns a ggplot object showing the UMAP projection of the data.
#' If \code{return} is "data", returns a data frame with the UMAP coordinates and the columns from the input data set.
#' If \code{return} is "all", returns a list with the UMAP configuration parameters, the input data set, and the ggplot object.
#' @import umap
#' @import ggplot2
#' @export
catwalk_umap <- function(catwalkdataset,
                         dataname = c("data_norm", "data_clean"),
                         umap_config = umap.defaults,
                         return = "plot",
                         color = "group") {
  # get the data set
  data <-
    catwalk_get_data_from_dataset(catwalkdataset, dataname = dataname)

  # perform the UMAP embedding
  umap <-
    umap(data %>% select(!c(animal, time_point, experiment)) %>% select_if(is.numeric), config = umap_config)

  # combine the UMAP coordinates with the input data set
  data_umap <- as.data.frame(umap$layout) %>% cbind(data)

  # determine what output format to return
  if (return == "data") {
    return(data_umap)
  } else {
    # plot the UMAP projection

    #look for color variable
    if (!is.null(color)){
      color <- data_umap %>% dplyr::pull(color)
    }


    plot <-
      ggplot(data_umap,
             aes(
               x = V2,
               y = V1,
               color = color
             )) +
      geom_point() + theme_A86() +
      theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank()
      )

    if (return == "plot") {
      return(plot)
    } else if (return == "all") {
      return(list(
        config = umap_config,
        data = data,
        plot = plot
      ))
    } else {
      stop("invalid 'return' argument provided. Valid inputs are 'data', 'plot', or 'all'")
    }
  }
}

#' @export
#' @import ggplot2
#' @import ggwordcloud
catwalk_tag_analysis <-
  function(catwalkdataset,
           anova_results = 1,
           factors = NULL,
           split_by = c("_", "[...]"),
           excluded_vars = character(),
           excluded_tags = "Mean",
           return = "plot") {
    data_anova_results = catwalkdataset@models[[anova_results]]
    dataname <- data_anova_results[["data_used"]]
    data <- catwalk_get_data_from_dataset(catwalkdataset, dataname)
    tags_all =  data.frame(
      tag = colnames(
        data %>%
          select(!c(time_point, animal, experiment)) %>%
          select(-matches(excluded_vars)) %>%
          select_if(is.numeric)
      ) %>%
        catwalk_variables_to_tags(split_by = split_by)
    )  %>%
      catwalk_process_tags(excluded_tags = excluded_tags)



    hits <- data_anova_results$hits
    if (!is.null(factors)) {
      missing_factors <- factors[!(factors %in% names(hits))]
      if (length(missing_factors) > 0) {
        if (length(missing_factors) == length(factors)) {
          stop("no factors found with the factor names provided")
        }
        else{
          warning(
            paste0(
              "the following factor could not be found and was not included in the analysis: ",
              missing_factors
            )
          )
        }
      }
      hits <- hits[names(hits) %in% factors]
    }
    tags_sig =  data.frame(tag = unique(unlist(hits)) %>%
                             catwalk_variables_to_tags(split_by = split_by)) %>%
      catwalk_process_tags(excluded_tags = excluded_tags)

    tags_pvals <-
      data.frame(
        tag = character(),
        pval_over = numeric(),
        pval_under = numeric(),
        n = numeric(),
        tot = numeric()
      )

    for (vartag in as.character(tags_sig$tag)) {
      overlap <-
        tags_sig %>% dplyr::filter(tag == vartag) %>% dplyr::pull(n) %>% as.numeric()
      m <-
        tags_all %>% dplyr::filter(tag == vartag) %>% dplyr::pull(n) %>% as.numeric()
      totaltags <- sum(tags_all$n) %>% as.numeric()
      totalsigtags <- sum(tags_sig$n) %>% as.numeric()
      pval_over <-
        phyper(overlap - 1, m, totaltags, totalsigtags, lower.tail = F)
      pval_under <-
        phyper(overlap, m, totaltags, totalsigtags, lower.tail = T)
      new_row <-
        data.frame(
          tag = vartag,
          pval_over = pval_over,
          pval_under = pval_under,
          n = overlap,
          tot = m
        )
      tags_pvals <- rbind(tags_pvals, new_row)
    }
    tags_pvals <- tags_pvals %>% arrange(pval_over, desc(n))
    if (return == "data") {
      return(tags_pvals)
    }

    wordcloud <-
      ggplot(tags_pvals, aes(
        label = tag,
        size = n,
        color = pval_over
      )) +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = 20) +
      scale_color_gradient(
        high = "red",
        low = "blue",
        trans = "sigmoidal",
        breaks = c(0.01, 0.05, 0.1, .99)
      ) +
      theme_minimal()
    if (return == "plot") {
      return(wordcloud)
    }
    if (return == "all") {
      return(list(data = tags_pvals, plot = wordcloud))
    }
    stop("invalid 'return' argument provided. Valid inputs are 'data', 'plot', or 'all'")
  }

#' Convert column names to tags for catwalk functions
#'
#' @param tags a character vector of column names to be converted to tags
#' @param split_by a character vector of strings to split the column names by
#' @return a character vector of tags
catwalk_variables_to_tags <- function(tags, split_by) {
  # Split the tags into multiple pieces based on strings in `split_by`
  for (tag in split_by) {
    tags <- tags %>% strsplit(tag) %>%
      unlist()
  }
  tags
}


#' Process tags for further analysis
#'
#' This function processes tags by filtering out excluded tags and empty tags,
#' counting the number of occurrences of each tag, and sorting the resulting
#' data frame in descending order of the number of occurrences.
#'
#' @param tags A data frame with one column called "tag" containing the tags
#' to be processed.
#' @param excluded_tags A character vector of tags to exclude from analysis.
#'
#' @return A data frame with one column called "tag" containing the tags that
#' occur more than once, and the number of occurrences of each tag.
catwalk_process_tags <- function(tags, excluded_tags) {
  # Filter out excluded tags and empty tags
  tags <- tags %>%
    dplyr::filter(!(tag %in% excluded_tags)) %>%
    dplyr::filter(tag != "") %>%
    # Count the number of occurrences of each tag
    count(tag) %>%
    # Filter out tags that occur only once
    dplyr::filter(n > 1) %>%
    # Sort the resulting data frame in descending order of the number of
    # occurrences
    arrange(desc(n))

  return(tags)
}
