% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catwalk_normalize.R
\name{catwalk_normalize}
\alias{catwalk_normalize}
\title{Normalize catwalk data to a specified timepoint}
\usage{
catwalk_normalize(
  catwalkdataset,
  normtimepoint,
  dataname = "data_clean",
  keep_unnormed = T,
  keep_norm_time_point = T
)
}
\arguments{
\item{catwalkdataset}{A catwalk dataset object to be normalized}

\item{normtimepoint}{The timepoint to normalize to}

\item{dataname}{The name of the data to be normalized within the catwalk dataset}

\item{keep_unnormed}{Whether to keep unnormalized variables in the output dataset}

\item{keep_norm_time_point}{Whether to keep the normalization timepoint in the output dataset}
}
\value{
A catwalk dataset object with a new 'data_norm' list element containing the normalized data
}
\description{
Normalize catwalk data to a specified timepoint
}
