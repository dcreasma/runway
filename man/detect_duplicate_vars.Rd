% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CatwalkDataset.R
\name{detect_duplicate_vars}
\alias{detect_duplicate_vars}
\title{Checks if a data set contains more than one variable containing a given string}
\usage{
detect_duplicate_vars(data, var_name)
}
\arguments{
\item{data}{A dataframe to check for duplicated variables.}

\item{var_name}{A string to look for in the dataframe's variable names.}
}
\value{
The only variable containing a given string (if only one) or the variable selected by the user if multiple variables contain the same string.
}
\description{
Checks if a data set contains more than one variable containing a given string
}
