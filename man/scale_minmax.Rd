% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale.R
\name{scale_minmax}
\alias{scale_minmax}
\title{Scaling to a given range.}
\usage{
scale_minmax(.data, ..., range = c(0, 1))
}
\arguments{
\item{.data}{A tbl.}

\item{...}{One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
It supports [dplyr::select]  comparable.}

\item{range}{Desired range of transformed data.}
}
\value{
An object of the same class as .data with "preprocessing-chain" attribution.
}
\description{
Scales and translates value such that it is in the given range, i.e. between zero and one.
}
\details{
The transformation is given by:
 X_std = (X - X.min(axis=0)) / (X.max(axis=0) - X.min(axis=0))
 x_transformed = (x - min(x))/(max(x) - min(x)) + min(x)

This transformation is often used as an alternative to zero mean, unit variance scaling.
}
