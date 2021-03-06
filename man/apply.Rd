% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing.R
\name{apply}
\alias{apply}
\title{Apply preprocessing object to new data (tbl)}
\usage{
apply(.data, pp, use_param = TRUE)
}
\arguments{
\item{.data}{a tbl.}

\item{pp}{preprocessing object}

\item{use_param}{A logical value. Wether use the same parameter created by preprocessing object}
}
\value{
An object of the same class as .data with "preprocessing-chain" attribution.
}
\description{
Returns a new preprocessing object obtained by applying a preprocessing object to new data
}
