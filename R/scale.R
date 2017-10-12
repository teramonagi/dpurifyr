#' Standardizing tbl
#'
#' Standardize each columns by removing the mean and scaling to unit standard deviation
#' Centering and scaling happen independently on each columns which you selected.
#' Mean and standard deviation are then stored to be used on later data using the transform method.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select]  comparable.
#' @param center
#'  A logical value.
#' @param scale
#'  A logical value.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @details
#' The value of `center` determines how column centering is performed.
#' If center is TRUE then centering is done by subtracting the column means (omitting NAs) of x from their corresponding columns,
#' and if center is FALSE, no centering is done.
#'
#' The value of `scale` determines how column scaling is performed (after centering).
#' If scale is TRUE then scaling is done by dividing the (centered) columns of x by their standard deviations if center is TRUE,
#' and the root mean square otherwise. If scale is FALSE, no scaling is done.
#'
#' @export
scale_standard <- function(.data, ..., center=TRUE, scale=TRUE)
{
  apply_preprocessing(.data, ..., func=standard_, arg=list(center=center, scale=scale))
}

standard_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(mean=base::mean(x), std=stats::sd(x))
  }

  m <- param$mean
  s <- param$std
  new_preprocessing((x-m)/s, standard_, param, arg)
}



#' Scaling to a given range.
#'
#' Scales and translates value such that it is in the given range, i.e. between zero and one.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select]  comparable.
#' @param range
#' Desired range of transformed data.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @details
#'The transformation is given by:
#'  X_std = (X - X.min(axis=0)) / (X.max(axis=0) - X.min(axis=0))
#'  x_transformed = (x - min(x))/(max(x) - min(x)) + min(x)
#'
#' This transformation is often used as an alternative to zero mean, unit variance scaling.
#' @export
scale_minmax <- function(.data, ..., range=c(0, 1))
{
  range <- sort(range)
  apply_preprocessing(.data, ...,func= minmax_, arg=list(range=range))
}

minmax_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(x_range=base::range(x, na.rm=TRUE, finite=TRUE))
  }

  range <- arg$range
  x_range <- param$x_range
  x_std = (x - x_range[1]) / diff(x_range)
  x_scaled =  x_std * diff(range) + range[1]

  new_preprocessing(x_scaled, minmax_, param, arg)
}
