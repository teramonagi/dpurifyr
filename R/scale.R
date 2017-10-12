#' Standardize each columns by removing the mean and scaling to unit standard deviation
#'
#' Centering and scaling happen independently on each feature by computing the relevant statistics on .
#' Mean and standard deviation are then stored to be used on later data using the transform method.
#'
#' @param .data
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
