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
  scale_ <- function(x, center, scale)
  {
    m <- base::mean(x)
    s <- stats::sd(x)
    preprocessing <- structure(list(mean=m, std=s), class="dpurifyr.scale_standarad")
    structure(list(data=(x-m)/s, preprocessing=preprocessing))
  }
  apply_preprocessing(.data, ..., func=function(x){scale_(x, center, scale)})
}

#' @export
scale_minmax <- function(.data, ..., range=c(0, 1))
{
  range <- sort(range)
  scale_ <- function(x, range)
  {
    x_range <- base::range(x, na.rm=TRUE, finite=TRUE)
    x_std = (x - x_range[1]) / diff(x_range)
    x_scaled =  x_std * diff(range) + range[1]
    preprocessing <- structure(list(x_range=x_range, range=range), class="dpurifyr.scale_minmax")
    structure(list(data=x_scaled, preprocessing=preprocessing))
  }
  apply_preprocessing(.data, ..., func=function(x){scale_(x, range)})
}
