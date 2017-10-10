#' @export
scale <- function (x, center=TRUE, scale=TRUE){UseMethod("scale")}

#' Get JPGIS2.1 Data
#'
#' @param .data
#'
#' @export
scale.data.frame <- function(.data, ..., center=TRUE, scale=TRUE)
{
  apply_preprocessing(.data, dots_to_character(...), preprocessing_chain(.data), center, scale)
}

scale_numeric <- function(x, center, scale)
{
  m <- base::mean(x)
  s <- stats::sd(x)
  preprocessing <- structure(list(mean=m, std=s), class="dpurifyr.scale")
  structure(list(data=(x-m)/s, preprocessing=preprocessing))
}
