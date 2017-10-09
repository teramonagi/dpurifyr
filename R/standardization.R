#' @export
scale <- function (x, center=TRUE, scale=TRUE){UseMethod("scale")}

#' @export
scale.data.frame <- function(.data, ..., center=TRUE, scale=TRUE)
{
  apply_preprocessing(.data, dots_to_character(...), preprocessing_chain(.data), center, scale)
}

scale_numeric <- function(x, center, scale)
{
  m <- mean(x)
  s <- sd(x)
  preprocessing <- structure(list(mean=m, std=s), class="dpurifyr.scale")
  structure(list(data=(x-m)/s, preprocessing=preprocessing))
}
