print.preprocessing <- function(x)
{
  print(x$data)
}

#' @export
scale <- function (x, ...) {UseMethod("scale", x)}
scale.numeric <- function(x, with_mean=TRUE, with_std=TRUE)
{
  m <- mean(x)
  s <- sd(x)
  preprocessing <- structure(list(mean=m, std=s), class="scale")
  structure(list(data=(x-m)/s, preprocessing=preprocessing, previous=NULL), class=c("preprocessing"))
}
evaluated_col_names <- function(.data, ...)
{
  col_name <- rlang::set_names(as.list(names(.data)), names(.data))
  purrr::map_chr(rlang::quos(...), rlang::eval_tidy, data=col_name)
}
scale.data.frame <- function(.data, ...)
{
  # specify selected columns
  selected_cols <- evaluated_col_names(.data, ...)
  # run pre-processing for all selected columns
  preprocessed <- lapply(.data[, selected_cols], scale)
  # Assign Data and make "preprocessing" object
  .data[, selected_cols] <- purrr::map(preprocessed, "data")
  preprocessing <- purrr::map(preprocessed, "preprocessing")
  structure(list(data=.data, preprocessing=preprocessing, previous=NULL), class=c("preprocessing"))
}
scale.preprocessing <- function(pp, ...)
{
  # specify selected columns
  selected_cols <- evaluated_col_names(pp$data, ...)
  # run pre-processing for all selected columns
  preprocessed <- lapply(pp$data[, selected_cols, drop=FALSE], scale)
  # Copy original data and make "preprocessing" object
  data <- pp$data
  data[, selected_cols] <- purrr::map(preprocessed, "data")
  preprocessing <- purrr::map(preprocessed, "preprocessing")
  structure(list(data=data, preprocessing=preprocessing, previous=pp), class=c("preprocessing"))
}
as.data.frame.preprocessing <- function(pp)
{
  as_data_frame(pp$data)
}
