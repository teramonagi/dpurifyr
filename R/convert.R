#' Convert data.frame to various format
#'
#' Convert data.frame to various format like matrix, vector
#'
#' @param .data
#'  A tbl.
#'
#' @return
#'  An object (matrix, vector etc.).
#' @name convert
NULL

#' @rdname convert
#' @export
to_matrix <- function(.data)
{
  apply_preprocessing_with_all_cols(.data, to_matrix_)
}

to_matrix_ <- function(x, arg=NULL, param=NULL)
{
  new_preprocessing(as.matrix(x), to_matrix_, param, arg)
}

#' @rdname convert
#' @export
to_vector <- function(.data)
{
  c(unlist(.data))
}

#' Printing Preprocessing object
#'
#' Print a preprocessed object.
#'
#' @param x
#' object of class data.frame.
#'
#' @param ...
#' optional arguments to print or plot methods.
#'
#' @export
print.preprocessed_data <- function(x, ...)
{
  base::print(x[,], ...)
}

