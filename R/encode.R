#' Encode labels with value
#'
#' Simply converting each values (character or factor) in a column to a specific number.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select]  comparable.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @details
#' The mapped value start from 1 (not 0).
#'
#' @export
encode_label <- function(.data, ...)
{
  apply_preprocessing(.data, ...,func=label_, arg=NULL)
}

label_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(levels=sort(unique(x)))
  }

  new_preprocessing(match(x, param$levels), label_, param, arg)
}

#' Replace labels with their count in the data set
#'
#' Simply converting each values (character or factor) in a column to the count of each values
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select]  comparable.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @details
#' It can be sensitive to outliers.
#' It might be a good strategy to add log-transformation
#'
#' @export
encode_count <- function(.data, ...)
{
  apply_preprocessing(.data, ...,func=count_, arg=NULL)
}

count_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(table=table(x))
  }

  new_preprocessing(as.vector(param$table[x]), count_, param, arg)
}


