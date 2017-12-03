#' Encode labels with value
#'
#' Simply converting each values (character or factor) in a column to a specific number.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select]  comparable.
#' @param start
#'  Integer, default 0.
#'  label starts.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @details
#' The mapped value start from 1 (not 0).
#'
#' @export
encode_label <- function(.data, ..., start=0)
{
  apply_preprocessing(.data, ..., func=label_, arg=list(start=start))
}

label_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(levels=sort(unique(x)))
  }

  new_preprocessing(match(x, param$levels)-1+arg$start, label_, param, arg)
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
  apply_preprocessing(.data, ..., func=count_, arg=NULL)
}

count_ <- function(x, arg, param=NULL)
{
  if(is.null(param)){
    param <- list(table=table(x))
  }

  new_preprocessing(as.vector(param$table[x]), count_, param, arg)
}

#' Encode categorical data using a one-hot aka one-of-K scheme
#'
#' Transforms categorical data (factor or character ot integer) to a format that
#' works better with classification and regression algorithms.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select] comparable.
#' @param drop_first
#'  bool, default TRUE
#'  Whether to get k-1 dummies out of k categorical levels (factor) by removing the first level.
#' @param sep
#'  Separator between columns.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @export
encode_onehot <- function(.data, ..., drop_first=TRUE, sep="_")
{
  apply_preprocessing(.data, ..., func=onehot_, arg=list(drop_first=drop_first, separator=sep))
}

onehot_ <- function(x, arg, param=NULL)
{
  x <- as.character(x)
  if(is.null(param)){
    param <- list(class=sort(unique(x)))
  }
  class <- param$class
  # Maps of class <--> its index
  index <- stats::setNames(seq_along(class), class)
  # Assine 1 value to matched column corresponding to categorical value
  xm <- matrix(0, nrow=length(x), ncol=length(class), dimnames=list(NULL, class))
  xm[cbind(seq_along(x), index[x])] <- 1
  df <- as.data.frame(xm)
  if(arg$drop_first){
    df <- df[, -1]
  }
  new_preprocessing(add_prefix_to_name(df, arg$separator), onehot_, param, arg)
}

