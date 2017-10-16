#' Apply preprocessing object to new data (tbl)
#'
#' Returns a new preprocessing object obtained by applying a preprocessing object to new data
#'
#' @param .data
#'  a tbl.
#' @param pp
#'  preprocessing object
#' @param use_param
#'  A logical value. Wether use the same parameter created by preprocessing object
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'
#' @export
apply <- function(.data, pp, use_param=TRUE)
{
  apply_chain(.data, preprocessing_chain(pp), use_param)
}

preprocessing_chain <- function(.data)
{
  attr(.data, "preprocessing-chain")
}

apply_chain <- function(.data, chain, use_param)
{
  if(!is.null(chain$previous)){
    .data <- apply_chain(.data, chain$previous, use_param)
  }

  cs <- chain$content
  cns <- names(cs)
  .data[cns] <- lapply(cns, function(name){
    pp <- cs[[name]]
    df <- .data[name]
    param <- if(use_param){pp$param}else{NULL}
    apply_preprocessing_with_selected_cols(df, names(df), func=function(x){pp$func(x, pp$arg, param)})
  })
  enchain(.data, cs, preprocessing_chain(.data))
}


apply_preprocessing <- function(.data, ..., func, arg)
{
  # Get selected columns using the backend of dplyr's select
  selected_cols <- tidyselect::vars_select(names(.data), ...)
  apply_preprocessing_with_selected_cols(.data, selected_cols, function(x){func(x, arg)})
}

apply_preprocessing_with_selected_cols <- function(.data, selected_cols, func)
{
  # run pre-processing for all selected columns
  preprocessed <- lapply(.data[, selected_cols, drop=FALSE], func)
  # Assign Data and make "preprocessing" object
  data <- purrr::map(preprocessed, "data")
  for(i in seq_along(data)){
    if(is.data.frame(data[[i]])){
      # Change names to "variablename__seperator__classname" style
      df <- data[[i]]
      names(df) <- paste0(selected_cols[i], names(df))
      # Insert df into .data at the point of "selected_cols[i]"
      index <- which(selected_cols[i] == names(.data))
      .data <- tibble::as_data_frame(append(.data, df, index))[, -index]
    } else{
      .data[, selected_cols[i]] <- data[[i]]
    }
  }
  # Enchain current preprocessing with previous preproessing
  enchain(.data, purrr::map(preprocessed, "preprocessing"), preprocessing_chain(.data))
}

enchain <- function(.data, preprocessing, previous)
{
  attr(.data, "preprocessing-chain") <- list(content=preprocessing, previous=previous)
  .data
}

new_preprocessing <- function(x, func, param, arg)
{
  preprocessing <- structure(list(func=func, param=param, arg=arg), class="preprocessing")
  structure(list(data=x, preprocessing=preprocessing))
}

