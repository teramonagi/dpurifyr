preprocessing_chain <- function(.data)
{
  attr(.data, "preprocessing-chain")
}

#' @importFrom dplyr if_else
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

#' @export
apply <- function(.data, pp, use_param=TRUE)
{
  apply_chain(.data, preprocessing_chain(pp), use_param)
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
  .data[, selected_cols] <- purrr::map(preprocessed, "data")
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

