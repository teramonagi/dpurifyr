# Constant
ATTR_PREPROCESSING_CHAIN <- "preprocessing-chain"
ALL_COLUMN_APPLY <- "__ALL_COLUMN_APPLY__"

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
  attr(.data, ATTR_PREPROCESSING_CHAIN) <- NULL
  apply_chain(.data, preprocessing_chain(pp), use_param)
}

preprocessing_chain <- function(.data)
{
  attr(.data, ATTR_PREPROCESSING_CHAIN)
}

apply_chain <- function(.data, chain, use_param)
{
  if(!is.null(chain$previous)){
    .data <- apply_chain(.data, chain$previous, use_param)
  }

  # Memorize current preprocessing chains
  current_chains <- preprocessing_chain(.data)
  #For each preprocessing
  cs <- chain$content
  column_names <- names(cs)
  if(length(column_names) == 1 && column_names == ALL_COLUMN_APPLY){
    # All column apply
    pp <- cs[[1]]
    param <- if(use_param){pp$param}else{NULL}
    .data <- apply_preprocessing_with_all_cols(.data, func=function(x){pp$func(x, pp$arg, param)})
  } else{
    # For each column (same preprocessing)
    for(name in column_names){
      pp <- cs[[name]]
      df <- .data[name]
      param <- if(use_param){pp$param}else{NULL}
      res <- apply_preprocessing_with_selected_cols(df, names(df), func=function(x){pp$func(x, pp$arg, param)})
      if(ncol(res) != 1){
        .data <- replace_dataframe(.data, res, name, FALSE)
      } else{
        .data[name] <- res
      }
    }
  }
  enchain(.data, cs, current_chains)
}


apply_preprocessing <- function(.data, ..., func, arg)
{
  # Get selected columns using the backend of dplyr's select
  selected_cols <- tidyselect::vars_select(names(.data), ...)
  apply_preprocessing_with_selected_cols(.data, selected_cols, function(x){func(x, arg)})
}

replace_dataframe <- function(df_replaced, df_replacement, col, change_name)
{
  if(change_name){
    # Change names to "variablename__seperator__classname" style
    names(df_replacement) <- paste0(col, names(df_replacement))
  }

  # Insert df into .data at the point of "selected_cols[i]"
  index <- which(col == names(df_replaced))
  tibble::as_data_frame(append(df_replaced, df_replacement, index))[, -index]
}

apply_preprocessing_with_all_cols <- function(.data, func)
{
  # Memorize current preprocessing chains if .data has
  current_chains <- preprocessing_chain(.data)
  # Run pre-processing for all columns
  preprocessed <- func(.data)
  # Assign preprocessed-Data to .data
  .data <- preprocessed$data
  # Enchain current preprocessing with previous preproessing
  preprocessing <- list(preprocessed$preprocessing)
  names(preprocessing) <- ALL_COLUMN_APPLY
  enchain(.data, preprocessing, current_chains)
}

apply_preprocessing_with_selected_cols <- function(.data, selected_cols, func)
{
  # Memorize current preprocessing chains if .data has
  current_chains <- preprocessing_chain(.data)
  # Run pre-processing for all selected columns
  preprocessed <- lapply(.data[, selected_cols, drop=FALSE], func)
  # Assign preprocessed-Data to .data and make "preprocessing" object
  preprocessed_data <- purrr::map(preprocessed, "data")
  for(i in seq_along(preprocessed_data)){
    if(is.data.frame(preprocessed_data[[i]])){
      .data <- replace_dataframe(.data, preprocessed_data[[i]], selected_cols[i], TRUE)
    } else{
      .data[, selected_cols[i]] <- preprocessed_data[[i]]
    }
  }
  # Enchain current preprocessing with previous preproessing
  enchain(.data, purrr::map(preprocessed, "preprocessing"), current_chains)
}

# Add "preproessing-chein" attribution to .data
enchain <- function(.data, preprocessing, previous)
{
  attr(.data, ATTR_PREPROCESSING_CHAIN) <- list(content=preprocessing, previous=previous)
  class(.data) <- unique(c("preprocessed_data", class(.data)))
  .data
}

# Generate preprocessing class object with data=x and preprocessing=
new_preprocessing <- function(x, func, param, arg)
{
  preprocessing <- structure(list(func=func, param=param, arg=arg), class="preprocessing")
  structure(list(data=x, preprocessing=preprocessing))
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
  class_x <- class(x)
  class(x) <- class_x[class_x != "preprocessed_data"]
  base::print(x[,], ...)
}
