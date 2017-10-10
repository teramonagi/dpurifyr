preprocessing_chain <- function(.data)
{
  attr(.data, "chain")
}

#' @importFrom tidyselect vars_select
apply_preprocessing <- function(.data, ..., func)
{
  selected_cols <- tidyselect::vars_select(names(.data), ...)
  # run pre-processing for all selected columns
  preprocessed <- lapply(.data[, selected_cols, drop=FALSE], func)
  # Assign Data and make "preprocessing" object
  .data[, selected_cols] <- purrr::map(preprocessed, "data")
  new_preprocessing(.data, purrr::map(preprocessed, "preprocessing"), preprocessing_chain(.data))
}

new_preprocessing <- function(.data, preprocessing, previous)
{
  attr(.data, "chain") <- list(content=preprocessing, previous=previous)
  class(.data) <- c(class(.data), "preprocessing")
  .data
}
