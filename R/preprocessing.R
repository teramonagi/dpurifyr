preprocessing_chain <- function(.data)
{
  attr(.data, "chain")
}

apply_preprocessing <- function(.data, selected_cols, previous, ...)
{
  # run pre-processing for all selected columns
  preprocessed <- lapply(.data[, selected_cols, drop=FALSE], scale_numeric, ...)
  # Assign Data and make "preprocessing" object
  .data[, selected_cols] <- purrr::map(preprocessed, "data")
  new_preprocessing(.data, purrr::map(preprocessed, "preprocessing"), previous)
}

new_preprocessing <- function(.data, preprocessing, previous)
{
  attr(.data, "chain") <- list(content=preprocessing, previous=previous)
  class(.data) <- c(class(.data), "preprocessing")
  .data
}
