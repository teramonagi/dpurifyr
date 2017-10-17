#' Split date object into 4 components(year, month, day and the day of week)
#'
#' Split date object into 4 components(year, month, day and the day of week) to a format that
#' works better when you want to specify the effect of year, month, day or the day of week.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select] comparable.
#' @param sep
#'  Separator between columns.
#' @param format
#'  a character string of formats.
#'  It should include all the separators and each format must be prefixed with argument of strptime.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'  https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
#'  <prefix>weekday columns is the day of the week as a number (1-7, Sunday is 1).
#'
#' @export
split_date <- function(.data, ..., sep="_", format=NULL)
{
  apply_preprocessing(.data, ..., func=date_, arg=list(separator=sep))
}

date_ <- function(x, arg, param=NULL)
{
  df <- data.frame(
    year=lubridate::year(x),
    month=lubridate::month(x),
    day=lubridate::day(x),
    weekday=lubridate::wday(x)
  )
  new_preprocessing(add_prefix_to_name(df, arg$separator), date_, param, arg)
}

#' Split date object into 8 components(year, month, day, the day of week, hour, minute, second and timezone)
#'
#' Split date object into 8 components(year, month, day, the day of week, hour, minute, second and timezone)
#' to a format that works better when you want to specify the effect of year, month, day, the day of week, hour, minute, second or timezone.
#'
#' @param .data
#'  A tbl.
#' @param ...
#'  One or more unquoted expressions separated by commas. You can treat variable names like they are positions.
#'  It supports [dplyr::select] comparable.
#' @param sep
#'  Separator between columns.
#' @param format
#'  a character string of formats.
#'  It should include all the separators and each format must be prefixed with argument of strptime.
#'
#' @return
#'  An object of the same class as .data with "preprocessing-chain" attribution.
#'  <prefix>weekday columns is the day of the week as a number (1-7, Sunday is 1).
#'
#' @export
split_datetime <- function(.data, ..., sep="_", format=NULL)
{
  apply_preprocessing(.data, ..., func=datetime_, arg=list(separator=sep))
}

datetime_ <- function(x, arg, param=NULL)
{
  df <- data.frame(
    year=lubridate::year(x),
    month=lubridate::month(x),
    day=lubridate::day(x),
    weekday=lubridate::wday(x),
    hour=lubridate::hour(x),
    minute=lubridate::minute(x),
    second=lubridate::second(x),
    timezone=lubridate::tz(x),
    stringsAsFactors=FALSE
  )
  new_preprocessing(add_prefix_to_name(df, arg$separator), date_, param, arg)
}
