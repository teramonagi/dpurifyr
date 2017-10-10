#' Automatically import `%>%` opeator when call library("dpurifyr")
#'
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

dots_to_character <- function(...)
{
  vapply(rlang::exprs(...), deparse, character(1))
}
