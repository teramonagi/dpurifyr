add_prefix_to_name <- function(df, prefix)
{
  stats::setNames(df, paste0(prefix, names(df)))
}
