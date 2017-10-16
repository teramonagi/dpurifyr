insert_data_frame <- function(df, df_added, col)
{
  index <- which(col == names(df))
  if(index == 1){
    cbind(df_added, df[, -index, drop=FALSE])
  } else if(index == ncol(df)){
    cbind(df[, -index, drop=FALSE], df_added)
  } else{
    cbind(
      df[, 1:(index-1), drop=FALSE],
      df_added,
      df[, (index+1):ncol(df), drop=FALSE]
    )
  }
}
