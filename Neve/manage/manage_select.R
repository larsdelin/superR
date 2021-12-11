manage_select <- function(){
  cat(bold("\n\nCManage/Change the Contents of your Dataframe"))

cat("\n\nDo you want to:
  1. Subset your data
  2. Change existing variables
  3. Create new variables")
  
  option <- readline("Type the number associated: ")
  
  manage_options(option)
}
