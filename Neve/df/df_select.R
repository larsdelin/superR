df_select <- function(){
  cat(bold("\n\nChoose a Dataframe")) 
  cat("\n\nDo you want to:
  1. Import new dataframe
  2. Choose a dataframe already loaded in your global environment")
  option <- readline("Type the number associated: ")
  
  df_options(option)
  main()
}
