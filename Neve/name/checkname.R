checkname <- function(name, fun, tryagain, datatype){
  back(name, fun)
  if(!(substr(name, 1, 1) %in% letters | substr(name, 1, 1) %in% LETTERS | substr(name, 1, 1) == ".")){
    cat(red("\nThe name you chose does not begin with a letter or a period (.).\n"))
    tryagain
  }
  if(illegal(name) == TRUE){
    cat(red("\nThe name you chose contains illegal characters or spaces."))
cat("\nIllegal characters are symbols/punctuation other than a period (.), dash (-), or underscore (_)\n")
    tryagain
  }
  if(datatype == "df"){
    if(exists(name) == TRUE){
      cat(red("\nThe name you chose is one that is already being used by something in your global environment."))
cat("\nYou may want to provide a different name.
If you use choose to use this name, it will replace the other element, even if it is not a dataframe.\n")
    rename <- readline("If you want to use this name (replace the other) type YES. If not, type anything (except YES) or press return ")
    back(name, fun)
    if(rename != "YES"){
      tryagain}
    }}
}

checkname("4me", df_select(), df_options("1"), "df")






         
          


