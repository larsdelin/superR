df_import <- function(){
  tryCatch({df <- file.choose()}, error = function(ex){})
  if(is.null(df)){
    cat("You pressed cancel, meaning you did not select a file.")
    tryagain <- readline("If you would like to go back to the options, type !!!, otherwise type anything or hit the return button: ")
    back(tryagain, df_select())
    df_import()
  }
  return(df)
}

df_import()
df

tryCatch({df <- file.choose()}, error = function(ex){})
new <- import(df)
