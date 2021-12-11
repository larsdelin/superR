df_options <- function(option){
  back(option, df_select())
  if(option == "1"){
    cat("\nYou chose to import a dataframe.
You will have to give your dataframe a name.")
    dfname <- naming("df")
    back(dfname, df_select())
    checkname(dfname, df_select(), df_options("1"), "df")
    df <- df_import()
    df <- import(df)
    assign(dfname[1], df, envir = .GlobalEnv)
    main()
  }
  else if(option == "2"){
    cat("\nYou chose to use an existing dataframe in your global environment.")
    dfname <- readline("Enter in the name of the dataframe: ")
    back(dfname, df_select())
    if(exists(dfname) == FALSE){
      cat("\nThe name you entered does not match any dataframe currently in your global environment.
Please enter the name of the dataframe you would like to use currently in your global environment. 
If the data frame you would like to use is not in your global environment, 
enter !!! to go back to the options for choosing your dataframe and pick option 1: Import new dataframe\n")
      df_options("2")}
    main()
  }
  else if(option != "1" | option != "2"){
    cat("\nYou did not select one of the two options.")
    option <- readline("Please enter the number associated with the option: ")
    df_options(option)}
}

df_select()

testdata <- import("email.csv")




