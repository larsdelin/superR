super <- function(){
  e.super <- new.env(parent = emptyenv())
  e.super$df <- NULL
  e.super$dfname <- NULL
  cat("\nWelcome to superR! This is an easy way to get your dataframe set up for analysis.
...

If you would like to go back to previous options, type !!!

If you would like to go back to the main menu of options, type !!!main

If at any point you would like to exit the program, press the esc key

Let's begin!")
  
  cat("\n\nTo get started, you will need a dataframe.")

  # Back: -----------
  #_________________________________
  back <- function(x, backto, df = e.super$df, dfname = e.super$dfname){
    if(x == "!!!"){
      backto}
    if(x == "!!!main"){
      if(!(is.null(df) | is.null(dfname))){
      main()}
      else{df_select()}
    }
  }
  options <- function(option, validoptions, backto, df = e.super$df, dfname = e.super$dfname){
    if(option %in% validoptions | option == "!!!" | option == "!!!main"){
      if(option == "!!!"){
        backto}
      else if(option == "!!!main"){
        if(!(is.null(df) | is.null(dfname))){
        main()}
        else{backto}}
      else{return(option)}
    }
    else{
      rm(option)
      cat(red$bold("\nError: "), red("You did not select one of the options."),
"\n> If you would like to go back, enter ", green$bold("!!!"), sep = "")
      if(!(is.null(df) | is.null(dfname))){
        cat("\n> If you would like to go back to the main menu, enter ", green$bold("!!!main"), sep = "")}
      option <- readline("Please enter the number associated with the option: ")
      options(option, validoptions, backto)
    }
  }
  
  # Name: -----------
  checkname <- function(name, nametype, backto){
    if(!(substr(name, 1, 1) %in% letters | substr(name, 1, 1) %in% LETTERS | substr(name, 1, 1) == ".")){
      rm(name)
      cat(red$bold("\nError: "), red("The name you chose does not begin with a letter or a period (.)."), sep = "")
      name <- naming(nametype, backto)
    }
    else if(illegal(name) == TRUE){
      rm(name)
      cat(red$bold("\nError: "), red("The name you chose contains illegal characters or spaces."), sep = "")
      cat("\nIllegal characters are symbols/punctuation other than a period (.), dash (-), or underscore (_)")
      name <- naming(nametype, backto)
    }
    else{name <- existingname(name, nametype, backto)}
    return(name)
  }
  existingname <- function(name, nametype, backto, df = e.super$df, dfname = e.super$dfname){
    if(nametype == "dataframe"){
      if(exists(name, envir = .GlobalEnv, inherits = FALSE)){
        cat(red$bold("\nWarning: "), red("The name you chose is one that is already being used by something in your global environment."), 
"\nYou may want to provide a different name.
If you use choose to use this name, it will ", red$bold("replace"), " the other element, even if it is not a dataframe.

Do you want to:
  1. Use this name (", red$bold("replace"), " the other)
  2. Choose a different name", sep = "")
        option <- readline("Type the number associated: ")
        option <- options(option, c("1", "2"), naming(nametype, backto))
        if(option == "2"){
          rm(name)
          name <- naming(nametype, backto)
          return(name)
        }
        else{
          return(name)
        }
      }
      else{return(name)}}
    if(nametype == "variable"){}
  }
  illegal <- function(name){
    name_sep <- unlist(strsplit(name, ""))
    error <- c()
    for(i in name_sep){
      error[[i]] <- ifelse(identical(".", i) == TRUE | 
                             identical("_", i) == TRUE | 
                             identical("-", i) == TRUE, FALSE,
                           ifelse(grepl("[[:punct:]]", i) == TRUE, TRUE, 
                                  ifelse(identical(" ", i) == TRUE, TRUE, FALSE)))}
    error <- unlist(error)
    "TRUE" %in% error}
  naming <- function(nametype, backto, df = e.super$df, dfname = e.super$dfname){
    cat("\nWhen naming a ", nametype , ", here are some things to be aware of.
The name should:
  - Be one word (no spaces)
  - Be concise and descriptive of the dataframe
  - Start with a letter or a period (.) -- should not start with a number or symbol other than a period (.)
  - Avoid using punctuation other than a period (.), dash (-), or underscore (_)
  - Avoid using a name that is already being used
      
Do you want to see examples of ", nametype, " names?
  1. Yes
  2. No", sep = "")
    
    option <- readline("Type the number associated: ")
    option <- options(option, c("1", "2"), backto)
    if(option == "1"){
      if(nametype == "dataframe"){
        cat("\nLet's say you want to name a dataframe that contains info on 100 patients at a hospital

Good examples:
  - patients
  - .HospitalPatients
  - patients100
  - patients_df

Bad examples:
  - 100 patients at a hospital
  - _Patients
  - patients??
  - 100patients\n")}
      if(nametype == "variable"){
        cat("\nLet's say you want to name a variable that shows the sex of a patient at a hospital

Good examples:
  - sex
  - PatientSex
  - .sex2
  - patients_df

Bad examples:
  - sex patients at a hospital
  - _sex
  - sex??
  - 2sex\n")}
    }
    if(nametype == "dataframe"){
      name <- readline("Enter what you would like to name the dataframe: ")}
    if(nametype == "variable"){
      name <- readline("Enter what you would like to name the variable: ")}
    back(name, backto)
    name <- checkname(name, nametype, backto)
    return(name)
  }
  
  # Variable Name: -----------
  varname <- function(dothis, backto, df = e.super$df, dfname = e.super$dfname){
    cat("\nThe variables in your dataframe are:\n  - ", paste(names(df), sep = "", collapse = "\n  - "), 
"\n\nWhen entering in the variable(s), list them out with spaces in between them:

examples:
  - var1
  - var1 var2 var5

non-examples:
  - var1, var2, var5
  - var1 and var2
  - var1+var2
  - all except var1 and var2

Which variable(s) would you like to ", bold(dothis), "?", sep = "")
    variables <- readline("Enter in the variable(s): ")
    back(variables, backto)
    checkvarname(variables, dothis, backto)
    return(variables)
  }
  checkvarname <- function(variables, dothis, backto, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    valid <- variablessplit %in% names(df)
    if(FALSE %in% valid){
      rm(variables)
      cat(red$bold("\nError: "), red("You entered an invalid variable or list of variables."),
"\nThis means you either inputted your variable(s) in wrong, 
or one or more of the variables you entered is not one of the variable names in your dataframe.", sep = "")
      varname(dothis, backto)
    }
    else{varconfirm(variables, dothis, backto)}
  }
  varconfirm <- function(variables, dothis, backto, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    cat("\nThe variable(s) you selected are:\n  - ", paste(variablessplit, collapse = "\n  - "), 
        "\n\nAre you sure you would like to ", bold(dothis), " the variable(s) and save it in the dataframe '", 
        underline(dfname), "'?
  1. Yes
  2. No", sep = "")
    option <- readline("Type the number associated: ")
    option <- options(option, c("1", "2"), backto)
    if(option == "2"){
      rm(variables)
      varname(dothis, backto)
    }
  }
  
  # Main: -----------
  main <- function(){
    cat(bold("\nWelcome to the "), bold$underline("Main Menu"),
        "\nHere, you will see the different 'main' options you have for this program
      
If at any point you would like to come back to the main menu, just type ", green$bold("!!!main"), 
"\n\nHere is the menu of overaching options:
  1. Choose a Dataframe
  2. Understand your Dataframe
  3. Manage/Change the Contents of your Dataframe
  4. Analyse you Dataframe
      
If you would like to learn more about an option, 
enter below the number associated, immediately followed by HELP
  - 1HELP
  - 2HELP
  - 3HELP
  - 4HELP", sep = "")
    
    option <- readline("Type the number associated or (the number)HELP: ")
    main_options(option)
    
  }
  main_options <- function(option){
    if(option == "1HELP"){
      rm(option)
      cat(bold("\n\nHELP: Choose a Dataframe"), 
          
          "\n\nYou have the choice to:
  1. Import new dataframe
  2. Choose a dataframe already loaded in your global environment", sep = "")
      readline("Type anything or hit the return button: ")
      main()
    }
    else if(option == "2HELP"){
      rm(option)
      cat(bold("\n\nHELP: Understand your Dataframe"), 
          
          "\n\nYou have the choice to:
...", sep = "")
      
      readline("Type anything or hit the return button: ")
      main()
    }
    else if(option == "3HELP"){
      rm(option)
      cat(bold("\n\nHELP: Manage/Change the Contents of your Dataframe"), 
          
          "\n\nYou have the choice to:
...", sep = "")
      readline("Type anything or hit the return button: ")
      main()
    }
    else if(option == "4HELP"){
      rm(option)
      cat(bold("\n\nHELP: Analyse you Dataframe"), 
          
          "\n\nYou have the choice to:
...", sep = "")
      readline("Type anything or hit the return button: ")
      main()
    }
    else if(option == "1"){
      df_select()
    }
    else if(option == "2"){
      
    }
    else if(option == "3"){
      manage_select()
    }
    else if(option == "4"){
      
    }
    else{
      rm(option)
      cat(red$bold("\nError: "), red("You did not select one of the options or the help for an option."), sep = "")
      option <- readline("Type the number associated or (the number)HELP: ")
      main_options(option)
    }
  }
  
  # 1. Dataframe: -----------
  df_select <- function(){
    cat(bold("\n\nChoose a Dataframe"), "\n\nDo you want to:
  1. Import a dataframe
  2. Use an existing dataframe in your global environment", sep = "")
    option <- readline("Type the number associated: ")
    df_options(option)
    main()
  }
  df_options <- function(option, df = e.super$df, dfname = e.super$dfname){
    if(is.null(df) | is.null(dfname)){
      option <- options(option, c("1", "2"), df_select()) 
    }
    else{option <- options(option, c("1", "2"), main())}
    
    if(option == "1"){
      df_import()
    }
    if(option == "2"){
      cat(bold("\nUse an Existing Dataframe in your Global Environment"))
      df_existing()
    }
  }
  df_existing <- function(df = e.super$df, dfname = e.super$dfname){
    name <- readline("Enter in the name of the dataframe: ")
    back(name, df_select())
    if(exists(name, envir = .GlobalEnv) && is.data.frame(get(name, envir = .GlobalEnv))){
      e.super$dfname <- name
      e.super$df <- get(name, envir = .GlobalEnv)}
    else{
      rm(name)
      cat(red$bold("\nError: "), red("The name you entered does not match any dataframe currently in your global environment."),
          "\nPlease enter the name of the dataframe you would like to use currently in your global environment. 
> If the dataframe you would like to use is not in your global environment, 
  enter ", green$bold("!!!"), " to go back to the options for choosing your dataframe and pick option 1: Import a Dataframe.\n", sep = "")
      if(!(is.null(df) | is.null(dfname))){
        cat("> If you would like to use the dataframe '", underline(dfname), "' that is already loaded in the program,
    enter ", green$bold("!!!main"), " to go back to the main menu.", sep = "") 
      }
      df_existing()
    }
  }
  df_import <- function(){
    cat(bold("\nImport a Dataframe"), "\n\nYou will have to give your dataframe a name.", sep = "")
    e.super$dfname <- naming("dataframe", df_select())
    e.super$df <- import()
    assign(e.super$dfname[1], e.super$df, envir = .GlobalEnv)
    main()
  }
  import <- function(){
    file <- tryCatch({file <- file.choose()}, error = function(ex){})
    if(is.null(file)){
      cat("You pressed cancel, meaning you did not select a file.")
      again <- readline("Type anything to choose again, or !!! to go back to the options: ")
      back(again, df_select())
      import()
    }
    else{
      file <- tolower(file)
      basename <- basename(file)
      extension <- tools::file_ext(file)
      
      df <- switch(extension,
                   "sas7bdat" = haven::read_sas(file, ...),
                   "dta" = haven::read_stata(file, ...),
                   "sav" = haven::read_spss(file, ...),
                   "xlsx" = readxl::read_excel(file, ...),
                   "xls" = readxl::read_excel(file, ...),
                   vroom::vroom(file, ...))
      return(df)
    }
  }
  
  # 2. Summarize: -----------
  
  # 3. Manage: -----------
  manage_select <- function(){
    cat(bold("\n\nManage/Change the Contents of your Dataframe"))
    
    cat("\n\nDo you want to:
  1. Subset your data
  2. Change existing variables
  3. Create new variables")
    
    option <- readline("Type the number associated: ")
    manage_options(option)
  }
  manage_options <- function(option){
    
    back(option, main())
    if(option == "1"){
      subset()
    }
    else if(option == "2"){
      changevar_select()
    }
    else if(option == "3"){
      createvar_select()
    }
    else{
      rm(option)
      cat(red$bold("\nError: "), red("You did not select one of the options."), 
"\n> If you would like to go back to the main menu, enter ", green$bold("!!!"), " or ", green$bold("!!!main"), sep = "")
      option <- readline("Please enter the number associated with the option: ")
      manage_options(option)
    }
  }
  subset <- function(df = e.super$df, dfname = e.super$dfname){
    cat(bold("\nSubset your Data"),
        "\n\nBefore you do any subsetting, would you like this subsetting to occur in your dataframe '", 
        underline(dfname), "', 
or in a copy of your dataframe '", underline(dfname), "'?

Would you like to:
  1. Create a copy of your dataframe '", underline(dfname), "': subset in a copy 
     to  preserve '", underline(dfname), "'  in your global environment
  2. Use the dataframe '", underline(dfname), "': subset in '", underline(dfname), "'", sep = "")
    option <- readline("Type the number associated: ")
    option <- options(option, c("1", "2"), manage_select())
    if(option == "1"){
      cat("\nYou will have to provide a name for your subset of the dataframe '", underline(dfname), "'", sep = "")
      e.super$dfname <- naming("dataframe", subset())
      assign(e.super$dfname[1], df, envir = .GlobalEnv)
    }
    subset_select()
  }

  #_______________________________________________
  # Subset:
  
  subset_select <- function(){
    cat(bold("\n\nSubsetting Options"), 
"\n\nDo you want to:
  1. Remove specific variables
  2. Keep specific variables
  3. Remove specific observations
  4. Keep specific observations
  5. Group the observations by variables
  6. Gather
  7. Spread", sep = "")
    option <- readline("Type the number associated: ")
    option <- options(option, c(1:7), manage_options("1"))
    subset_options(option)
  }
  subset_options <- function(option, df = e.super$df, dfname = e.super$dfname){
    if(option == "1"){
      cat(bold("\nRemove Specific Variables"))
      variables <- varname("remove", subset_select())
      remove_vars(variables)
      subset_select()
    }
    if(option == "2"){
      cat(bold("\nKeep Specific Variables"))
      variables <- varname("keep", subset("2"))
      keep_vars(variables)
      subset_select()
    }
    if(option == "3"){
      cat(bold("\nRemove Specific Observations"))
    }
    if(option == "4"){
      cat(bold("\nKeep Specific Observations"))
    }
    if(option == "5"){
      cat(bold("\nGroup the Observations by Variables"))
    }
    if(option == "6"){
      cat(bold("\nGather your Data"))
    }
    if(option == "7"){
      cat(bold("\nSpread your Data"))
    }
    
  }
  remove_vars <- function(variables, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    
    e.super$df <- df %>% 
      select(-all_of(variablessplit))
    
    assign(dfname[1], e.super$df, envir = .GlobalEnv)
  }
  keep_vars <- function(variables, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    
    e.super$df <- df %>% 
      select(all_of(variablessplit))
    
    assign(dfname[1], e.super$df, envir = .GlobalEnv)
  }

  # 4. Analyse: -----------
  #---------
  
  df_select()
  
}


super()
