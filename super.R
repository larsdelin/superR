
super <- function(){
  e.super <- new.env(parent = emptyenv())
  e.super$df <- NULL
  e.super$dfname <- NULL
  e.super$variables <- NULL
  e.super$name <- NULL
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
    else if(x == "!!!main"){
      if(is.null(df) | is.null(dfname)){
        df_select()}
      else{main()}
    }
    else{NULL}
  }
  options <- function(option, validoptions, backto, df = e.super$df, dfname = e.super$dfname,
                      variables = e.super$variables, name = e.super$name){
    repeat{
      go <- back(option, backto)
      
      if(!(is.null(go))){
        result <- list(go = go)
        return(result)
        break 
      }
      else if(option %in% validoptions){
        result <- list(go = go, option = option)
        return(result)
        break}
      else{
        if(is.null(df) | is.null(dfname)){
          cat(red$bold("\nError: "), red("You did not select one of the options."),
              "\n> If you would like to go back, enter ", green$bold("!!!"), sep = "")
          option <- readline("Please enter the number associated with the option: ")
        }
        else{
          cat(red$bold("\nError: "), red("You did not select one of the options."),
              "\n> If you would like to go back, enter ", green$bold("!!!"), 
              "\n> If you would like to go back to the main menu, enter ", green$bold("!!!"), sep = "")
          option <- readline("Please enter the number associated with the option: ")
        }
      }
    }
  }
  
  # Name: -----------
  illegal <- function(name){
    name_sep <- unlist(strsplit(name, ""))
    error <- c()
    for(i in name_sep){
      error[[i]] <- ifelse(identical(".", i) | 
                             identical("_", i) | 
                             identical("-", i) , FALSE,
                           ifelse(grepl("[[:punct:]]", i), TRUE, 
                                  ifelse(identical(" ", i), TRUE, FALSE)))}
    error <- unlist(error)
    "TRUE" %in% error}
  naming <- function(nametype, backto, df = e.super$df, dfname = e.super$dfname){
    e.super$name <- NULL
    repeat{
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
      result <- options(option, 1:2, backto)
      if(!(is.null(result$go))){break}
      option <- result$option
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
      go <- back(name, backto)
      if(!(is.null(go))){break}
      if(length(name) == 0){
        cat(red$bold("\nError: "), red("You did not enter in a name."), sep = "")
      }
      else if(!(substr(name, 1, 1) %in% letters | substr(name, 1, 1) %in% LETTERS | substr(name, 1, 1) == ".")){
        cat(red$bold("\nError: "), red("The name you chose does not begin with a letter or a period (.)."), sep = "")}
      else if(illegal(name) == TRUE){
        cat(red$bold("\nError: "), red("The name you chose contains illegal characters or spaces."), sep = "")
        cat("\nIllegal characters are symbols/punctuation other than a period (.), dash (-), or underscore (_)")}
      # check if existing name
      else{
        if(nametype == "dataframe"){
          if(exists(name, envir = .GlobalEnv, inherits = FALSE)){
            cat(red$bold("\nWarning: "), red("The name you chose is one that is already being used by something in your global environment."), 
                "\n\nYou may want to provide a different name.
If you use choose to use this name, it will ", red$bold("replace"), " the other element, even if it is not a dataframe.

Do you want to:
  1. Use this name (", red$bold("replace"), " the other)
  2. Choose a different name", sep = "")
            option <- readline("Type the number associated: ")
            result <- options(option, c("1", "2"), backto)
            if(!(is.null(result$go))){break}
            option <- result$option
            if(option == "1"){
              e.super$name <- name
              break}
          }
          else{e.super$name <- name
          break}
        }
        if(nametype == "variable"){
          if(name %in% names(df)){
            cat(red$bold("\nWarning: "), red("The name you chose is one that is already the name of a variable in the dataframe '"),
                underline(dfname),
                "'.\n\nYou will have to choose a different name.")}
          else{e.super$name <- name
          break}}
      }
    }
  }
  
  # Variable Name: -----------
  
  varname <- function(dothis, backto, solo = 0, df = e.super$df, dfname = e.super$dfname){
    e.super$variables <- NULL
    if(solo == 1){
      repeat{
        cat("\nThe variables in your dataframe are:\n  - ", paste(names(df), sep = "", collapse = "\n  - "), 
            "\n\nWhich variable would you like to ", bold(dothis), "?", sep = "")
        variables <- readline("Enter in the variable: ")
        go <- back(variables, backto)
        if(!(is.null(go))){break}
        if(variables == ""){
          cat(red$bold("\nError: "), red("You did not enter in a variable."), sep = "")}
        else{
          variablessplit <- strsplit(variables, " ")
          variablessplit <- variablessplit[[1]]
          if(length(variablessplit) > 1){
            cat(red$bold("\nError: "), red("You entered in more than 1 variable."), sep = "")}
          else if(!(variables %in% names(df))){
            cat(red$bold("\nError: "), red("You entered an invalid variable."),
                "\nThe variable you entered is not one of the variable names in your dataframe.", sep = "")
          }
          
          # confirm
          else{
            cat("\nThe variable you selected is: ", green$bold(variables),
                "\n\nAre you sure you would like to ", bold(dothis), " the variable in the dataframe '", 
                underline(dfname), "'?
  1. Yes
  2. No", sep = "")
            option <- readline("Type the number associated: ")
            result <- options(option, 1:2, backto)
            if(!(is.null(result$go))){break}
            option <- result$option
            if(option == 1){
              e.super$variables <- variables
              break}
          }
        }
      }
    }  
    else{
      repeat{
        cat("\nThe variables in your dataframe are:\n  - ", paste(names(df), sep = "", collapse = "\n  - "), 
            "\n\nWhen entering in the variable(s), list them out with spaces in between them:

examples:
  - var1
  - var1 var2 var5

Which variable(s) would you like to ", bold(dothis), "?", sep = "")
        variables <- readline("Enter in the variable(s): ")
        go <- back(variables, backto)
        if(!(is.null(go))){break}
        
        variablessplit <- strsplit(variables, " ")
        variablessplit <- variablessplit[[1]]
        valid <- variablessplit %in% names(df)
        
        # check variable name
        if(variables == ""){
          cat(red$bold("\nError: "), red("You did not enter in any variable(s)."), sep = "")}
        else if(FALSE %in% valid){
          cat(red$bold("\nError: "), red("You entered an invalid variable or list of variables."),
              "\nThis means you either inputted your variable(s) in wrong, 
or one or more of the variables you entered is not one of the variable names in your dataframe.", sep = "")
        }
        
        # confirm
        else{
          cat("\nThe variable(s) you selected are:\n  - ", paste(variablessplit, collapse = "\n  - "), 
              "\n\nAre you sure you would like to ", bold(dothis), " the variable(s) in the dataframe '", 
              underline(dfname), "'?
  1. Yes
  2. No", sep = "")
          option <- readline("Type the number associated: ")
          result <- options(option, 1:2, backto)
          if(!(is.null(result$go))){break}
          option <- result$option
          if(option == 1){
            e.super$variables <- variables
            break}
        }
      }
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
      
If you would like to learn more about an option, type HELP", sep = "")
    
    option <- readline("Type the number associated or (the number)HELP: ")
    while(!(option %in% 1:4 | option == "HELP")){
      cat(red$bold("\nError: "), red("You did not select one of the options or HELP."), sep = "")
      option <- readline("Type the number associated or HELP: ")
    }
    main_options(option)
  }
  main_options <- function(option){
    if(option == "HELP"){
      cat(bold("\n\nHELP: Choose a Dataframe"), 
          
          "\n\nYou have the choice to:
  1. Import new dataframe
  2. Choose a dataframe already loaded in your global environment", sep = "")
      readline("Type anything or hit the return button: ")
    }
    else if(option == "1"){
      df_select()
    }
    else if(option == "2"){
      supersummary()
    }
    else if(option == "3"){
      manage_select()
    }
    else if(option == "4"){
      super.mreg()
    }
    main()
  }
  
  # 1. Dataframe: -----------
  df_select <- function(df = e.super$df, dfname = e.super$dfname, name = e.super$name){
    cat(bold("\n\nChoose a Dataframe"), "\n\nDo you want to:
  1. Import a dataframe
  2. Use an existing dataframe in your global environment", sep = "")
    option <- readline("Type the number associated: ")
    if(is.null(df) | is.null(dfname)){
      result <- options(option, 1:2, df_select()) 
    }
    else{result <- options(option, 1:2, main())}
    if(is.null(result$go)){
      option <- result$option
      if(option == 1){
        df_import()
      }
      else if(option == 2){
        df_existing()
      }
      main()
    }
  }
  df_existing <- function(df = e.super$df, dfname = e.super$dfname){
    repeat{
      cat(bold("\nUse an Existing Dataframe in your Global Environment"))
      name <- readline("Enter in the name of the dataframe: ")
      go <- back(name, df_select())
      if(!(is.null(go))){break}
      if(exists(name, envir = .GlobalEnv) && is.data.frame(get(name, envir = .GlobalEnv))){
        e.super$dfname <- name
        e.super$df <- get(name, envir = .GlobalEnv)
        break}
      else{
        cat(red$bold("\nError: "), red("The name you entered does not match any dataframe currently in your global environment."),
            "\nPlease enter the name of the dataframe you would like to use currently in your global environment. 
> If the dataframe you would like to use is not in your global environment, 
  enter ", green$bold("!!!"), " to go back to the options for choosing your dataframe and pick option 1: Import a Dataframe.\n", sep = "")
        if(!(is.null(df) | is.null(dfname))){
          cat("> If you would like to use the dataframe '", underline(dfname), "' that is already loaded in the program,
  enter ", green$bold("!!!main"), " to go back to the main menu.", sep = "") 
        }
      }
    }
  }
  df_import <- function(df = e.super$df, dfname = e.super$dfname, name = e.super$name){
    cat(bold("\nImport a Dataframe"), "\n\nYou will have to give your dataframe a name.", sep = "")
    naming("dataframe", df_select())
    if(!(is.null(name))){
      e.super$dfname <- name
      e.super$df <- import()
      assign(e.super$dfname[1], e.super$df, envir = .GlobalEnv)
      main()
    }
  }
  import <- function(){
    repeat{
      file <- tryCatch({file <- file.choose()}, error = function(ex){})
      if(is.null(file)){
        cat("You pressed cancel, meaning you did not select a file.")
        again <- readline("Type anything to choose again, or !!! to go back to the options: ")
        go <- back(again, df_select())
        if(!(is.null(go))){
          break
        }
      }
      else{
        file <- tolower(file)
        basename <- basename(file)
        extension <- tools::file_ext(file)
        
        df <- switch(extension,
                     "sas7bdat" = haven::read_sas(file),
                     "dta" = haven::read_stata(file),
                     "sav" = haven::read_spss(file),
                     "xlsx" = readxl::read_excel(file),
                     "xls" = readxl::read_excel(file),
                     vroom::vroom(file))
        return(df)
        break
      }
    }
  }
  
  # 2. Summarize: -----------
  supersummary <- function(df = e.super$df, dfname = e.super$dfname){
    repeat{
      cat("What information about your dataframe '", underline(dfname), "'? 

Would you like to see:
  1. View dataframe
  2. View variables
  3. Summary satistics
  4. Detect missing data
  5. Go back to the main menu", sep = "")
      option <- readline(prompt="Type the number associated: ")
      result <- options(option, 1:5, main())
      if(!(is.null(result$go))){break}
      option <- result$option
      if (option == 1) {
        cat("\n", "Data Preview:","\n","---------","\n")
        print(df)
      }
      if (option == 2) {
        cat("\n", "Variables summary:","\n","---------","\n")
        dta <- data.frame()
        for (i in 1:length(df)){
          dta[i,1] <- colnames(df[i])
          dta[i,2] <- sapply(df[i], "class")
          unq <- na.omit(length(unique(df[[i]])))
          dta[i,3] <- unq
          temp <- na.omit(unique(df[[i]])[1:10])
          dta[i,4] <- paste(temp, collapse = " ")
          if (unq>10){
            dta[i,5] <- "..."
            dta[i,6] <- unq-10
            dta[i,7] <- "more"
          } else{
            dta[i,5] <- ""
            dta[i,6] <- ""
            dta[i,7] <- ""
          }
        }
        names(dta) <- c("Vars", "Class", "Unique", "Values", "", "", "")
        print(dta, right = F)
      }
      if (option == 3) {
        cat("\n", "Summary statistics:","\n","---------","\n")
        print(summary(df))
      }
      if (option == 4) {
        cat("\n", "Missing data by variable:","\n","---------","\n")
        dta <- data.frame()
        for (i in 1:length(df)){
          missing <- sum(is.na(df[i]))
          dta[i,1] <- colnames(df[i])
          dta[i,2] <- missing
        }
        names(dta) <- c("Vars", "Miss")
        print(dta)
        
        cat("\n")
        
        if (sum(dta[,2]) > 1) {
          cat("This dataset has ", missing, " missing values.\nGo back to the main menu to handle this missing data")
        }
        if (sum(dta[,2]) == 1) {
          cat("This dataset has 1 missing value.\nGo back to the main menu to handle this missing data")
        }
        else {
          cat("No missing data detected in this dataset.\n",
              "Tip: check dataset codebook if avaiable to determine if any missing data is encoded as a distinct value (ex. 999).")
        }
      }
      if (option == 5) {
        main()
        break
      }
      cat("\n", "--------------", "\n", "\n")
      
    }
  }
  
  # 3. Manage: -----------
  manage_select <- function(){
    cat(bold("\n\nManage/Change the Contents of your Dataframe"), "\n\nDo you want to:
  1. Subset your data
  2. Change existing variables", sep = "")
    option <- readline("Type the number associated: ")
    result <- options(option, 1:2, main())
    if(is.null(result$go)){
      manage_options(option)
    }
  }
  manage_options <- function(option){
    if(option == "1"){
      subset()
    }
    else if(option == "2"){
      changevar_select()
    }
  }
  subset <- function(df = e.super$df, dfname = e.super$dfname, name = e.super$name){
    repeat{
      cat(bold("\nSubset your Data"),
          "\n\nBefore you do any subsetting, would you like this subsetting to occur in your dataframe '", 
          underline(dfname), "', 
or in a copy of your dataframe '", underline(dfname), "'?

Would you like to:
  1. Create a copy of your dataframe '", underline(dfname), "': subset in a copy 
     to  preserve '", underline(dfname), "'  in your global environment
  2. Use the dataframe '", underline(dfname), "': subset in '", underline(dfname), "'", sep = "")
      option <- readline("Type the number associated: ")
      result <- options(option, 1:2, manage_select())
      if(!(is.null(result$go))){break}
      option <- result$option
      if(option == 1){
        cat("\nYou will have to provide a name for your subset of the dataframe '", underline(dfname), "'", sep = "")
        naming("dataframe", subset())
        if(!(is.null(name))){
          e.super$dfname <- name
          assign(e.super$dfname[1], df, envir = .GlobalEnv)
          subset_select()
          break}
        else{break}
      }
      else{
        subset_select()
        break}
    }
  }
  changevar <- function(df = e.super$df, dfname = e.super$dfname, name = e.super$name){
    repeat{
      cat(bold("\nChange Existing Variables"),
          "\n\nBefore you change any variables, would you like this data management to occur in your dataframe '", 
          underline(dfname), "', 
or in a copy of your dataframe '", underline(dfname), "'?

Would you like to:
  1. Create a copy of your dataframe '", underline(dfname), "': subset in a copy 
     to  preserve '", underline(dfname), "'  in your global environment
  2. Use the dataframe '", underline(dfname), "': subset in '", underline(dfname), "'", sep = "")
      option <- readline("Type the number associated: ")
      result <- options(option, 1:2, manage_select())
      if(!(is.null(result$go))){break}
      option <- result$option
      if(option == "1"){
        cat("\nYou will have to provide a name for your copy of the dataframe '", underline(dfname), "'", sep = "")
        naming("dataframe", changevar())
        if(!(is.null(name))){
          e.super$dfname <- name
          assign(e.super$dfname[1], df, envir = .GlobalEnv)
          changevar()
          break}
        else{break}
      }
      else{
        changevar()
        break}
    }
  }
  
  #_______________________________________________
  # Subset:
  
  subset_select <- function(){
    cat(bold("\n\nSubsetting Options"), 
        "\n\nDo you want to:
  1. Remove specific variables
  2. Keep specific variables
  3. Remove specific observations
  4. Keep specific observations", sep = "")
    option <- readline("Type the number associated: ")
    result <- options(option, c(1:4), manage_options("1"))
    if(is.null(result$go)){
      option <- result$option
      subset_options(option)
    }
  }
  subset_options <- function(option, df = e.super$df, dfname = e.super$dfname, variables = e.super$variables){
    if(option == "1"){
      cat(bold("\nRemove Specific Variables"))
      varname("remove", subset_select())
      if(!(is.null(variables))){
        remove_vars()
        subset_select()
      }
    }
    if(option == "2"){
      cat(bold("\nKeep Specific Variables"))
      varname("only keep", subset("2"))
      if(!(is.null(variables))){
        keep_vars()
        subset_select() 
      }
    }
    if(option == "3"){
      cat(bold("\nRemove Specific Observations"))
      
      varname("use as constraint(s)", subset_select())
      if(!(is.null(variables))){
        filtering()
      }
      
    }
    if(option == "4"){
      cat(bold("\nKeep Specific Observations"))
    }
  }
  remove_vars <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    
    e.super$df <- df %>% 
      select(-all_of(variablessplit))
    
    assign(dfname[1], e.super$df, envir = .GlobalEnv)
  }
  keep_vars <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    
    e.super$df <- df %>% 
      select(all_of(variablessplit))
    
    assign(dfname[1], e.super$df, envir = .GlobalEnv)
  }
  filtering <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname){
    variablessplit <- strsplit(variables, " ")
    variablessplit <- variablessplit[[1]]
    classes <- c("numeric", "factor", "character", "logical")
    valid <- list()
    filterit <- list()
    for(i in variablessplit){
      valid[[i]] <- class(df[[i]]) %in% classes}
    if(!(FALSE %in% valid)){
      for(i in variablessplit){
        cat(bold("\nVariable = ", i), sep = "")
        if(is.factor(i)){
          cat("\nThis variable is a factor variable with ", length(levels(df[[i]])), "levels. The levels are:\n -",
              paste(levels(df[[i]]), sep = "", collapse = "\n  - "),
              "\nWhich level would you like to choose for filtering?", sep = "")
          option <- readline("Type the name of the level: ")
          option <- options(option, levels(df[, i]), backto)
          filterit[[i]] <- fact(i)
        }
        else if(is.numeric(i)){
          cat("\nThe range of your variable is: ", bold(range(df[[i]])[1], " - ", range(df[[i]])[2]), 
              "There are a few operations you can use. 
Is the variable:
  1. = : equal to a number
  2. != : not equal to a number
  3. > : greater than a number
  4. >= : greater than or equal to a number
  5. < : less than a number
  6. <= : less than or equal to a number
  7. between: between two numbers
  8. outside: outside of two numbers", sep = "")
          option <- readline("Type the number associated: ")
          option <- options(option, 1:8, backto)
          filterit[[i]] <- operation(i, option)
        }
        else if(is.character(i)){
          cat("\nWould you like to filter using: 
  1. The whole string of an observation
  2. A substring of an observation")
          option <- readline("Type the number associated: ")
          option <- options(option, 1:2, backto)
          if(option == 1){
            filterit[[i]] <- whole(i)
          }
          if(option == 2){
            filterit[[i]] <- part(i)
          }
        }
        else if(is.logical(i)){
          cat("\nWhich observation would you like to choose for filtering?
  1. TRUE
  2. FALSE", sep = "")
          option <- readline("Type the number associated: ")
          option <- options(option, 1:2, backto)
          filterit[[i]] <- logic(i, option)
        }
      }
    } 
    else{
      cat("\nOne or more of the variables you selected has a class that cannot be accommodated for", sep = "")
      subset_select()
    }
  }
  
  #_______________________________________________
  # Change Vars:
  changevar_select <- function(){
    cat(bold("\nChange Existing Variables"),
        "\n\nDo you want to:
  1. Change variable class
  2. Rename variable
  3. Recode variable values")
    option <- readline("Type the number associated: ")
    result <- options(option, c(1:3), manage_select())
    if(is.null(result$go)){
      option <- result$option
      changevar_options(option)
    }
  }
  changevar_options <- function(option, df = e.super$df, dfname = e.super$dfname){
    if(option == "1"){
      cat(bold("\nChange Variable Class"))
      varname("re-class", changevar_select(), solo = 1)
      if(!(is.null(variables))){
        changevar_class()
        manage_select()}
    }
    if(option == "2"){
      cat(bold("\nRename Variable"))
      varname("rename", changevar_select(), solo = 1)
      if(!(is.null(variables))){
        changevar_name()
        manage_select()}
    }
    if(option == "3"){
      cat(bold("\nRecode Variable Values"))
      varname("recode", changevar_select(), solo = 1)
      if(!(is.null(variables))){
        changevar_recode()
        manage_select()}
    }
  }
  changevar_class <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname) {
    cat("\nWhat class would you like to change the variable(s) ", variables, "to?",
        "
  1. Numeric
  2. Character
  3. Logical
  4. Factor")
    option <- readline("Type the number associated: ")
    result <- options(option, c(1:4), changevar_options(1))
    if(is.null(result$go)){
      option <- result$option
      variablessplit <- strsplit(variables, " ")
      variablessplit <- variablessplit[[1]]
      if (option == 1){
        temp <- list()
        for (i in variablessplit){
          if (length(na.omit(df[[i]])) == length(na.omit(as.numeric(df[[i]])))){
            df[[i]] <- as.numeric(df[[i]])
          } 
          else {
            append(temp, i)
          }
        }
        if(length(temp) > 0){
          cat("Unfortunately these variables were not coerible into class numeric:", temp)
        } 
        else {
          cat("All specified variables were changed to class numeric")
        }
      }
      if (option == 2){
        for (i in variablessplit){
          df[[i]] <- as.character(df[[i]])
        }
        cat("All specified variables were changed to class character")
      }
      if (option == 3){
        temp <- list()
        for (i in variablessplit){
          try <- unique(na.omit(df[[i]]))
          if (length(try) == 2){
            for (n in 1:length(df[[i]])){
              if (df[[i]][[n]] == try[1]){
                df[[i]][[n]] <- FALSE
              }
              if (df[[i]][[n]] == try[2]){
                df[[i]][[n]] <- TRUE
              }
            }
            df[[i]] <- as.logical(df[[i]])
            cat(try[1], "recoded as: FALSE\n", try[2], "recoded as: TRUE\n")
          } 
          else {
            append(temp, i)
          }
        }
        if(length(temp) > 0){
          cat("Unfortunately these variables were not coerible into class logical:", temp)
        } else {
          cat("All specified variables were changed to class logical")
        }
      }
      if (option == 4){
        cat("All specified variables were changed to class factor\nLevels:\n")
        for (i in variablessplit){
          df[[i]] <- as.factor(df[[i]])
          cat(i,": ", levels(df[[i]]),"\n")
        }
      }
      
      e.super$df <- df
      assign(dfname[1], e.super$df, envir = .GlobalEnv)
    }
  }
  changevar_name <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname, name = e.super$name) {
    naming("variable", changevar_options(2))
    if(!(is.null(e.super$name))){
      e.super$df <- rename(df, name = variables)
      assign(dfname[1], df, envir = .GlobalEnv)
    }
  }
  changevar_recode <- function(variables = e.super$variables, df = e.super$df, dfname = e.super$dfname) {
    cat("\nWhat value of ", variables, " would you like to recode?")
    print(unique(df[[variables]]))
    option <- readline("Type a valid variable response: ")
    result <- options(option, unique(df[[variables]]), changevar_options(3))
    if(is.null(result$go)){
      option <- result$option
      response <- readline("What would you like to recode: ")
      go <- back(response, changevar_options(3))
      if(is.null(go)){
        for (i in 1:length(df[[variables]])){
          if (df[[variables]][[i]] == option){
            df[[variables]][[i]] <- response}
        }
        e.super$df <- df
        assign(dfname[1], e.super$df, envir = .GlobalEnv)
        cat(option," has now been recoded as ", response)
      }
    } 
  }
  
  # 4. Analyse: -----------
  super.mreg <- function(){
    df <- e.super$df
    plot.mreg <- function(x, points = FALSE, dot.size = 1, dot.alpha=.5, ci= TRUE, ...){
      vars <- names(x$model)[names(x$model) != x$terms[[2]]]
      myplots <- vector(mode="list", length=length(vars))
      names(myplots) <- vars
      for(i in vars){
        myplots[[i]] <- plot(ggeffect(x, i), add.data=points,
                             dot.size=dot.size, dot.alpha=dot.alpha, ci=ci, ...)+ labs(title="")
      }
      final <- wrap_plots(myplots) + plot_annotation(title="Effects Plots",
                                                     subtitle="Each variables bilateral relationship controlling for the others")
      return(final)
    }
    mreg <- function(formula, df){
      fit <- lm(formula, df)
      fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                                 ", data=", deparse(substitute(df)), ")"))
      class(fit) <- c("mreg", "lm")
      return(fit)
    }
    #Introduction to reg ----
    repeat{
      cat(underline("Welcome to conducting a multiple regression!\n"),
          "Would you like a brief overview of what a multiple regression entails?\n",
          "  1.", green(" Yes\n"),
          "  2.", red(" No"),sep="")
      overview <- readline("Please enter the number associated with the option:")
      result <- options(overview, 1:2, main())
      if(!(is.null(result$go))){break}
      overview <- result$option
      if (overview == 1){
        cat(underline(green("\nMultiple linear regression")), " also known simply as multiple regression, is a statistical technique 
that uses ", bold("several explanatory variables"), " (i.e predictor variables or independent variables) to
predict the outcome of a ", bold("response variable"), " (i.e predicted variable or dependent variable). 
\nMultiple regression is an extension of simple linear regression as you can input more than one 
explanatory variable that gives you a better and more in-depth analysis.\n",
            green("\nIn summary"), ", multiple regression will allow you to see how the explanatory variables are affecting 
the response variables, which is useful in trying to make conclusions regarding certain hypothesis.", sep="")
      }
      cat("\n----------------------------------------------------------------------------------------------------------------",sep="")
      cat(bold(red("\nNote:")), " Please ensure all variables that will be used in this regression have", 
          red$bold(" no missing values"), " and are the \nsuitable type and class for the multiple regression to provide meaningful results.","\n",
          "\n> If at any point you want to go back to the main menu just type: ", green("!!!"), sep="")
      cat("\n----------------------------------------------------------------------------------------------------------------",sep="")
      cat("\nNow you will be asked to input the variable you want to predict (i.e response/dependent variable).", "\n",
          "Please ensure you enter the ", bold(red("exact name")), " of this variable.", sep = "")
      #Input dependent variable ----
      repeat{
        dv <- readline("Enter dependent variable name here and hit return:")
        go <- back(dv, main())
        if(!(is.null(go))){break}
        if(length(setdiff(dv, names(df))) > 0){
          cat("\n'", bold(underline((setdiff(dv, names(df))))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset"," '", blue(e.super$dfname),"'", "\n",
              "\nPlease input a variable that is in the dataset"," '", blue(e.super$dfname),"'", sep = "")
        }
        else{
          input1 <- dv
          cat("\nThanks! The variable you chose is: ", bold(input1),"\n", sep="")
          break
        }
      }
      if(!(is.null(go))){break}
      cat("\nNow you will be asked to enter the ", bold(red("exact name")), " of the explanatory variables (i.e independent/predictor)
that will be tested against the response variable.\n", sep="")
      #Input independent variable ----
      repeat{
        iv <- readline("Please enter the independent variables seperated by a space:")
        go <- back(iv, main())
        if(!(is.null(go))){break}
        iv <- strsplit(iv, " ")
        iv <- iv[[1]]
        if (length(setdiff(iv, names(df))) > 0){
          cat("\nThe following variables are ", bold(red("invalid ")),"as they are not in the dataset"," '", blue(e.super$dfname),"':","\n",
              underline(bold(setdiff(iv, names(df)))),"\n\n", 
              "Please input a variable that is in the dataset"," '", blue(e.super$dfname),"'", sep = "")
        }else{
          input2 <- iv
          cat("\nThanks! The variables you chose are: ", bold(input2), sep="")
          break
        }
      }
      #Results ----
      if(!(is.null(go))){break}
      cat(green(bold(underline("\nHere are the results for the multiple regression:"))))
      ls <- input1
      rs <- paste(input2, collapse="+")
      f <- paste(ls, "~", rs)
      formula <- as.formula(f)
      fit <- mreg(formula, df)
      print(summary.lm(fit))
      results <- summary.lm(fit)$coefficients
      cat(underline("Do you need help understanding the", bold("output and significance"), "of the above regression?"),"\n",
          "  1.", green(" Yes\n"),
          "  2.", red(" No"),sep="")
      select <- readline("Please enter the number associated with the option:")
      result <- options(select, 1:2, main())
      if(!(is.null(result$go))){break}
      select <- result$option
      if (select == 1){
        cat(underline("\nHere are the ", bold(blue("significant results")), " where the p value was less than 0.05:"),"\n\n", sep = "")
        for(i in 2:nrow(results)){
          if(results[i, 4] < .05){
            cat("For a 1 point change in ",  bold(dimnames(results)[[1]][i]),
                ", ", blue(input1), " is expected to change by ", red(results[i, 1]),".\n", sep = "")
          }
        }
      }
      repeat{ 
        cat("\n",underline("Additionally, do you want to do any of the following?"),"\n", 
            " 1.", blue("Understand the ", bold("R-squared value and Adjusted R-squared"), " value\n"),
            " 2.", green("Get ", bold("plots of each paired relationship"), " between the dependent and each independent variable\n"),
            " 3.", magenta("Get the ", bold("relative importance"), " of each variable in your model\n"),
            " 4.", yellow("Go back to the ", bold("main menu"), sep=""))
        input <-  readline("Please enter the number associated with the option:")
        result <- options(input, 1:4, main())
        if(!(is.null(result$go))){break}
        input <- result$option
        #R and Adjust R squared analysis ----
        if (input == 1){
          r.s <- summary.lm(fit)$r.squared
          ars <- summary.lm(fit)$adj.r.squared
          cat(red("\nThe R-squared value for this analysis is "), bold(r.s),
              "\n> The R-squared value explains the degree to which your explanatory variables explains the variation 
of your response variable.\n> So an R-squared value of ", r.s," means that ",(r.s)*100,"%"," of the variation in the response variable 
is explained by the explanatory variables in the analysis.","\n\n", sep ="")
          cat(red("The Adjusted-R squared value for this analysis is "), bold(ars),
              "\nThe Adjusted R-squared increases when new terms improve the model more than would be expected by chance and
it decreases when a explanatory variable improves the model by less than expected. 
> The Adjusted R-squared helps to determine how much of the significance is just due to addition of 
explanatory variables. 
> The adjusted R-squared compensates for the addition of variables and only increases if the new 
explanatory added enhances the model above what would be obtained by probability. Conversely, it will 
decrease when a explanatory variable improves the model less than what is predicted by chance\n", sep="")
        }
        # Mreg Plots ----
        if (input == 2){ 
          cat(bold(blue(underline(("\nOn the right is the plots! >>>>")))))
          cat("\n>These plots", bold(" show the relationship of response to each explanatory variable seperately"), " so that you can 
understand the relationship between each response varibale and explanatory variables better.\n", sep="")
          p <- plot.mreg(fit, points=TRUE)
          print(p)
        }
        # General dominance plots ----
        if (input == 3){
          cat(bold(magenta(underline("\nOn the right is your general dominance plot! >>>>"))),
              "\nThis plot shows you the ", bold("average importance of each explanatory variable"), " relative to all the other
explanatory variables included in the multiple regression.\n", sep="")
          rela_fit <- lm(formula, mtcars)
          da_df <- dominanceAnalysis(rela_fit)
          rp <- plot(da_df) + 
            labs(x = "Variable Name", 
                 y = "Relative Importance", 
                 title = "General Dominance Plot of Response Variables in Analysis")+ 
            coord_flip() +
            theme(legend.position= "none")
          print(rp)
        }
        if (input == 4){
          cat()
          main()
          break
        }
      }
      if(!(is.null(result$go))){break}
    }
  }
  
  
  df_select()
  
}


super()
