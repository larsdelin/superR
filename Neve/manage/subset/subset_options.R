subset_options <- function(option){
  back(option, subset_select())
  if(option == "1"){
    cat("\nYou chose to remove specific variables.")
    varname()
    cat("\n\nWhich variables would you like to remove?")
    remove_vars()
    
    vars <- readline("Enter in each variable you want to remove: ")

    
  }
  else if(option == "2"){
    cat("\nYou chose to keep specific variables.")
  }
  else if(option == "3"){
    cat("\nYou chose to remove specific observations.")
  }
  else if(option == "4"){
    cat("\nYou chose to keep specific observations.")
  }
  else if(option == "5"){
    cat("\nYou chose to group the observations by variables.")
  }
  else if(option == "6"){
    cat("\nYou chose to gather your data.")
  }
  else if(option == "7"){
    cat("\nYou chose to spread your data.")
  }
  else{
    cat("\nYou did not select one of the seven options.")
    option <- readline("Please enter the number associated with the option: ")
    subset_options(option)
  }
  
}
