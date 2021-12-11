manage_options <- function(option){
  back(option, manage_select())
  if(option == "1"){
    subset_select()
  }
  else if(option == "2"){
    changevar_select()
  }
  else if(option == "3"){
    createvar_select()
  }
  else{
    cat(red("\nYou did not select one of the three options."))
    option <- readline("Please enter the number associated with the option: ")
    manage_options(option)
  }
}



