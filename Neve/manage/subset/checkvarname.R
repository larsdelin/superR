checkvarname <- function(fun){
  if(fun == "remove"){
    
    
    
    back(vars, subset_options())
    varssplit <- strsplit(vars, " ")
    varssplit <- varssep[[1]]
    valid <- varssplit %in% df
    if(FALSE %in% valid){
      cat(red("You entered an invalid list of variables.
This means you either inputted your variables in wrong, 
or one or more of the variables you entered is not one of the variable names"))
      
      
    }
    
    remove_vars(vars)
  }
}
}