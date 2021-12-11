main_options <- function(option){
  if(option == "1HELP"){
    cat("\n\nChoose a Dataframe 

You have the choice to:
  1. Import new dataframe
  2. Choose a dataframe already loaded in your global environment")
    readline("Type anything or hit the return button: ")
    main()
  }
  else if(option == "2HELP"){
    cat("\n\nUnderstand your Dataframe 

You have the choice to:
...")
    readline("Type anything or hit the return button: ")
    main()
  }
  else if(option == "3HELP"){
    cat("\n\nManage/Change the Contents of your Dataframe 

You have the choice to:
...")
    readline("Type anything or hit the return button: ")
    main()
  }
  else if(option == "4HELP"){
    cat("\n\nAnalyse you Dataframe 

You have the choice to:
...")
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
    cat("\nYou did not select one of the four options or the help for an option.")
    option <- readline("Type the number associated or (the number)HELP: ")
    main_options(option)
  }
}
