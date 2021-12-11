main <- function(){
  cat("\nWelcome to the main menu
Here, you will see the different 'main' options you have for this program
      
If at any point you would like to come back to the main menu, just type !!!main

Here is the menu of overaching options:
  1. Choose a Dataframe
  2. Understand your Dataframe
  3. Manage/Change the Contents of your Dataframe
  4. Analyse you Dataframe
      
If you would like to learn more about an option, 
enter below the number associated, immediately followed by HELP
  - 1HELP
  - 2HELP
  - 3HELP
  - 4HELP
      
If you already know which option you would like to choose, enter the number bellow.")

  option <- readline("Type the number associated or (the number)HELP: ")
  main_options(option)
  
}
