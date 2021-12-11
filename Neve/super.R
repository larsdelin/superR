super <- function(){
  cat("\nWelcome to superR! This is an easy way to get your dataframe set up for analysis.
...

If you would like to go back to previous options, type !!!

If you would like to go back to the main menu of options, type !!!main

If at any point you would like to exit the program, press the esc key

Let's begin!")
  
  name <- readline("What is your name? ")
  cat("\nHi ", name, "!", sep="")
  cat("\n\nTo get started, you will need a dataframe.")

  df_select()
  main()

}

super()
