supersummary <- function(x){
  cat("What information about your data would you like to see?\n1. View dataframe\n2. View Variables\n3. Summary Satistics\n4. Detect missing data")
  temp <- 0
  while (temp == 0) {
    option <- readline(prompt="Please select from options 1 - 4: ")
    if(option == 1 | option == 2 | option == 3 | option == 4){
      temp <- 1
    }
  }
  if (option == 1) {
    print(x)
  }
  if (option == 2) {
    str(x)
  }
  if (option == 3) {
    print(summary(x))
  }
  if (option == 4) {
    missing <- sum(is.na(x))
    if (missing > 1) {
      cat("This dataset has ", missing, " missing values.\nGo back to the main menu to handle this missing data")
    }
    if (missing == 1) {
      cat("This dataset has 1 missing value.\nGo back to the main menu to handle this missing data")
    }
    else {
      cat("No missing data detected in this dataset.\n",
          "Tip: check dataset codebook if avaiable to determine if any missing data is encoded as a distinct value (ex. 999).")
    }
  }
  cat("\n", "--------------", "\n", "\n")
  supersummary(x)
}
