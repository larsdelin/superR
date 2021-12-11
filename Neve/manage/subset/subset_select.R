subset_select <- function(){
  cat(bold("\nYou chose to subset your data.\n\n"))
  cat("Do you want to:
  1. Remove specific variables
  2. Keep specific variables
  3. Remove specific observations
  4. Keep specific observations
  5. Group the observations by variables
  6. Gather
  7. Spread")
  option <- readline("Type the number associated: ")
  subset_options(option)
}