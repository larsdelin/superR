varname <- function(fun){
  cat("\n\nWhen entering in the variables, list them out with spaces in between them:

examples:
  - var1
  - var1 var2 var5

non-examples:
  - var1, var2, var5
  - var1 and var2
  - var1+var2
  - all except var1 and var2")
  checkvarname(fun)
  cat("\nThe variables you've selected are:", vars)
}
  