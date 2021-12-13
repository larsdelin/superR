#package needed
#library(crayon)


mreg <- function(formula, data){
  fit <- lm(formula, data)
  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))
  class(fit) <- c("mreg", "lm")
  return(fit)
}

#things to consider -
#1. only write and explain about significant p values but unsure how to extract those
#2. how to have user inputting more than 3 variables and perhaps create a for loop that that will
#   input all those variables into the function as required
#3. how to get the explanation to also loop through and explain all significant p values no matter how
#   many there are
#4. How to change name of varibale from data[[a]] to the actual name
#5. Include any r squared explanations
#6. If the values are logged and if I should offer an option for logged values in which case
#   the results would be described differently
data <- mtcars
library(crayon)
# working function 1 ----
super.mreg <- function(a, b, c){
  fit <- mreg(data[[a]] ~ data[[b]] + data[[c]], data)
  print(summary(fit))
  coef <- fit$coefficients
  cat("Do you need help understanding your results?
  1.Yes
  2.No")
  select <- readline("Please enter the number associated with the option you want:")
  if (select == 1){
    cat("Each time", red(a), "went up by a factor of 1,", red(b), "changes by a factor of", bold(red(coef[2])),"\n", seq = "")
    cat("Each time", red(a), "went up by a factor of 1,", red(c), "changes by a factor of", bold(red(coef[3])), seq = "")

  }
}


#spaces between variables when they are wrong for independent variables
# NEWEST VERSION ----
data <- mtcars

super.mreg <- function(){
  cat("Welcome to conducting a multiple regression!", "\n",
      "Please enter the exact name of the dependent variable (i.e predicted variable)", sep = "")
  input1 <- readline("Enter here and hit return:")
  if (length(setdiff(input1, names(data))) == 0){
    cat("Thanks!","\n")
    print(input1)
  }else{
    cat("'", bold(setdiff(input1, names(data))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset", "\n",
        "Now we will go back to the begining, please ensure you enter a valid variable", "\n",
        "that is in the dataset","\n", sep = "")
    input1 = character(0)
    super.mreg()
  }
  cat("Now you will be asked to enter the exact names of the independent variables
(i.e predictor variables)")
  input2 <- readline("Enter the independent variables seperated by a comma:")
  input2 <- strsplit(input2, ",")
  input2 <- input2[[1]]
  if (length(setdiff(input2, names(data))) > 0){
    cat("The following variables are ", bold(red("invalid ")),"as they are not in the dataset:","\n",
        bold(setdiff(input2, names(data))),"\n", sep = "")
    cat("Now we will go back to the begining, please ensure you enter a valid variables", "\n",
        "that are in the dataset","\n", sep = "")
    input2 = character(0)
    super.mreg()
  }else{
    cat("Thanks!","\n")
  }
  cat("Here are the results for the multiple regression:")
  print("test")
  ls <- input1
  rs <- paste(input2, collapse="+")
  f <- paste(ls, "~", rs)
  formula <- as.formula(f)
  fit <- mreg(formula, data)
  print(summary(fit))
  results <- summary.lm(fit)
  results <- summary.lm(fit)$coefficients
  cat("Do you need help understanding your results?
  1.Yes
  2.No")
  select <- readline("Please enter the number associated with the option you want:")
  if (select == 1){
    for(i in 2:nrow(results)){
      if(results[i, 4] < .05){
        cat("Here are your significant results:")
        cat("For a 1 pt change in",  dimnames(results)[[1]][i],
            ",", input1, "is expected to change by", results[i, 1], "\n")
        cat("This will be the r-squared value")
        }
    }

}}


library(crayon)
super.mreg()
hinput1
data <- mtcars
input1
input2 <- c("hp", "wt", "am")







hi# previous functions that may help ----
# ttest <- function(data, x, y, digits = 2, col = "skyblue", ...){
#   t_results <- t.test(data[[y]] ~ data[[x]])
#   std <- aggregate(data[[y]], by=list(data[[x]]), FUN=sd)
#
#   if(t_results$p.value < .05) {
#     signif_flag <- "significant"
#   } else {
#     signif_flag <- "non significant"
#   }
#   cat("There was a ", signif_flag, " difference in ", y,
#       " for ", x, " =", std[1,1],
#       " (M =", round(t_results$estimate[1], digits), ", SD=", round(std[1,2], digits), ") and", "\n",
#       x, " =", round(std[2,1], digits), " (M=", round(t_results$estimate[2],digits), ", SD =", round(std[2,2],digits),
#       "), t(",
#       round(t_results$parameter, digits), ") =", round(t_results$statistic, digits),
#       ", p=", round(t_results$p.value, digits), ".", sep= '', fill = TRUE)
#   boxplot(data[[y]] ~ data[[x]],
#           xlab = x, ylab = y, col = col, ...)
# }
#

# ryx <- function(data, y, x = NULL, digits = 3){
#   result <- data.frame()
#   if(is.null(x) | is.numeric(x) == FALSE){
#     x <- names(data)[sapply(data, is.numeric)]
#   }
#   for(i in x){
#     test <- cor.test(data[,i], data[,y])
#     if (test$p.value < 0.05){
#       test$signif <- "*"}
#     if (test$p.value < 0.01){
#       test$signif <- "**"}
#     if (test$p.value < 0.001){
#       test$signif <- "***"}
#     result <- rbind(result, data.frame("variable" = i, "r" = test$estimate, "p" = test$p.value, "signif" = test$signif))
#     result <- result[order(result$r, decreasing = FALSE),]
#   }
#   row.names(result) <- NULL
#   cat("Correlations of", y, "with", "\n")
#   return(result)
# }
