#package needed -
#library(crayon)


mreg <- function(formula, data){
  fit <- lm(formula, data)
  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))
  class(fit) <- c("mreg", "lm")
  return(fit)
}
data <- mtcars
library(crayon)

#spaces between variables when they are wrong for independent variables
# NEWEST VERSION that works very well----
data <- mtcars

super.mreg <- function(){
  cat("Welcome to conducting a multiple regression!", "\n",
      "Would you like a brief overview of what a multiple regression entails?
  1.Yes
  2.No")
  overview <- readline("Please enter the number associated with the option you want:")
  if (overview == 1){
    cat("Multiple linear regression, also known simply as multiple regression, is a statistical technique 
that uses several explanatory variables (i.e predictor variables or independent variables)
to predict the outcome of a response variable (i.e predicted variable or dependent variable). 
Multiple regression is an extension of simple linear regression because in a multiple regression
you can input more than one explanatory variable.","\n", sep = "")
  }
  cat("Now will be asked to input the variable you want to predict.", "\n",
      "Please ensure you enter the exact name of the dependent variable (i.e predicted variable)", sep = "")
  input1 = 0
  while(input1 == 0){
    dv <- readline("Enter dependent variable name here and hit return:")
    if(length(setdiff(dv, names(data))) > 0){
      cat("'", bold(setdiff(dv, names(data))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset.", "\n",
          "Please input a variable that is in the dataset.", sep = "")
    }else{
      input1 <- dv
      print(input1)
      cat("Thanks!", "\n")
    }
  }
  cat("Now you will be asked to enter the exact names of the independent variables
(i.e predictor variables) that will test the relationship with the depedent variable")
  input2 = 0
  while(input2 == 0){
    iv <- readline("Please enter the independent variables seperated by a space:")
    iv <- strsplit(iv, " ")
    iv <- iv[[1]]
    if (length(setdiff(iv, names(data))) > 0){
      cat("The following variables are ", bold(red("invalid ")),"as they are not in the dataset:","\n",
          bold(setdiff(iv, names(data))),"\n", 
          "Please input a variable that is in the dataset.", sep = "")
    }else{
      input2 <- iv
      print(input2)
      cat("Thanks!","\n")
    }
  }
  cat(bold(green("Here are the results for the multiple regression:")))
  print("test")
  ls <- input1
  rs <- paste(input2, collapse="+")
  f <- paste(ls, "~", rs)
  formula <- as.formula(f)
  fit <- mreg(formula, data)
  print(summary.lm(fit))
  results <- summary.lm(fit)
  results <- summary.lm(fit)$coefficients
  cat("Do you need help understanding your results?
  1.Yes
  2.No")
  select <- readline("Please enter the number associated with the option you want:")
  if (select == 1){
    cat(underline("Here are the significant results where the p value was less than 0.05:"),"\n", sep = "")
    for(i in 2:nrow(results)){
      if(results[i, 4] < .05){
        cat("For a 1 point change in",  bold(dimnames(results)[[1]][i]),
            ",", blue(input1), "is expected to change by", red(results[i, 1]),".", "\n", sep = "")
      }
    }
    cat("Do you want to further understand the multiple regression by looking at the r-sqaured value? 
        1.Yes
        2.No")
    rsq
    cat("This will be the r-squared value")
    
  }
}

super.mreg()



# super.mreg <- function(){
#   cat("Welcome to conducting a multiple regression!", "\n",
#       "Please enter the exact name of the dependent variable (i.e predicted variable)", sep = "")
#   input1 <- readline("Enter here and hit return:")
#   if (length(setdiff(input1, names(data))) == 0){
#     cat("Thanks!","\n")
#     print(input1)
#   }else{
#     cat("'", bold(setdiff(input1, names(data))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset", "\n",
#         "Now we will go back to the begining, please ensure you enter a valid variable", "\n",
#         "that is in the dataset","\n", sep = "")
#     input1 = character(0)
#     super.mreg()
#   }
#   cat("Now you will be asked to enter the exact names of the independent variables
# (i.e predictor variables)")
#   input2 <- readline("Enter the independent variables seperated by a comma:")
#   input2 <- strsplit(input2, ",")
#   input2 <- input2[[1]]
#   if (length(setdiff(input2, names(data))) > 0){
#     cat("The following variables are ", bold(red("invalid ")),"as they are not in the dataset:","\n",
#         bold(setdiff(input2, names(data))),"\n", sep = "")
#     cat("Now we will go back to the begining, please ensure you enter a valid variables", "\n",
#         "that are in the dataset","\n", sep = "")
#     input2 = character(0)
#     super.mreg()
#   }else{
#     cat("Thanks!","\n")
#   }
#   cat("Here are the results for the multiple regression:")
#   print("test")
#   ls <- input1
#   rs <- paste(input2, collapse="+")
#   f <- paste(ls, "~", rs)
#   formula <- as.formula(f)
#   fit <- mreg(formula, data)
#   print(summary(fit))
#   results <- summary.lm(fit)
#   results <- summary.lm(fit)$coefficients
#   cat("Do you need help understanding your results?
#   1.Yes
#   2.No")
#   select <- readline("Please enter the number associated with the option you want:")
#   if (select == 1){
#     for(i in 2:nrow(results)){
#       if(results[i, 4] < .05){
#         cat("Here are your significant results:")
#         cat("For a 1 pt change in",  dimnames(results)[[1]][i],
#             ",", input1, "is expected to change by", results[i, 1], "\n")
#         cat("This will be the r-squared value")
#         }
#     }
# 
# }}




# previous functions that may help ----
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
