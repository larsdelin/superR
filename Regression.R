#packages needed -
library(crayon)
library(ggeffects)
library(ggplot2)
library(effects)
library(patchwork)
library(dominanceanalysis)
library(stats)

library(readr)
data <- read_csv("~/Desktop/police_killings.csv")

# These will be at the beginning of the 4. analysis section in super
plot.mreg <- function(x, points = FALSE, dot.size = 1, dot.alpha=.5, ci= TRUE, ...){
  vars <- names(x$model)[names(x$model) != x$terms[[2]]]
  myplots <- vector(mode="list", length=length(vars))
  names(myplots) <- vars
  for(i in vars){
    myplots[[i]] <- plot(ggeffect(x, i), add.data=points,
                         dot.size=dot.size, dot.alpha=dot.alpha, ci=ci, ...)+ labs(title="")
  }
  final <- wrap_plots(myplots) + plot_annotation(title="Effects Plots",
                                                 subtitle="Each variables bilateral relationship controlling for the others")
  return(final)
}

mreg <- function(formula, data){
  fit <- lm(formula, data)
  fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                             ", data=", deparse(substitute(data)), ")"))
  class(fit) <- c("mreg", "lm")
  return(fit)
}



# just to test 
data <- mtcars

#1. check warning message at the end of regression that says In while (input2 == 0) { :
#   the condition has length > 1 and only the first element will be used when saying no help
#2. Ensure variables are suitable for multiple regression and what type or class does
#   the dependent variable and independent variable has to be
#3. 
super.mreg <- function(){
  #Intro to function ----
  cat("Welcome to conducting a multiple regression!", "\n",
      "Would you like a brief overview of what a multiple regression entails?
  1.Yes
  2.No")
  overview <- readline("Please enter the number associated with the option:")
  if (overview == 1){
    cat("Multiple linear regression, also known simply as multiple regression, is a statistical technique 
that uses ", bold("several explanatory variables"), " (i.e predictor variables or independent variables)
to predict the outcome of a ", bold("response variable"), " (i.e predicted variable or dependent variable). 
\nMultiple regression is an extension of simple linear regression as you can input more than one explanatory 
variable that gives you a better and more in-depth analysis.","\n\n", sep = "")
  }
#Inputting the dependent variable ----
  cat(bold(red("Note:")), "Please ensure all variables that will be used in this regression
have no missing values and are the suitable type and class for the multiple regression
to provide meaningful results. The depedent variable should be numeric","\n\n", sep="")
  cat("Now you will be asked to input the variable you want to predict.", "\n",
      "Please ensure you enter the ", bold(red("exact name")), " of the dependent variable (i.e predicted variable)", sep = "")
  input1 = 0
  while(input1 == 0){
    dv <- readline("Enter dependent variable name here and hit return:")
    if(length(setdiff(dv, names(data))) > 0){
      cat("'", bold(setdiff(dv, names(data))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset.", "\n",
          "\nPlease input a variable that is in the dataset.", sep = "")
    }else{
      input1 <- dv
      cat("\nThanks!", "\n")
    }
  }
#Inputting the independent variables ----
  cat("Now you will be asked to enter the ", bold(red("exact name")), " of the independent variables
(i.e predictor variables) that will be tested against the depedent variable\n")
  input2 = 0
  while(input2 == 0){
    iv <- readline("Please enter the independent variables seperated by a space:")
    iv <- strsplit(iv, " ")
    iv <- iv[[1]]
    if (length(setdiff(iv, names(data))) > 0){
      cat("The following variables are ", bold(red("invalid ")),"as they are not in the dataset:","\n",
          bold(setdiff(iv, names(data))),"\n\n", 
        "Please input a variable that is in the dataset.")
    }else{
      input2 <- iv
      cat("Thanks!","\n")
    }
  }
# Providing the output of the multiple regression ----
  cat(bold(green("Here are the results for the multiple regression:")))
  ls <- input1
  rs <- paste(input2, collapse="+")
  f <- paste(ls, "~", rs)
  formula <- as.formula(f)
  fit <- mreg(formula, data)
  print(summary.lm(fit))
  results <- summary.lm(fit)$coefficients
#Understanding results of the regression further ----
  cat("Do you need help understanding your results?
1.Yes
2.No")
  select <- readline("Please enter the number associated with the option:")
  if (select == 1){
    cat(underline("Here are the ", bold(blue("significant results")), " where the p value was less than 0.05:"),"\n\n", sep = "")
    for(i in 2:nrow(results)){
      if(results[i, 4] < .05){
        cat("For a 1 point change in ",  bold(dimnames(results)[[1]][i]),
            ", ", blue(input1), " is expected to change by ", red(results[i, 1]),".\n", sep = "")
      }
    }
    cat("\n\nDo you want a more in-depth understanding of the multiple regression? 
    \nPlease select from the following: 
1. Understand the R-squared value and Adjusted R-squared value
2. Get plots of each paired relationship between the dependent and each independent variable
3. Get the relative importance of each variable in your model")
    input <-  readline("Please enter the number associated with the option:")
    if (input == 1){
      r.s <- summary.lm(fit)$r.squared
      ars <- summary.lm(fit)$adj.r.squared
      cat(red("The R-squared value for this analysis is "), bold(r.s),
"\nThe R-squared value explains the degree to which your input variables explains the variation of your predicted variable.
So an R-squared value of ", r.s," means that ",(r.s)*100,"%"," of the variation in the dependent variable is explained by 
the independent variables in the analysis.","\n\n", sep ="")
      cat(red("The Adjusted-R squared value for this analysis is"), bold(ars),
"\nThe Adjusted R-squared increases when the new term improves the model more than would be expected by chance.
It decreases when a predictor improves the model by less than expected. The Adjusted R-squared helps to determine 
how much of the significance is just due to addition of independent variables. The adjusted R-squared compensates for 
the addition of variables and only increases if the new predictor added enhances the model above what would be obtained 
by probability. Conversely, it will decrease when a predictor improves the model less than what is predicted by chance")
    }
    if (input == 2){ 
      cat(bold(blue("On the right is the plots! >>>>")))
      cat("\nThese plots show the relationship of the dependent variable to each independent variable
seperately so that you can see the results of the multiple regression more clearly")
      p <- plot.mreg(fit, points=TRUE)
      print(p)
    }
    if (input == 3){
      cat(bold(magenta("On the right is your general dominance plot!")),
          "\nThis plot shows you the average importance of each independent variable relative to all the other
independent variables included in the multiple regression", sep="")
      rela_fit <- lm(formula, mtcars)
      da_df <- dominanceAnalysis(rela_fit)
      rp <- plot(da_df) + coord_flip() +
        theme(legend.position= "none")
      print(rp)
      } 
  }
  cat("\n\n",bold(green("Hope this was helpful!\n")))
# back function to go one back to the regression understanding menu  
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
# }
# }

select <- readline("Please enter the number associated with the option:")
if (select == 1){
  cat(underline("Here are the significant results where the p value was less than 0.05:"),"\n\n", sep = "")
  for(i in 2:nrow(results)){
    if(results[i, 4] < .05){
      cat("For a 1 point change in ",  bold(dimnames(results)[[1]][i]),
          ",", blue(input1), " is expected to change by", red(results[i, 1]),".\n", sep = ""

fit <- mreg(mpg ~ cyl + am + carb, mtcars)  
summary(fit)
r.s <- summary.lm(fit)$r.squared
ars <- summary.lm(fit)$adj.r.squared
ars
