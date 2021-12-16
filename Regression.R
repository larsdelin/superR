#packages needed -
library(rlang)
library(dplyr)
library(crayon)
library(ggeffects)
library(ggplot2)
library(effects)
library(patchwork)
library(dominanceanalysis)
library(stats)
arifa <- mtcars
super.mreg <- function(){
    df <- e.super$df
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
    mreg <- function(formula, df){
      fit <- lm(formula, df)
      fit$call <- str2lang(paste("lm(formula=", deparse(substitute(formula)),
                                 ", data=", deparse(substitute(df)), ")"))
      class(fit) <- c("mreg", "lm")
      return(fit)
    }
    #Introduction to reg ----
    repeat{
      cat(underline("Welcome to conducting a multiple regression!\n"),
          "Would you like a brief overview of what a multiple regression entails?\n",
  "  1.", green(" Yes\n"),
  "  2.", red(" No"),sep="")
      overview <- readline("Please enter the number associated with the option:")
      result <- options(overview, 1:2, main())
      if(!(is.null(result$go))){break}
      overview <- result$option
      if (overview == 1){
        cat(underline(green("\nMultiple linear regression")), " also known simply as multiple regression, is a statistical technique 
that uses ", bold("several explanatory variables"), " (i.e predictor variables or independent variables) to
predict the outcome of a ", bold("response variable"), " (i.e predicted variable or dependent variable). 
\nMultiple regression is an extension of simple linear regression as you can input more than one 
explanatory variable that gives you a better and more in-depth analysis.\n",
green("\nIn summary"), ", multiple regression will allow you to see how the explanatory variables are affecting 
the response variables, which is useful in trying to make conclusions regarding certain hypothesis.", sep="")
      }
      cat("\n----------------------------------------------------------------------------------------------------------------",sep="")
      cat(bold(red("\nNote:")), " Please ensure all variables that will be used in this regression have", 
red$bold(" no missing values"), " and are the \nsuitable type and class for the multiple regression to provide meaningful results.","\n",
"\n> If at any point you want to go back to the main menu just type: ", green("!!!"), sep="")
      cat("\n----------------------------------------------------------------------------------------------------------------",sep="")
      cat("\nNow you will be asked to input the variable you want to predict (i.e response/dependent variable).", "\n",
          "Please ensure you enter the ", bold(red("exact name")), " of this variable.", sep = "")
      #Input dependent variable ----
      repeat{
        dv <- readline("Enter dependent variable name here and hit return:")
        go <- back(dv, main())
        if(!(is.null(go))){break}
        if(length(setdiff(dv, names(df))) > 0){
          cat("\n'", bold(underline((setdiff(dv, names(df))))),"'", " is an", bold(red(" invalid"))," variable that is not in the dataset"," '", blue(e.super$dfname),"'", "\n",
              "\nPlease input a variable that is in the dataset"," '", blue(e.super$dfname),"'", sep = "")
        }
        else{
          input1 <- dv
          cat("\nThanks! The variable you chose is: ", bold(input1),"\n", sep="")
          break
        }
      }
      if(!(is.null(go))){break}
      cat("\nNow you will be asked to enter the ", bold(red("exact name")), " of the explanatory variables (i.e independent/predictor)
that will be tested against the response variable.\n", sep="")
      #Input independent variable ----
      repeat{
        iv <- readline("Please enter the independent variables seperated by a space:")
        go <- back(iv, main())
        if(!(is.null(go))){break}
        iv <- strsplit(iv, " ")
        iv <- iv[[1]]
        if (length(setdiff(iv, names(df))) > 0){
          cat("\nThe following variables are ", bold(red("invalid ")),"as they are not in the dataset"," '", blue(e.super$dfname),"':","\n",
              underline(bold(setdiff(iv, names(df)))),"\n\n", 
              "Please input a variable that is in the dataset"," '", blue(e.super$dfname),"'", sep = "")
        }else{
          input2 <- iv
          cat("\nThanks! The variables you chose are: ", bold(input2), sep="")
          break
        }
      }
      #Results ----
      if(!(is.null(go))){break}
      cat(green(bold(underline("\nHere are the results for the multiple regression:"))))
      ls <- input1
      rs <- paste(input2, collapse="+")
      f <- paste(ls, "~", rs)
      formula <- as.formula(f)
      fit <- mreg(formula, df)
      print(summary.lm(fit))
      results <- summary.lm(fit)$coefficients
      cat(underline("Do you need help understanding the", bold("output and significance"), "of the above regression?"),"\n",
  "  1.", green(" Yes\n"),
  "  2.", red(" No"),sep="")
      select <- readline("Please enter the number associated with the option:")
      result <- options(select, 1:2, main())
      if(!(is.null(result$go))){break}
      select <- result$option
      if (select == 1){
        cat(underline("\nHere are the ", bold(blue("significant results")), " where the p value was less than 0.05:"),"\n\n", sep = "")
        for(i in 2:nrow(results)){
          if(results[i, 4] < .05){
            cat("For a 1 point change in ",  bold(dimnames(results)[[1]][i]),
                ", ", blue(input1), " is expected to change by ", red(results[i, 1]),".\n", sep = "")
          }
        }
      }
      repeat{ 
        cat("\n",underline("Additionally, do you want to do any of the following?"),"\n", 
" 1.", blue("Understand the ", bold("R-squared value and Adjusted R-squared"), " value\n"),
" 2.", green("Get ", bold("plots of each paired relationship"), " between the dependent and each independent variable\n"),
" 3.", magenta("Get the ", bold("relative importance"), " of each variable in your model\n"),
" 4.", yellow("Go back to the ", bold("main menu"), sep=""))
        input <-  readline("Please enter the number associated with the option:")
        result <- options(input, 1:4, main())
        if(!(is.null(result$go))){break}
        input <- result$option
        #R and Adjust R squared analysis ----
        if (input == 1){
          r.s <- summary.lm(fit)$r.squared
          ars <- summary.lm(fit)$adj.r.squared
          cat(red("\nThe R-squared value for this analysis is "), bold(r.s),
              "\n> The R-squared value explains the degree to which your explanatory variables explains the variation 
of your response variable.\n> So an R-squared value of ", r.s," means that ",(r.s)*100,"%"," of the variation in the response variable 
is explained by the explanatory variables in the analysis.","\n\n", sep ="")
          cat(red("The Adjusted-R squared value for this analysis is "), bold(ars),
              "\nThe Adjusted R-squared increases when new terms improve the model more than would be expected by chance and
it decreases when a explanatory variable improves the model by less than expected. 
> The Adjusted R-squared helps to determine how much of the significance is just due to addition of 
explanatory variables. 
> The adjusted R-squared compensates for the addition of variables and only increases if the new 
explanatory added enhances the model above what would be obtained by probability. Conversely, it will 
decrease when a explanatory variable improves the model less than what is predicted by chance\n", sep="")
        }
        # Mreg Plots ----
        if (input == 2){ 
          cat(bold(blue(underline(("\nOn the right is the plots! >>>>")))))
          cat("\n>These plots", bold(" show the relationship of response to each explanatory variable seperately"), " so that you can 
understand the relationship between each response varibale and explanatory variables better.\n", sep="")
          p <- plot.mreg(fit, points=TRUE)
          print(p)
        }
        # General dominance plots ----
        if (input == 3){
          cat(bold(magenta(underline("\nOn the right is your general dominance plot! >>>>"))),
              "\nThis plot shows you the ", bold("average importance of each explanatory variable"), " relative to all the other
explanatory variables included in the multiple regression.\n", sep="")
          rela_fit <- lm(formula, mtcars)
          da_df <- dominanceAnalysis(rela_fit)
          rp <- plot(da_df) + 
            labs(x = "Variable Name", 
                 y = "Relative Importance", 
                 title = "General Dominance Plot of Response Variables in Analysis")+ 
                   coord_flip() +
                   theme(legend.position= "none")
          print(rp)
        }
        if (input == 4){
          cat()
          main()
          break
        }
      }
      if(!(is.null(result$go))){break}
    }
  }
  
  
  df_select()
  
}


super()


