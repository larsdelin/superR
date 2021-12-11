remove_vars <- function(){
    seenames <- readline("If you would like to see the names of the variables in your dataframe, type YES, otherwise type anything or hit the return button: ")
    back(seenames, subset_options("1"))
    if(seenames == "YES"){
      cat("The variables in", dfname, "are:", names(df))
    }
    subset_options("1")
  }
  vars <- readline("Enter in each variable you want to remove: ")
  remove <- readline("Are you sure you would like to remove these variables from you dataframe? Enter either YES or NO")
  back(remove, subset_options())
  
  if(remove == "YES"){
    cat("\nWould you like to:
1. Create a copy of your dataframe and remove variables from that 
   to keep the original dataframe in your environment
2. Remove the variables from the origional dataframe")
    option <- readline("Type the number associated: ")
    back(option, subset_options())
    if(option == "1"){
      cat("\nYou will have to provide a name for your dataframe subset")
      dfname <- naming("df")
      back(dfname, )
      checkname(dfname, df_select(), df_options("1"), "df")
    }
    
    
    
  }
  else if(remove == "NO"){
    subset_options("1")
  }
    
  df <- df %>% 
    select(-all_of(vars))

}

valid <- vars %in% names(test)





vars <- readline("")
my <- remove_vars(vars)


vars <- c("spam", "from", "cc")
test <- new %>% 
  select(-all_of(vars))


attach(test)
exists()




attach(testdata)
exists("g")

fun <- function(vars){
  df <- assign(dfname[[1]], "test", envir = .GlobalEnv)
  attach(df)
  vars
}
fun(vars)

dfname <- "testdata"
dfname <- as.list(dfname)
dfname
dfname[[1]]

get(dfname)

n <- df[, vars]

# a name as a value --> use as a dataframe name
