illegal <- function(name){
  name_sep <- unlist(strsplit(name, ""))
  error <- c()
  for(i in name_sep){
    error[[i]] <- ifelse(identical(".", i) == TRUE | 
                           identical("_", i) == TRUE | 
                           identical("-", i) == TRUE, FALSE,
                         ifelse(grepl("[[:punct:]]", i) == TRUE, TRUE, 
                                ifelse(identical(" ", i) == TRUE, TRUE, FALSE)))}
  error <- unlist(error)
  "TRUE" %in% error}
