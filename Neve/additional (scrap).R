strsplit()


# importR
# ggplot2
# stringr
# dplyr
# qacReg

library(importR)
library(ggplot2)
library(dplyr)
library(stringr)

import()

dfname <- "r"
dfname <- as.name(dfname)
get(dfname)
dfname[1] <- df

test <- import()

cat(paste(letters, 100* 1:26), fill = TRUE, labels = paste0("{", 1:10, "}:"))

 