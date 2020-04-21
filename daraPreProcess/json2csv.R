setwd("~/Documents/GitHub/Covid19predict/daraPreProcess")

library(rjson)
library (plyr)

library(reshape2) # library for reshaping data (tall-narrow <-> short-wide)
library(deSolve) # library for solving differential equations
json_file = "https://api.covid19uk.live/historyfigures"
readData =  fromJSON(paste(readLines(json_file), collapse="", simplify=TRUE))

hisData = readData[["data"]]

write.csv2(hisData, file = "historyfigures.csv")
