# setwd("~/Documents/GitHub/Covid19predict/daraPreProcess")

library(rjson)
library (plyr)

library(reshape2) # library for reshaping data (tall-narrow <-> short-wide)
library(deSolve) # library for solving differential equations
json_file = "https://api.covid19uk.live/historyfigures"
readData =  fromJSON(paste(readLines(json_file), collapse="", simplify=TRUE))

hisData = readData[["data"]]
for (item in 1:length(hisData)) {
  hisData[[item]] = unlist(as.character(hisData[[item]]), use.names = T)
}

dfData = as.data.frame( Reduce(rbind, hisData))
colnames(dfData) = names(readData[["data"]][[1]])
rownames(dfData) = NULL
write.csv(dfData, file = "historyfigures.csv", row.names = F, quote = F)
