# setwd("Covid19predict")

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
write.csv(dfData, file = "data/UK_data.csv", row.names = F, quote = F)


# Read EU data
dfGlobalData = read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = T)

dfEUData =  subset(dfGlobalData, Country.Region == "Spain"| Country.Region == "Italy" | Country.Region =="Germany", select = c(2, 5:length(dfGlobalData)))
dfEUData = transpose(dfEUData)
colnames(dfEUData) = dfEUData[1,1:3]
dfEUData = dfEUData[6:nrow(dfEUData),1:3]

dfEUData$Spain_new = 0; dfEUData$Italy_new = 0; dfEUData$Germany_new = 0
dfEUData$Spain = as.numeric(dfEUData$Spain); dfEUData$Italy = as.numeric(dfEUData$Italy); dfEUData$Germany = as.numeric(dfEUData$Germany)
for (item in 2:nrow(dfEUData)) {
  dfEUData$Spain_new[item] = dfEUData$Spain[item] - dfEUData$Spain[item-1]
  dfEUData$Italy_new[item] = dfEUData$Italy[item] - dfEUData$Italy[item-1]
  dfEUData$Germany_new[item] = dfEUData$Germany[item] - dfEUData$Germany[item-1]
}

write.csv(dfEUData, file = "data/EU_confirmed.csv", row.names = F, quote = F)