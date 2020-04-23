# setwd("Covid19predict")
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)

dfUK = read.csv("data/UK_data.csv", header = T)
dfUK$newConfirmed = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newConfirmed[item] = dfUK$confirmed[item] - dfUK$confirmed[item-1]
}


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
# dfChina_m = read.csv("China_m.csv", header = T)
# dfKorea_s = read.csv("korea_s.csv", header = T)
# dfGermany = read.csv("germany.csv", header = T)
# dfItaly = read.csv("italy.csv", header = T)
# dfSpain = read.csv("spain.csv", header = T)

# Remove abnomal point
dfEUData = dfEUData[-c(47),]

ggplot(data = dfUK, aes(x = dfUK$confirmed, y = dfUK$newConfirmed, color = "UK")) +
  geom_line(size = 0.8, lineend = "round")+
  #geom_line(data = dfChina_m, aes(x = China, y=china_new, color = "China"))+
  #geom_line(data = dfKorea_s, aes(x = KoreaS, y=KoreaS_new, color = "KoreaS"))+
  geom_line(data = dfEUData, aes(x = Italy, y=Italy_new, color = "Italy"), alpha = 0.4)+
  geom_line(data = dfEUData, aes(x = Spain, y=Spain_new, color = "Spain"), alpha = 0.4)+
  geom_line(data = dfEUData, aes(x = Germany, y=Germany_new, color = "Germany"), alpha = 0.4)+
  geom_point(data = dfUK[c(nrow(dfUK)),c(1:length(dfUK))], aes(x = confirmed, y = newConfirmed), size = 2, alpha = 0.4)+
  #stat_smooth(geom='line', alpha=0.6, se=FALSE, size = 1.5)+
  geom_abline(aes(slope = 1, intercept = 0, color = "Unlimited growth"), show.legend = T, size = 2)+
  xlab("Total confirmed")+
  labs(color = "Trend")+
  ylab("New confirmed")+
  theme_light()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")+
  scale_x_log10(limits = c(55,316123), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  # scale_x_continuous(trans='log10',breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(30,31612),breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ggsave("plot/UKlogTrend.png", scale = 2.5, width = 7, height = 5, units = "cm", dpi = "retina")
