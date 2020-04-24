# setwd("Covid19predict")
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)

# Read UK data
dfUK = read.csv("data/UK_data.csv", header = T)
dfUK$newConfirmed = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newConfirmed[item] = dfUK$confirmed[item] - dfUK$confirmed[item-1]
}

dfUK$newDeath = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newDeath[item] = dfUK$death[item] - dfUK$death[item-1]
}

# Read EU data
dfEUData = read.csv("data/EU_confirmed.csv")


dfChina_m = read.csv("data/China_m.csv", header = T)
dfKorea_s = read.csv("data/korea_s.csv", header = T)

# Remove abnomal point
dfEUData = dfEUData[-c(47),]

# Plot log trend compare

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


ggplot(data = dfUK, aes(x = dfUK$confirmed, y = dfUK$newConfirmed, color = "UK")) +
  #geom_line(size = 0.8, lineend = "round")+
  geom_line(data = dfChina_m, aes(x = China, y=china_new, color = "China"))+
  geom_line(data = dfKorea_s, aes(x = KoreaS, y=KoreaS_new, color = "KoreaS"))+
  #geom_line(data = dfEUData, aes(x = Italy, y=Italy_new, color = "Italy"), alpha = 0.4)+
  #geom_line(data = dfEUData, aes(x = Spain, y=Spain_new, color = "Spain"), alpha = 0.4)+
  #geom_line(data = dfEUData, aes(x = Germany, y=Germany_new, color = "Germany"), alpha = 0.4)+
  #geom_point(data = dfUK[c(nrow(dfUK)),c(1:length(dfUK))], aes(x = confirmed, y = newConfirmed), size = 2, alpha = 0.4)+
  #stat_smooth(geom='line', alpha=0.6, se=FALSE, size = 1.5)+
  geom_abline(aes(slope = 1, intercept = 0, color = "Unlimited growth"), show.legend = T, size = 2)+
  xlab("Total confirmed")+
  labs(color = "Trend")+
  ylab("New confirmed")+
  theme_light()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")+
  scale_x_log10(limits = c(40,316123), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  # scale_x_continuous(trans='log10',breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ggsave("plot/SuccessTrend.png", scale = 2.5, width = 7, height = 5, units = "cm", dpi = "retina")

# Plot daily case

ggplot()+
  #geom_point()+
  geom_col(data = dfUK[c((nrow(dfUK)-45):nrow(dfUK)),c(1:length(dfUK))], aes(x = id, y = newConfirmed),alpha = 0.5, size = 0)+
  geom_col(data = dfUK[c((nrow(dfUK)-45):nrow(dfUK)),c(1:length(dfUK))], aes(x = id, y = newDeath,color = "Death"),alpha = 0.5, size = 0, fill = "darkred")+
  xlab("Days")+
  labs(color = "")+
  ylab("Increase")+
  theme_minimal()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")+
  scale_y_continuous(expand = c(0, 20))

ggsave("plot/DailyIncrease.png", scale = 2.5, width = 9, height = 5, units = "cm", dpi = "retina")

ggplot(data = dfUK[c((nrow(dfUK)-45):nrow(dfUK)),c(1:length(dfUK))], aes(x = id, y = newDeath ))+
  #geom_point()+
  geom_col(data = dfUK[c((nrow(dfUK)-45):nrow(dfUK)),c(1:length(dfUK))], aes(x = id, y = newDeath ),alpha = 0.5, fill = 'darkred', size = 0)+
  xlab("Days")+
  ylab("New death")+
  theme_minimal()+
  scale_y_continuous(expand = c(0, 20))

ggsave("plot/DeathIncrease.png", scale = 2.5, width = 9, height = 5, units = "cm", dpi = "retina")
