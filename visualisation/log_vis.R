# setwd("~/Documents/GitHub/Covid19predict/visualisation")
library(ggplot2)
library(scales)

dfUK = read.csv("../daraPreProcess/historyfigures.csv", header = T)
# dfChina_m = read.csv("China_m.csv", header = T)
# dfKorea_s = read.csv("korea_s.csv", header = T)
dfGermany = read.csv("germany.csv", header = T)
dfItaly = read.csv("italy.csv", header = T)
dfSpain = read.csv("spain.csv", header = T)


dfUK$newConfirmed = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newConfirmed[item] = dfUK$confirmed[item] - dfUK$confirmed[item-1]
}


ggplot(data = dfUK, aes(x = dfUK$confirmed, y = dfUK$newConfirmed, color = "UK")) +
  geom_line(size =1)+
  #geom_line(data = dfChina_m, aes(x = China, y=china_new, color = "China"))+
  #geom_line(data = dfKorea_s, aes(x = KoreaS, y=KoreaS_new, color = "KoreaS"))+
  geom_line(data = dfItaly, aes(x = Italy, y=Italy_new, color = "Italy"), alpha = 0.4)+
  geom_line(data = dfSpain, aes(x = Spain, y=Spain_new, color = "Spain"), alpha = 0.4)+
  geom_line(data = dfGermany, aes(x = Germany, y=Germany_new, color = "Germany"), alpha = 0.4)+
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
  scale_y_log10(limits = c(10,31612),breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ggsave("UKlogTrend.png", scale = 2.5, width = 7, height = 5, units = "cm", dpi = "retina")
