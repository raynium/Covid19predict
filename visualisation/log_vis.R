setwd("~/Documents/GitHub/Covid19predict/visualisation")
library(ggplot2)
library(scales)

dfUK = read.csv("../daraPreProcess/historyfigures.csv", header = T)

dfUK$newConfirmed = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newConfirmed[item] = dfUK$confirmed[item] - dfUK$confirmed[item-1]
}

ggplot(data = dfUK, aes(x = dfUK$confirmed, y = dfUK$newConfirmed, color = "UK")) +
  geom_line()+
  stat_smooth(geom='line', alpha=0.6, se=FALSE, size = 1.5)+
  geom_abline(aes(slope = 1, intercept = 0, color = "Unlimited growth"), show.legend = T, size = 2)+
  xlab("Total confirmed")+
  labs(color = "Trend")+
  ylab("New confirmed")+
  theme_light()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")+
  scale_x_continuous(trans='log10',breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(trans='log10',breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ggsave("UKlogTrend.png", scale = 2.5, width = 7, height = 5, units = "cm", dpi = "retina")
