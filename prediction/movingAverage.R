library(tseries)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)

dfUK = read.csv("data/UK_data.csv", header = T)

UK.arima = arima(dfUK$confirmed[1:(nrow(dfUK)-5)], order = c(3, 3, 3))
summary(UK.arima)
pred<-predict(UK.arima,n.ahead=30)
pred = as.data.frame(pred)

dfUK$pred = dfUK$confirmed
dfUK$se = 0
dfUKpred = bind_rows(dfUK[c(1:(nrow(dfUK)-5)), c(1:length(dfUK))],pred)

dfUKpred$id = rownames(dfUKpred)
dfUKpred$newConfirmedPred = 0
for (item in 2:nrow(dfUKpred)) {
  dfUKpred$newConfirmedPred[item] = dfUKpred$pred[item] - dfUKpred$pred[item-1]
}


subData = dfUKpred[c((nrow(dfUKpred)-60):nrow(dfUKpred)), c(1:length(dfUKpred))]

# Plot total prediction
ggplot(data = subData, aes(x = as.integer(id), y = pred, color = "Predict"))+
  geom_line(linetype = "dashed")+
  geom_line(data = dfUK[c((nrow(dfUK)-60):nrow(dfUK)), c(1:length(dfUK))], aes(x = as.integer(id), y = confirmed, color = "Real"),size = 2, lineend = "round")+
  xlab("Days")+
  labs(color = "")+
  ylab("Total Confirmed")+
  theme_minimal()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")
ggsave("prediction/ARIMAtotal.png", scale = 2.5, width = 8, height = 4, units = "cm", dpi = "retina")

# Plot Log trend

ggplot(data = dfUKpred, aes(x = pred, y = newConfirmedPred)) +
  geom_line(size = 0.8, lineend = "round",aes(color = "Prediction"))+
  geom_line(data = dfUK, aes(x = dfUK$confirmed, y = dfUK$newConfirmed, color = "UK"), size = 1.5)+
  geom_abline(aes(slope = 1, intercept = 0,  color = "Unlimited growth"), show.legend = T, size = 2)+
  xlab("Total confirmed")+
  labs(color = "Trend")+
  ylab("New confirmed")+
  theme_light()+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")+
  scale_x_log10(limits = c(55,316123), breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))+
  # scale_x_continuous(trans='log10',breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(limits = c(30,31612),breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))

ggsave("prediction/UKlogTrend.png", scale = 2.5, width = 9, height = 5, units = "cm", dpi = "retina")


# Compare prediction