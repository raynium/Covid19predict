library(tseries)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)
library(ggpubr)
library(forecast)


# Compare last 7 days
dfUK = read.csv("data/UK_data.csv", header = T)
dfUK = dfUK[c(1:nrow(dfUK)), c(1,3,4,15,16)]
days = nrow(dfUK)



UK.autoARIMA = auto.arima(dfUK$confirmed, 
                          max.p = 21, max.q = 21, start.q = 1, max.d = 4, max.order = 42, 
                          seasonal = F,stepwise = F, 
                          #parallel = T, num.cores = 8, 
                          approximation = T,trace = T)
UK.autoARIMA

plotT1 = ggplot2::autoplot(forecast(UK.autoARIMA,20),color = "red")+
  xlab("Days")+
  ylab("Total Confirmed")+
  theme_minimal()+
  geom_point(data = dfUK[days,], aes(x = id+0.5, y = confirmed+ 0.4*newConfirmed), size = 4, alpha = 0.3, color = 'blue')+
  scale_x_continuous(limits = c(30,NA))+
  theme(legend.position="bottom")
plotT1
ggsave("prediction/ARIMAtotal.png", scale = 2.5, width = 8, height = 4, units = "cm", dpi = "retina")


tsdiag(UK.autoARIMA)
tmp = forecast(UK.autoARIMA,20)
pred = c(tmp[["fitted"]],tmp[["mean"]])
pred = as.data.frame(pred)
pred$UK = c(forecast(UK.autoARIMA,20)[["x"]],rep(NA,20))

colnames(pred) = c("Prediction","Confirmed")
pred$newConfirmedPred = 0
pred$newConfirmed = 0
for (item in 2:nrow(pred)) {
  pred$newConfirmedPred[item] = pred$Prediction[item] - pred$Prediction[item-1]
  pred$newConfirmed[item] = pred$Confirmed[item] - pred$Confirmed[item-1]
}
pred$day  =rownames(pred)

#subData = dfUK[c((nrow(dfUK)-75):nrow(dfUK))-15,]


# Plot total prediction

# plotT1 = ggplot(data = subData, aes(x = as.integer(id), y = pred, color = "Predict"))+
#   geom_line(linetype = "dashed")+
#   geom_line(aes(x = as.integer(id), y = confirmed, color = "Real"),size = 2, lineend = "round")+
#   xlab("Days")+
#   labs(color = "")+
#   ylab("Total Confirmed")+
#   annotate("text", label = "p, d, q, ML (Grid BEST)", x = 60, y = 210000, size = 3, hjust = 0, colour = "black", family = "mono", show.legend = FALSE)+
#   #annotate("text", label = toString(bestGrid), x = 60, y = 200000, size = 3, hjust = 0, colour = "black", family = "mono", show.legend = FALSE)+
#   theme_minimal()+
#   scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
#   theme(legend.key.size =  unit(2, "mm"),
#         legend.position="bottom")
# plotT1
#ggsave("prediction/ARIMAtotal.png", scale = 2.5, width = 8, height = 4, units = "cm", dpi = "retina")


# Plot Log trend

ggplot(data = pred, aes(x = Confirmed, y = newConfirmed)) +
  geom_line(size = 1.5, lineend = "round",aes(color = "Confirmed"))+
  geom_line(data = pred, aes(x = pred$Prediction, y = pred$newConfirmedPred, color = "Prediction"))+
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

pred.Recent =  pred[c((days-5):(days+10)),]
pred.Recent$day = as.integer(pred.Recent$day)
pred.Recent$Prediction = as.integer(pred.Recent$Prediction)
pred.Recent$newConfirmedPred = as.integer(pred.Recent$newConfirmedPred)
plotC1 = ggplot(data = pred.Recent, aes(x = day, y = Confirmed))+
  geom_col(size = 0,aes(x=day, y= Prediction, fill = "Prediction"),alpha = 0.9)+
  geom_col(size = 0,aes(fill = "Confirmed"), alpha = 0.8)+
  geom_label(aes(label = Confirmed, fill = "Confirmed"),vjust = -0.2, colour = "white", show.legend = FALSE,size = 3, alpha = 0.8)+
  geom_label(aes(label = c(rep(NA,6),pred.Recent$Prediction[7:16]),x=pred.Recent$day, y=pred.Recent$Prediction,fill = "Prediction"),vjust = -0.2, colour = "white", show.legend = FALSE,size = 3)+
  annotate("text", label = "TODAY", x = days, y = 5000, size = 3, colour = "white",fontface = "bold", show.legend = FALSE)+
  xlab("Day")+
  ylab("Total Confirmed")+
  theme_minimal()+
  labs(fill = "",color = "")+
  theme(legend.position="bottom")+
  scale_y_continuous(limits = c(0,max(pred$Prediction, na.rm = T)+20000), expand = c(0, 2000))
plotC1
ggsave("prediction/Bar.png", scale = 2.5, width = 10, height = 4, units = "cm", dpi = "retina")

plotC2 = ggplot(data = pred.Recent, aes(x = day, y = newConfirmed))+
  geom_col(size = 0,aes(x=day, y= newConfirmedPred, fill = "Prediction"),alpha = 0.9)+
  geom_col(size = 0,aes(fill = "Daily Confirmed"), alpha = 0.8)+
  geom_label(aes(label = newConfirmed, fill = "Daily Confirmed"),vjust = -0.2, colour = "white", show.legend = FALSE,size = 3, alpha = 0.8)+
  geom_label(aes(label = c(rep(NA,6),pred.Recent$newConfirmedPred[7:16]),x=pred.Recent$day, y=pred.Recent$newConfirmedPred,fill = "Prediction"),vjust = -0.2, colour = "white", show.legend = FALSE,size = 3)+
  annotate("text", label = "TODAY", x = days, y = 5000, size = 3, colour = "white",fontface = "bold", show.legend = FALSE)+
  xlab("Day")+
  ylab("Daily increase")+
  theme_minimal()+
  theme(legend.position="bottom")+
  labs(fill = "",color = "")+
  scale_y_continuous(limits = c(0,max(pred$newConfirmedPred, na.rm = T)+1000), expand = c(0, 0))
plotC2
ggsave("prediction/DailyBar.png", scale = 2.5, width = 10, height = 4, units = "cm", dpi = "retina")



# Compare last days

# dfUK = read.csv("data/UK_data.csv", header = T)
# dfUK = dfUK[c(1:nrow(dfUK)), c(1,3,4,15,16)]
# arima.order = c(bestGrid$i, bestGrid$j, bestGrid$k)
# UK.arima.recent = arima(dfUK$confirmed[1:(nrow(dfUK)-7)], order = arima.order)
# pred.recent<-predict(UK.arima.recent, n.ahead=7)
# pred.recent = as.data.frame(pred.recent)
# 
# dfRecent = as.data.frame( dfUK$confirmed[(nrow(dfUK)-6):nrow(dfUK)])
# dfRecent$Prediction = pred.recent$pred
# colnames(dfRecent) = c("Real","Prediction")
# dfRecent$id = dfUK$id[(nrow(dfUK)-6):nrow(dfUK)]
# dfRecent$Error = abs(dfRecent$Real-dfRecent$Prediction)/(dfRecent$Real)
# sum(dfRecent$Error)

plotE1 = ggplot(data = pred[c((days-7):days),], aes(x = as.integer(pred$day[(days-7):days]), y =  tmp[["model"]][["residuals"]][(days-7):days]))+
  geom_col()+
  geom_label(aes(label = as.integer(tmp[["model"]][["residuals"]][(days-7):days])), fontface = "bold", show.legend = FALSE, size = 3)+
  xlab("Days")+
  ylab("Residual in past 7 days")+
  theme_minimal()+
  ylim(-1000,1000)
plotE1
ggsave("prediction/Error.png", scale = 3, width = 5, height = 5, units = "cm", dpi = "retina")


ggarrange(plotT1,plotE1, widths = c(0.8,0.4))

ggsave("prediction/Compare.png", scale = 2.5, width = 10, height = 4, units = "cm", dpi = "retina")

