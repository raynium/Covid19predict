library(tseries)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)
library(ggpubr)


# Compare last 7 days
dfUK = read.csv("data/UK_data.csv", header = T)
dfUK = dfUK[c(1:nrow(dfUK)), c(1,3,4,15,16)]

gridSearch = list(c(i=0,j=0,k=0,sum = 0))
cnt = 1
for (i in 2:4){
  for (j in 2:4){
    for (k in 1:6){
      arima.order = c(i, j, k)
      #print(c(i,j,k))
      UK.arima.recent = arima(dfUK$confirmed[1:(nrow(dfUK)-7)], order = arima.order)
      pred.recent<-predict(UK.arima.recent, n.ahead=7)
      pred.recent = as.data.frame(pred.recent)
      
      dfRecent = as.data.frame( dfUK$confirmed[(nrow(dfUK)-6):nrow(dfUK)])
      dfRecent$Prediction = pred.recent$pred
      colnames(dfRecent) = c("Real","Prediction")
      dfRecent$id = dfUK$id[(nrow(dfUK)-6):nrow(dfUK)]
      dfRecent$Error = (dfRecent$Real-dfRecent$Prediction)^2
      gridSearch[[cnt]] = c(i,j,k,sum = sum(dfRecent$Error))
      cnt = cnt+1
      #print(i,j,k,sum(dfRecent$Error)) 
    }
  }
}

gridSearch = as.data.frame(transpose(gridSearch))
colnames(gridSearch) = c("i", "j","k","sum")
plot(gridSearch$i*100+gridSearch$j*20+gridSearch$k, gridSearch$sum)
bestGrid = gridSearch[which.min(gridSearch$sum),]
bestGrid$sum = as.integer(bestGrid$sum)

# Use the best grid to predict

dfUK = read.csv("data/UK_data.csv", header = T)
dfUK = dfUK[c(1:nrow(dfUK)), c(1,3,4,15,16)]

arima.order = c(bestGrid$i, bestGrid$j, bestGrid$k)

UK.arima = arima(dfUK$confirmed, order = arima.order)
summary(UK.arima)
pred<-predict(UK.arima,n.ahead=30)
pred = as.data.frame(pred)

temp = c(dfUK$confirmed,pred$pred)
dfUK[nrow(dfUK)+30,] = 0
dfUK$pred = temp
dfUK$id = rownames(dfUK)
dfUK$newConfirmedPred = 0
for (item in 2:nrow(dfUK)) {
  dfUK$newConfirmedPred[item] = dfUK$pred[item] - dfUK$pred[item-1]
}


subData = dfUK[c((nrow(dfUK)-75):nrow(dfUK))-15,]

# Plot total prediction
plotT1 = ggplot(data = subData, aes(x = as.integer(id), y = pred, color = "Predict"))+
  geom_line(linetype = "dashed")+
  geom_line(aes(x = as.integer(id), y = confirmed, color = "Real"),size = 2, lineend = "round")+
  xlab("Days")+
  labs(color = "")+
  ylab("Total Confirmed")+
  annotate("text", label = "p, d, q, SSE (Grid BEST)", x = 60, y = 210000, size = 3, hjust = 0, colour = "black", family = "mono", show.legend = FALSE)+
  annotate("text", label = toString(bestGrid), x = 60, y = 200000, size = 3, hjust = 0, colour = "black", family = "mono", show.legend = FALSE)+
  theme_minimal()+
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
  theme(legend.key.size =  unit(2, "mm"),
        legend.position="bottom")

ggsave("prediction/ARIMAtotal.png", scale = 2.5, width = 8, height = 4, units = "cm", dpi = "retina")

# Plot Log trend

ggplot(data = dfUK, aes(x = pred, y = newConfirmedPred)) +
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

subData = dfUK[c((nrow(dfUK)-35):(nrow(dfUK)-20)),c(1,2,6)]
subData$pred = as.integer(subData$pred)
subData$pred[1:6] = NA
subData = melt(subData)

plotC1 = ggplot(data = subData, aes(x = as.factor(id), y = value, color = variable))+
  geom_col(size = 0,aes(fill = variable), show.legend = F)+
  geom_label(aes(label = value, fill = variable),vjust = -0.2, colour = "white", show.legend = FALSE)+
  annotate("text", label = "TODAY", x = 6, y = 5000, size = 3, colour = "white",fontface = "bold", show.legend = FALSE)+
  xlab("Days")+
  ylab("Total Confirmed")+
  theme_minimal()+
  labs(fill = "",color = "")+
  scale_y_continuous(limits = c(0,max(subData$value, na.rm = T)+20000), expand = c(0, 2000))+
  scale_fill_hue(c=60, h = c(20,220))
plotC1
ggsave("prediction/Bar.png", scale = 2.5, width = 10, height = 4, units = "cm", dpi = "retina")

# Compare last days
dfUK = read.csv("data/UK_data.csv", header = T)
dfUK = dfUK[c(1:nrow(dfUK)), c(1,3,4,15,16)]
arima.order = c(bestGrid$i, bestGrid$j, bestGrid$k)
UK.arima.recent = arima(dfUK$confirmed[1:(nrow(dfUK)-7)], order = arima.order)
pred.recent<-predict(UK.arima.recent, n.ahead=7)
pred.recent = as.data.frame(pred.recent)

dfRecent = as.data.frame( dfUK$confirmed[(nrow(dfUK)-6):nrow(dfUK)])
dfRecent$Prediction = pred.recent$pred
colnames(dfRecent) = c("Real","Prediction")
dfRecent$id = dfUK$id[(nrow(dfUK)-6):nrow(dfUK)]
dfRecent$Error = abs(dfRecent$Real-dfRecent$Prediction)/(dfRecent$Real)
sum(dfRecent$Error)

plotE1 = ggplot(data = dfRecent, aes(x = as.integer(id), y = Error))+
  geom_col()+
  geom_label(aes(label = as.integer(abs(dfRecent$Real-dfRecent$Prediction))),vjust = -0.4, fontface = "bold", show.legend = FALSE, size = 3)+
  xlab("Days")+
  ylab("Relative Error in past 7 days")+
  theme_minimal()+
  scale_y_continuous(limits = c(0,max(dfRecent$Error)+0.003), expand = c(0,0),labels = scales::percent)
plotE1
ggsave("prediction/Error.png", scale = 3, width = 5, height = 5, units = "cm", dpi = "retina")


ggarrange(plotT1,plotE1, widths = c(0.8,0.4))

ggsave("prediction/Compare.png", scale = 2.5, width = 10, height = 4, units = "cm", dpi = "retina")

