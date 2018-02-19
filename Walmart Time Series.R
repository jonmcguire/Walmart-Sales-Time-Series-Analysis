#time series
#AR, MA, ARMA, ARIMA

#walmart

#set working directory
setwd("C:/Users/Jon/Desktop")

#load packages
library(tseries)
library(ggplot2)
library(dplyr)

#load data
stores=read.csv("stores.csv")
features=read.csv("features.csv")
train=read.csv("train.csv")

#merge data
train=merge(train,stores, all.x = TRUE)
train=merge(train, features, all.x = TRUE)

#change date to date type
str(train)
train$Date<- as.Date(train$Date, "%m/%d/%Y")

#create sum per day over time graph
graph1<-train %>% 
  group_by(Date) %>% 
  summarise(sum=sum(Weekly_Sales)) %>%
  ggplot( aes(x=Date, y=sum))+geom_line()+geom_smooth()

graph1

train$Date<-ts(train$Date)
  
#test for stationarity Dickey-Fuller test
adf.test(train$Weekly_Sales, alternative="stationary", k=12)
#it is stationary

 
#create acf graph 
graph2<-train %>% 
  group_by(Date) %>% 
  summarise(sum=sum(Weekly_Sales))

#ARIMA

#AR
pacf(graph2$sum)#value of p= 1

#I
#d=0 plot was already stationary

#MA
acf(x=graph2$sum)  #looking for q = 4

graph2$Date<-ts(graph2$Date)

fit<- arima(graph2$sum, c(1,0,4), seasonal = c(0,0,1))
pred<-predict(fit, n.ahead=10*12)

ts.plot(pred$pred)
