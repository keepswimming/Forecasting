# MyData <- read.csv(file="C:\\Users\\Hubstaff\\Desktop\\02R21471EX054735R\\fwdfargohealthgroup02r21471ex054735r\\work5R\\Dataset.csv", header=FALSE, sep=",")

MyData <-read.csv(file.choose())

head(MyData)
summary(MyData)
#clean the datsaset
complete.cases(MyData)#clean the file 
which(complete.cases(MyData))#find the missing value
which(!complete.cases(MyData))
MyData1<-MyData[-2,]#remove 2 row because of *.
head(MyData1)
any(is.na(MyData1))

#Mice
library(mice) #implement the mice
md.pattern(MyData1) #observe the missing value
library(VIM)
#visual which represents missing values
mice_plot <- aggr(MyData1, col=c('navyblue','yellow'), 
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(MyData1), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
#impute the missing values.
imputed_Data <- mice(MyData1, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

#Amelia
library(Amelia)
amelia_fit <- amelia(MyData, m=5, parallel = "multicore", noms = "Month")
missmap(MyData) #check the missing value
# fit an ARIMA model of order P, D, Q
library(forecast)
tsMyData1 = ts(MyData1)
tsdata =auto.arima(tsMyData1[,1])
boxplot(tsdata$x)
arima(x = tsdata$x , order = c (1, 1, 1))
arima_data <- arima(x = tsdata$x , order = c (1, 1, 1))
arima(x = tsdata$x, order = c (2, 1, 2))
summary(arima_data)
plot(arima_data) #plot the model
residuals(arima_data)
plot(residuals(arima_data))
accuracy(arima_data)#accuracy of the model
forecast(arima_data)
plot(forecast(arima_data)) #forcasting
cycle(arima_data)# print the cycle across years
# fit an ARIMA model of order P, D, Q
library(forecast)
tsMyData1 = ts(MyData1)
as.numeric(unlist(MyData1))
auep<- ets(tsdata$fitted)
summary(auep)
plot(auep)
residuals(auep)
plot(residuals(auep))
accuracy(auep)
forecast(auep)
plot(forecast(auep))


#Holt-Winters forecast
demand <- ts(MyData1, start = c(2000, 1), frequency = 12)
plot(demand)
hwf <- HoltWinters(demand)
summary(hwf)
plot(hwf[[1]],hwf[[2]])#plot the Holf winter forecast 
residuals(hwf)#independent variable on the horizontal axis
plot(residuals(hwf))
xp <- predict(hwf, n.ahead = 12, prediction.interval = T, level = 0.95)
scatter.smooth(xp)

