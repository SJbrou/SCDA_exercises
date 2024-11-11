
library(zoo)
library(forecast)
library(TTR)

#import data from CSV
AS <- read.csv("D:/Courses/Supply Chain Data Analytics 2024/Week 2/Tutorial/ApplianceShipments.csv")

#convert data to a time series object
AS.ts <- ts(AS$Shipments,start=c(1985,1), end=c(1989,4),freq=4)

AS.ts


#part a)
#plot the original time series
plot(AS.ts, xlab="Time", ylab="Shipments", bty="l",xlim=c(1985,1989), ylim=c(3500,5500))

#part b)
#create centered and trailing moving averages
ma.centered <- rollmean(AS.ts,k=4,align="center")  #order is window size
ma.centered 
ma.trailing <- rollmean(AS.ts,k=4,align="right") 
ma.trailing

# add centered moving average line
lines(ma.centered,lwd=2,lty=2, col="red")

#add trailing moving average line
lines(ma.trailing,lwd=2,lty=3, col="blue")
legend(1987,5500, c("Shipments","Centered Moving Average"), lty=c(1,1))

#part c)
#Make training and validation sets
#define validation period
nValid <- 4
#define training period
nTrain <- length(AS.ts) - nValid
#define training set
train.ts <- window(AS.ts, start=c(1985,1), end=c(1985,nTrain))
#define validation set
valid.ts <- window(AS.ts, start=c(1985,nTrain+1), end=c(1985,nTrain+nValid))

train.ts
valid.ts

#part d)
#Do the smoothing
#fit a smoothing model using ets() function. You could use ses(), holt(), or hw() depending on your time series characteristics
shipment <- ets(train.ts, model="AAA") #alternatively you could fit ZMA or ZAM or ZMM
#do the forecast based on the fitted model
shipment.forecast <- forecast(hwin,h=nValid,level=0)

shipment.forecast

#calculate the accuracy on validation data
accuracy(shipment.forecast, valid.ts)
#the output of the accuracy() shows you the metrics on both training data and validation data. To assess the performance of your model without any bias, you should always look at validation data (unseen data).



#part e)
#plot
#par(mfrow =c(2,1))
#plot the forecast for validation period
plot(shipment.forecast, xlab="Time", ylab="Shipments", bty="l",xlim=c(1985,1990), ylim=c(3500,5500))

#plot the actual data for validation period
lines(valid.ts)

#Plot the fitted values for training period
lines(shipment.forecast$fitted,lwd=2,col="blue")
#shipment.forecast$fitted: this gives you the fitted values on the training data

#part f)
#plot the residuals of the training data
plot(shipment.forecast$residuals, xlab="Time", ylab="Shipments", bty="l",xlim=c(1985,1989.8),
     ylim=c(0,0.1)) #multiplitive errors residuals are (real value / fitted values) - 1.
#shipment.forecast$residuals: this gives you the residuals on the training data

