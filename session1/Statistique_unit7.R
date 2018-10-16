library(forecast)

setwd("C:/workspace/Statistique")
data_passengers <- read.csv2("Unit7_Passengers.csv", header = TRUE)

users <- data_passengers$USERS
  
TS_USERS <- ts(users, frequency = 12, start = c(1989, 1), end = c(2015, 12))

plot(TS_USERS, main = "My time series plot", type = "l", lwd = 2, col = "blue", xlab = "month", ylab = "users")

stl(TS_USERS, s.window="periodic")
plot(stl(TS_USERS, s.window="periodic"))

#An uilisatio of the ARIMA FUNCTION
model_best <- auto.arima(TS_USERS)

prediction <- predict(model_best, n.ahead = 9)

ts.plot(TS_USERS, prediction$pred, lty = c(1,3), col=c(4,2))

upper <- prediction$pred + 1.96*prediction$se
lower <- prediction$pred - 1.96*prediction$se


plot(prediction$pred, col = "blue") 
lines(upper, col = "red", lty = 3)
lines(lower, col = "red", lty = 3)

actual <- c(6441.1, 6446.6, 7466.3, 7652.31, 7935.89, 8154.57,8712.82, 8706.77, 8762.54)

ts_actual <- ts(actual, frequency = 12, start = c(2016,1), end = c(2016,9))
#Now plot the actual values together with the forecasts and their C.I.
#plot the actual values in the forecasted periods as red points
plot(ts_actual,col="red", type="p",ylim=c(6000, 9000))
#add to the plot the confidence intervals of the forecasts as dotted blue lines
lines(upper,col="blue",lty=3)
lines(lower,col="blue",lty=3)
#add to the plot the forecasted values as blue points
points(prediction$pred,col="blue")

prediction_forecast <- forecast(model_best, h = 14, level = 0.95)

plot(prediction_forecas)t

prediction_value <- prediction_forecast$mean
prediction_upper <- prediction_forecast$upper
prediction_lower <- prediction_forecast$lower

plot(ts_actual,col="red", type="p",ylim=c(6000, 9000))
#add to the plot the confidence intervals of the forecasts as dotted blue lines
lines(prediction_upper,col="blue",lty=3)
lines(prediction_lower,col="blue",lty=3)
#add to the plot the forecasted values as blue points
points(prediction_value,col="blue")

accuracy(model_best)
accuracy(prediction$pred, ts_actual)
