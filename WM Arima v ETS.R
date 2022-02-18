library(forecast)
library(quantmod)

#change the date range to what y'all are using
getSymbols("WM",from="2010-01-01",to="2021-10-02", periodicity = "monthly")
names(WM)[names(WM) == "WM.Adjusted"] <- "WM"
WM <- data.frame(Date = index(WM), coredata(WM) )

myts <- ts(WM$WM, start=c(2010, 1), end=c(2021, 10), frequency=12)
autoplot(myts, 
         main = "Waste Management STOCK PRICE, January 2010 - October 2021", 
         ylab = "Adusted Close")

auto.arima(myts, ic = "aic")
fit <- arima(myts, order=c(0, 1, 0))
accuracy(fit)
forecast(fit, 48)
plot(forecast(fit, 48))

fit2 <- ets(myts)
accuracy(fit2)
forecast(fit2, 48)
plot(forecast(fit2, 48))
