library(forecast)
library(quantmod)

getSymbols("AMZN",from="2014-01-01",to="2020-10-02", periodicity = "monthly")
names(AMZN)[names(AMZN) == "AMZN.Adjusted"] <- "AMZN"
AMZN <- data.frame(Date = index(AMZN), coredata(AMZN) )

myts <- ts(AMZN$AMZN, start=c(2014, 1), end=c(2020, 10), frequency=12)
autoplot(myts, 
         main = "AMAZON STOCK PRICE, January 2014 - October 2020", 
         ylab = "Adusted Close")

auto.arima(myts, ic = "aic")
fit <- arima(myts, order=c(0, 1, 0))
accuracy(fit)
forecast(fit, 24)
plot(forecast(fit, 24))

fit2 <- ets(myts)
accuracy(fit2)
forecast(fit2, 24)
plot(forecast(fit2, 24))