#libraries you need for this project to run:

library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)
library(plyr)
#Loads the company stock using ticker

getSymbols("CE",from="2010-01-01",to="2021-10-01")
getSymbols("FB",from="2010-01-01",to="2021-10-01")
getSymbols("TSLA",from="2010-01-01",to="2021-10-01")
getSymbols("AAPL",from="2010-01-01",to="2021-10-01")
getSymbols("GOOGL",from="2010-01-01",to="2021-10-01")

#Use R to observe a stock's performance
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

CE%>%Ad()%>%chartSeries()
CE%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

FB%>%Ad()%>%chartSeries()
FB%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

TSLA%>%Ad()%>%chartSeries()
TSLA%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

AAPL%>%Ad()%>%chartSeries()
AAPL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

GOOGL%>%Ad()%>%chartSeries()
GOOGL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2020')

#Stock returns in log

CE_log_returns<-CE%>%Ad()%>%dailyReturn(type='log')
FB_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')
TSLA_log_returns<-TSLA%>%Ad()%>%dailyReturn(type='log')
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')
GOOGL_log_returns<-GOOGL%>%Ad()%>%dailyReturn(type='log')

#Mean of log stock returns 

CE_mean_log<-mean(CE_log_returns)
FB_mean_log<-mean(FB_log_returns)
TSLA_mean_log<-mean(TSLA_log_returns)
AAPL_mean_log<-mean(AAPL_log_returns)
GOOGL_mean_log<-mean(GOOGL_log_returns)

#round it to 4 decimal places

mean_log<-c(CE_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,GOOGL_mean_log)
mean_log<-round(mean_log,4)

#standard deviation of log stock returns

CE_sd_log<-sd(CE_log_returns)
FB_sd_log<-sd(FB_log_returns)
TSLA_sd_log<-sd(TSLA_log_returns)
AAPL_sd_log<-sd(AAPL_log_returns)
GOOGL_sd_Log<-sd(GOOGL_log_returns)

#round it to 4 decimal places 

sd_log<-c(CE_sd_log,FB_sd_log,TSLA_sd_log,AAPL_sd_log,GOOGL_sd_Log)
sd_log<-round(sd_log,4)

#create data frame
#Data frame contains the 4 companies with each company's average log return and standard deviation.
graphic1<-data.frame(rbind(c("CE",CE_mean_log,CE_sd_log),c("FB",FB_mean_log,FB_sd_log),c("TSLA",TSLA_mean_log,TSLA_sd_log),c("AAPL",AAPL_mean_log,AAPL_sd_log),c("GOOGL",GOOGL_mean_log,GOOGL_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("CE","FB","TSLA","AAPL","GOOGL")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

#Used plotly to create a visualization of each stock's risk v reward. 
#Risk: standard deviation of log returns
#Reward: mean of log returns

xlab<-list(title="Reward", titlefont='f')
ylab<-list(title="Risk", titlefont='f')

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)

#Checking the correlation of 4 stocks: tesla, facebook, google, Celanese

data<-cbind(diff(log(Cl(CE))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)


#random walk: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy


mu<-CE_mean_log
sig<-CE_sd_log
testsim<-rep(NA,1000)

#generate random daily exponent increase rate using CE's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days

price<-rep(NA,252*4)

#most recent price
price[1]<-as.numeric(CE$CE.Adjusted[length(CE$CE.Adjusted),])

#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)

random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Celanese (CE) price simulation for 4 years")+theme_bw()

#monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables

N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(CE$CE.Adjusted[length(CE$CE.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(CE$CE.Adjusted[length(CE$CE.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as_tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 501

final_mat%>%gather("Simulation","Price",2:501)%>%ggplot(aes(x=Day,y=Price,Group=Simulation))+geom_line(alpha=0.2)+labs(title="Celanese Stock (CE): 500 Monte Carlo Simulations for 4 Years")+theme_bw()

#Print Projected Stock Price to Screen based on the following Probabilities
probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)

CE_dist<-CE_log_returns%>%quantile(probs=probs,na.rm=TRUE)
CE_mean<-mean(CE_log_returns,na.rm=TRUE)
CE_sd<-sd(CE_log_returns,na.rm=TRUE)

CE_mean%>%exp() # 1.001271

final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs)

#is it likely? Check the confidence interval

#Convert xts objects to data frames, covert the date from an index to a
#field for inclusion in the .csv file

CE2 <- data.frame(Date = index(CE), coredata(CE) )
AAPL2 <- data.frame(Date = index(AAPL), coredata(AAPL) )
FB2 <- data.frame(Date = index(FB), coredata(FB) )
GOOGL2 <- data.frame(Date = index(GOOGL), coredata(GOOGL) )
TSLA2 <- data.frame(Date = index(TSLA), coredata(TSLA) )

#Combine all stock price info in one data table
stocksgrouped <-join_all(list(AAPL2, CE2, FB2, GOOGL2, TSLA2), by='Date', type='left')

#Write Stocks to .csv file-writes to working directory
write.csv(stocksgrouped, "stocks.csv")


