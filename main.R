##
## This script loads the stock price data from CSV files, then calls the function
## 'transform.data' to obtain the time series for monthly average prices and volatility,
## and finally plots the results in well segregated labeled figures.
##

dirdata="./data/"

temp.prices.dfr<-data.frame(read.csv(paste0(dirdata, "Airlines.csv")), industry="Airlines")
temp.prices.dfr<-rbind(temp.prices.dfr, data.frame(read.csv(paste0(dirdata, "Restaurants.csv")), industry="Restaurants"))
temp.prices.dfr<-rbind(temp.prices.dfr, data.frame(read.csv(paste0(dirdata, "Cable.csv")), industry="Cable"))

prices.dfr<-data.frame(date=as.Date(temp.prices.dfr$date), stock=temp.prices.dfr$symbol, close.price=temp.prices.dfr$close, industry=temp.prices.dfr$industry)
attach(prices.dfr)


datalist.ready<-transform.data("Restaurants")

windows()
ts.plot(datalist.ready[[1]],gpars = list(ylab="Mean price ($)", xlab="Time",
          main="Mean price by month for restaurants", col=c("black","blue","red")))
legend(2010,700, legend=c("MCD", "SBUX", "CMG"), col=c("black","blue","red"), lty=1)

ts.plot(datalist.ready[[2]],gpars = list(ylab="Volatility", xlab="Time", col=c("black","blue","red")),
          main="Volatility by month for restaurants")
legend(2015,80, legend=c("MCD", "SBUX", "CMG"), col=c("black","blue","red"), lty=1)

windows()
Prices<-datalist.ready[[1]][,1]
Volatility<-datalist.ready[[2]][,1]
acf(ts.intersect(Prices, Volatility)) # MCD
mtext("McDonald's", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,2]
Volatility<-datalist.ready[[2]][,2]
acf(ts.intersect(Prices, Volatility)) # SBUX
mtext("Starbucks", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,3]
Volatility<-datalist.ready[[2]][,3]
acf(ts.intersect(Prices, Volatility)) # CMG
mtext("Chipotle", line=3, font=3, cex=1.2)



datalist.ready<-transform.data("Airlines")

windows()
ts.plot(datalist.ready[[1]],gpars = list(ylab="Mean price ($)", xlab="Time", col=c("black","blue","red")),
          main="Mean price by month for airlines")
legend(2015.5,15, legend=c("AAL", "DAL", "UAL"), col=c("black","blue","red"), lty=1)

windows()
ts.plot(datalist.ready[[2]],gpars = list(ylab="Volatility", xlab="Time", col=c("black","blue","red")),
          main="Volatility by month for airlines")
legend(2015.5,90, legend=c("AAL", "DAL", "UAL"), col=c("black","blue","red"), lty=1)

windows()
Prices<-datalist.ready[[1]][,1]
Volatility<-datalist.ready[[2]][,1]
acf(ts.intersect(Prices, Volatility)) # AAL
mtext("American Airlines", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,2]
Volatility<-datalist.ready[[2]][,2]
acf(ts.intersect(Prices, Volatility)) # DAL
mtext("Delta Airlines", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,3]
Volatility<-datalist.ready[[2]][,3]
acf(ts.intersect(Prices, Volatility)) # UAL
mtext("United Airlines", line=3, font=3, cex=1.2)


datalist.ready<-transform.data("Cable")

windows()
ts.plot(datalist.ready[[1]],gpars = list(ylab="Mean price ($)", xlab="Time", col=c("black","blue","red")),
          main="Mean price by month for cable/satellite companies")
legend(2015.4,35, legend=c("CMCSA", "DIS", "TWX"), col=c("black","blue","red"), lty=1)

windows()
ts.plot(datalist.ready[[2]],gpars = list(ylab="Volatility", xlab="Time", col=c("black","blue","red")),
          main="Volatility by month for cable/satellite companies")
legend(2015.4,60, legend=c("CMCSA", "DIS", "TWX"), col=c("black","blue","red"), lty=1)


windows()
Prices<-datalist.ready[[1]][,1]
Volatility<-datalist.ready[[2]][,1]
acf(ts.intersect(Prices, Volatility)) # Comcast
mtext("Comcast", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,2]
Volatility<-datalist.ready[[2]][,2]
acf(ts.intersect(Prices, Volatility)) # Disney
mtext("Disney", line=3, font=3, cex=1.2)

windows()
Prices<-datalist.ready[[1]][,3]
Volatility<-datalist.ready[[2]][,3]
acf(ts.intersect(Prices, Volatility)) # Time Warner
mtext("Time Warner", line=3, font=3, cex=1.2)

