compute.returns<-function(prices.subset.dfr){
  # Calculates the asset returns for one stock, given all close prices.
  prices<-prices.subset.dfr[,"close.price"]
  n <- length(prices)
  ret<-data.frame(dates=prices.subset.dfr[1:n-1,"date"], r=rep(0,n-1))
  for(i in 1:n-1){
    ret[i,"r"] <- (prices[i+1] - prices[i])/prices[i] * 100
  }
  
  return(cbind(ret,months = substr(ret[,"dates"], 1, 7)))
}

compute.volatility<-function(returns){
  # Calculates volatility by month, given ALL asset returns
  
  avg.return.by.month<-aggregate(x = returns[,"r"],
             by = list(month = substr(returns[,"dates"], 1, 7)), 
             FUN = mean)
  
  result<-data.frame(month=avg.return.by.month$month, volatility=0)
  
  for(month in avg.return.by.month$month){
    Ri<-returns[returns$months==month,"r"]
    v<-((Ri - avg.return.by.month[avg.return.by.month$month==month,"x"])^2)
    
    m<-length(Ri)
    
    result[result$month==month,2]<-sqrt(252/(m-1) * sum(v))
  }
  
  return(result)
}

transform.data<-function(indt){  
  prices.industry.dfr<-subset(prices.dfr, subset = prices.dfr$industry==indt)
  
  prices.plot.ts<-NULL
  volatility.plot.ts<-NULL
  
  for(s in unique(prices.industry.dfr$stock)){
    
    print(paste0("Computing stock: ",s))
    
    mean.price.by.month<-aggregate(
      x = prices.industry.dfr[prices.industry.dfr$stock==s
                        & order(prices.industry.dfr$date),"close.price"],
      by = list(month = substr(prices.industry.dfr[prices.industry.dfr$stock==s
                        & order(prices.industry.dfr$date),"date"], 1, 7)), 
      FUN = mean)
    
    price.ts<-ts(mean.price.by.month$x, start=c(2010,1), end=c(2016,12), frequency = 12)
    
    prices.plot.ts<-ts.intersect(prices.plot.ts, price.ts)
    
    returns<-compute.returns(
      prices.industry.dfr[prices.industry.dfr$stock==s
                  & order(prices.industry.dfr$date),c(1,3)]) # Computes the returns and saves in data frame with date
    
    volatility.by.month<-compute.volatility(returns)
    
    volatility.ts<-ts(volatility.by.month$volatility, start=c(2010,1), end=c(2016,12), frequency = 12)
    
    volatility.plot.ts<-ts.intersect(volatility.plot.ts, volatility.ts)
    
  }
  
  return(list(prices.plot.ts, volatility.plot.ts))
  
}
