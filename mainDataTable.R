# Create simulated data
numStores <- 1000
numWeeks  <- 52
numProds  <- 100

salesData <- data.frame(store=rep(1:numStores,each = numWeeks*numProds),
                        week=rep(1:numWeeks,times = numStores,each = numProds),
                        prod=rep(1:numProds,times = numStores*numWeeks),
                        revenue=rnorm(n = numStores*numWeeks*numProds,mean = 200,sd = 30))

# Total Sales per product
system.time(salesPerProd <- by(data = salesData$revenue,INDICES = salesData$prod,FUN = sum))

library(data.table)
dtsalesData <- data.table(salesData)
system.time(dtsalesPerProd <- dtsalesData[,list(revenue=sum(revenue)),by=prod])

# Mean sales per product-week
system.time(salesPerProdWeek <- by(data = salesData$revenue,INDICES = paste0(salesData$prod,"_",salesData$week), FUN = mean))

system.time(dtsalesPerProdWeek <- dtsalesData[,list(revenue=mean(revenue)),by=list(prod,week)])


#Market share
totalRevenue <- sum(dtsalesData$revenue)
system.time(k1<-as.vector( by(data = salesData$revenue,INDICES = salesData$prod,FUN = sum)/totalRevenue ) )
system.time(dtsalesMarketShare <- dtsalesData[,list(Mshare=sum(revenue)/totalRevenue),by=list(prod)])

