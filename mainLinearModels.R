### 
library(car)
data(Salaries)
lm1 <- lm(salary~rank,data = Salaries)
sumlm1 <- summary(lm1)
sumlm1




# Create 500 samples of Salaries of size 50
# and apply regression analysis
# save rankProf coefficients

numSamples <- 500
sampleSize <- 50
coef <- c()
for (curSampleId in 1:5000){
  selectedRows <- sample(1:nrow(Salaries),size = 50,replace = F)
  curSample <- Salaries[selectedRows,]
  curLM <- lm(salary~rank,data = curSample)
  summaryCurLM <- summary(curLM)
  coef[curSampleId] <- summaryCurLM$coefficients[3]  
}
summary(coef)


numSamples <- 5000
sampleSize <- 5000
coef <- c()
for (curSampleId in 1:numSamples){
  selectedRows <- sample(1:nrow(Salaries),size = sampleSize,replace = T)
  curSample <- Salaries[selectedRows,]
  curLM <- lm(salary~rank,data = curSample)
  summaryCurLM <- summary(curLM)
  coef[curSampleId] <- summaryCurLM$coefficients[3]  
}
summary(coef)
hist(coef)


lmfun <- function(numSamples,sampleSize){
  #numSamples <- 5000
  #sampleSize <- 5000
  coef <- c()
  for (curSampleId in 1:numSamples){
    selectedRows <- sample(1:nrow(Salaries),size = sampleSize,replace = T)
    curSample <- Salaries[selectedRows,]
    curLM <- lm(salary~rank,data = curSample)
    summaryCurLM <- summary(curLM)
    coef[curSampleId] <- summaryCurLM$coefficients[3]  
  }
  #summary(coef)
  #hist(coef)
  return(coef)
}


system.time(coef1 <- lmfun(numSamples = 5000, sampleSize = 5000) )


lmfun2 <- function(){
  selectedRows <- sample(1:nrow(Salaries),size = sampleSize,replace = T)
  curSample <- Salaries[selectedRows,]
  curLM <- lm(salary~rank,data = curSample)
  summaryCurLM <- summary(curLM)
  return(summaryCurLM$coefficients[3]  )
}

library(doParallel)
numCores <- 2
cl <- makeCluster(numCores)
registerDoParallel(cl)

t1<-system.time(coefList <- foreach(curSampleId=1:numSamples) %do% { lmfun2() })
t2<-system.time(coefList <- foreach(curSampleId=1:numSamples) %dopar% { lmfun2() })
cat("NumSamples:",numSamples,"  SampleSize:",sampleSize," time(1-core):",t1[3]," time(2-core):",t2[3],"\n")
stopCluster(cl)

#
numSamples <- 50
sampleSize <- 500000
t1<-system.time(coefList <- foreach(curSampleId=1:numSamples) %do% { lmfun2() })
numCores <- 2
cl <- makeCluster(numCores)
registerDoParallel(cl)
t2<-system.time(coefList <- foreach(curSampleId=1:numSamples) %dopar% { lmfun2() })
stopCluster(cl)

numCores <- 3
cl <- makeCluster(numCores)
registerDoParallel(cl)
t3<-system.time(coefList <- foreach(curSampleId=1:numSamples) %dopar% { lmfun2() })
stopCluster(cl)

cat("NumSamples:",numSamples,"  SampleSize:",sampleSize," time(1-core):",t1[3],
    " time(2-core):",t2[3]," time(3-core):",t3[3],"\n")



lmfun3 <- function(curID){
  selectedRows <- sample(1:nrow(Salaries),size = sampleSize,replace = T)
  curSample <- Salaries[selectedRows,]
  curLM <- lm(salary~yrs.service,data = curSample)
  summaryCurLM <- summary(curLM)
  png(filename = paste0("LMimages/SalariesPlot_",curID,".png"))
  plot(curSample$yrs.service,curSample$salary,xlab = "Years in service",ylab = "Salary")
  title(paste0("Linear Model ",curID))
  abline(curLM)
  dev.off() 
  #cat("Created plot",curID)
  cat("\rCreated plot",curID)
  return(summaryCurLM$coefficients[2]  )
}

dir.create("LMimages")
k1 <- sapply(1:50,lmfun3)
