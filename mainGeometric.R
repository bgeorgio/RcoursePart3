# Geometric distribution
sampleSize <- 100
prob <- 0.2
# success: if randomNumber < prob

randSample <- c()
for (i in 1:sampleSize)  {
  counterTrials <- 0
  repeat {
    randNumber <- runif(n = 1)
    counterTrials <- counterTrials + 1
    #cat(i,randNumber,prob,counterTrials,"\n")  
    if ( randNumber < prob) break 
  }
  randSample[i] <- counterTrials
}

cat("Mean:",1/prob,"  Estimated Mean:",mean(randSample),"\n")





# in function
SimulateFromGeometric <- function(sampleSize,probability){
  randSample <- c()
  for (i in 1:sampleSize)  {
    counterTrials <- 0
    repeat {
      randNumber <- runif(n = 1)
      counterTrials <- counterTrials + 1
      #cat(i,randNumber,probability,counterTrials,"\n")  
      if ( randNumber < probability) break 
    }
    randSample[i] <- counterTrials
  }
  return(randSample)
}

sample1 <- SimulateFromGeometric(sampleSize,prob)

ergodicMean <- function(x){
  # Computes the ergodic mean of vector x
  cumSumX <- cumsum(x)
  return( cumSumX/1:length(cumSumX) )
}

erg1 <- ergodicMean(sample1)
plot(erg1,type = "l")

# burn-in 
numBurnInCases <- 50
selectedCases <- (numBurnInCases+1):length(erg1)
plot(erg1[selectedCases],type = "l",ylim = c(5,15),ylab = "Mean num of trials until success")
plot(selectedCases,erg1[selectedCases],type = "l",ylim = c(5,15))

