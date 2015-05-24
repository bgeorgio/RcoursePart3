n <- 1000000
x <- runif(n)
y <- runif(n)
z <- vector(length = n)
system.time(for(i in 1:n) z[i]<-x[i]+y[i])
system.time(z<-x+y)

#
# Binomial experiment
# Estimate probability of 4 or more successes in 5 coin tosses

#Monte Carlo probability estimation
numTrials <- 1000000
counterSuccess <- 0
for (i in 1:numTrials){
  randomVariateBinom <- rbinom(n = 1,size = 5,prob = 0.5)
  if (randomVariateBinom>=4) counterSuccess <- counterSuccess + 1
}
cat("P =", counterSuccess/numTrials,"\n")



system.time({
  numTrials <- 1000000
  counterSuccess <- 0
  for (i in 1:numTrials){
    randomVariateBinom <- rbinom(n = 1,size = 5,prob = 0.5)
    if (randomVariateBinom>=4) counterSuccess <- counterSuccess + 1
  }
  cat("P =", counterSuccess/numTrials,"\n")
})



x <- rbinom(n = numTrials, size = 5,prob = 0.5)
mean(x>=4)
system.time({
  x<- rbinom(n = numTrials, size = 5,prob = 0.5)
  mean(x>=4)
})



#Convert to function
binom1 <- function(numberTrials){
  #numTrials <- 1000
  counterSuccess <- 0
  for (i in 1:numberTrials){
    randomVariateBinom <- rbinom(n = 1,size = 5,prob = 0.5)
    if (randomVariateBinom>=4) counterSuccess <- counterSuccess + 1
  }
  return(counterSuccess/numberTrials)
}
cat("P =", binom1(numTrials),"\n")
numTrials <- 10000000
system.time(p1<-binom1(numTrials))




binom2 <- function(numberTrials){
  x<- rbinom(n = numberTrials, size = 5,prob = 0.5)
  return(mean(x>=4))
}
system.time(p2<-binom2(numTrials))

