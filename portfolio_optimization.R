#install.packages("quantmod")
#install.packages("GA")
#install.packages("parallel")
#install.packages("doParallel")

library(quantmod)
library(GA)
library(parallel)
library(doParallel)

#Initial portfolio
#myStocks <- c("AKER", "BLDP", "GOOG", "AAPL")
#Portfolio with tech companies with positive returns
myStocks <- c("VNET","DDD","JOBS","GOOG","EGHT","AAN","ACIA","ACIW","ACMR","ADBE","AMD")
#Number of assets in a portfolio
numStocks <- length(myStocks)

#Get the weekly returns for each asset
weeklyRet <- lapply(myStocks, function(sym) weeklyReturn(na.omit(getSymbols(sym, from="2018-01-01", to="2019-01-01", auto.assign=FALSE))))
#Merge the assets into 1 matrix
weeklyRet <- do.call(merge, weeklyRet)

#Get the mean of the weekly return
meanWeeklyRet <- colMeans(weeklyRet)

#Covariance matrix
covMatWeeklyRet <- cov(weeklyRet)

#Set the lower and the upper limit of the generated solutions as well as the chromosome size
lower <- c(rep(0,numStocks))
upper <- c(rep(1,numStocks))

#Scale the chromosomes down to 1
scaleWeights <- function(x){
  return (x/sum(x))
}

#Calculate the return of a chromosome
maxReturn <- function(x){
  return (sum(meanWeeklyRet*x))
}

#Calculate the risk of a chromosome
minRisk <- function(x){
  return (sum(t(covMatWeeklyRet*x)*x))
}

gen_po <- function(w){
  multiObj <- function(x){
    #Scale the solution down to 1
    x <- scaleWeights(x)
    #Since we need to maximize return and minimize risk we take the positive of 
    #maxReturn and the negative of minRisk
    fitness <- (w*maxReturn(x))+((1-w)*(-minRisk(x)))
    return(fitness)  
  }

  #Genetic algorithm
  ga_po <- ga(type="real-valued",
           fitness = multiObj, maxiter = 200,popSize = 200,
           lower = lower,
           upper = upper, elitism=2,
           run=50, parallel=TRUE,
           monitor=TRUE,seed = 1)
  out <- summary(ga_po)
  return(ga_po@solution)
}

#Run the genetic algorithm 17 times with different risk/return ratios starting from 0.2 to 1 in increments of 0.05
solutions <- mapply(gen_po,seq(from = 0.2, to = 1,0.05))
#Create a matrix of the list of solutions
solutions <- split(solutions, rep(1:ncol(solutions), each = nrow(solutions)))
#Solutions matrix
solutions

#scaled is the list of best solutions scaled down to 1
scaled <- lapply(solutions,scaleWeights)

#returns holds a vector of the return values for the best solutions
returns <- sapply(scaled, maxReturn)

#risks holds a vector of the risk values for the best solutions
risks <- sapply(scaled[], minRisk)

#Plots the returns against the risk to get the paretto plot
plot(returns,risks)