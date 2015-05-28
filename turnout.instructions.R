###############################################################################
## R2jags example using Simon Jackman's Turnout example - logit/probit model ##
###############################################################################

## Johannes Karreth
## ICPSR Summer Program 2015

## Compare to Simon Jackman's BUGS code: 
## http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Turnout


## Required libraries
library(foreign)
library(R2jags)

setwd("~/R/Bayes/turnout")

## Read the data
## This time, read in the data from an external file. Copy the data from Simon Jackman's WinBUGS code into a text file, turnout.bugs.txt, in your WD. Then,

bugs2jags("turnout.bugs.txt", "turnout.jags.txt")

## Alternatively, read in the data from the web: 
source("http://www.jkarreth.net/files/turnout.dat.jags")

## Then create a list of these elements:

turnout.data=list("y", "educ", "age", "south", "govelec", "closing", "N")


## Model:
## Might want to run the model only on the first 300 or so observations to save time while playing around:

turnout.model.jags <- function()  {
   for (i in 1:N){   ## or: for (i in 1:300)              
     y[i] ~ dbern(p[i])  
     logit(p[i]) <- mu[i]                   
     mu[i] <- beta1              
              + beta2 *educ[i]
              + beta3 *age[i]
              + beta4 *south[i]
              + beta5 *govelec[i]
              + beta6 *closing[i]
         
         
     llh[i] <- y[i]*log(p[i]) + (1-y[i])*log(1-p[i])   
   }
 
   sumllh <- sum(llh[])     
  
   
  
   beta1 ~ dnorm(0.0,1.0E-6)
   beta2 ~ dnorm(0.0,1.0E-6)
   beta3 ~ dnorm(0.0,1.0E-6)
   beta4 ~ dnorm(0.0,1.0E-6)
   beta5 ~ dnorm(0.0,1.0E-6) 
   beta6 ~ dnorm(0.0,1.0E-6)
     
 }

					
## Name the JAGS parameters

turnout.params <- c("beta1", "beta2", "beta3", "beta4", "beta5", "beta6")

## Define the starting values for JAGS

#  turnout.inits <- function(){
#  list("beta1"=c(0), "beta2"=c(0), "beta3"=c(0), "beta4"=c(0), "beta5"=c(0), "beta6"=c(0), "beta7"=c(0), "beta8"=c(0), "beta9"=c(0), "beta10"=c(0))
#  }

#  turnout.inits <- function(){
#  list(beta=c(-0.3392423378,  0.0741911953,  0.0012163747,  0.0230970246, -0.0001679677, -0.0333484965,  0.0204799754, -0.0068319918, 0.0017752978, -0.0001432201))
#  }

inits1 <- list(beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, beta6 = 0) 
inits2 <- list(beta1 = -3.49, beta2 = 0.55, beta3 = 0.03, beta4 = -0.13, beta5 = 0.11, beta6 = -0.009)
turnout.inits <- list(inits1, inits2)

## Fit the model in JAGS

turnout.fit <- jags(data=turnout.data, inits=NULL, turnout.params, n.chains=2, n.iter=500, n.burnin=100, model.file=turnout.model.jags)