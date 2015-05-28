#################################################################
## R2jags example using the WVS example - random effects model ##
#################################################################

## Johannes Karreth
## jkarreth@albany.edu


## Required libraries
library(foreign)
library(R2jags)

## Read the data

source("http://www.jkarreth.net/files/wvs.jags.txt")

wvs.dat <- list("N", "nation1", "trust1", "lifesat", "lr", "relig1", "proud1", "sex1", "age", "educ")

## JAGS model: very simple, but can be extended as you see fit


wvs.mod <- function(){
 for(i in 1:N)
  {
   
    
     lifesat[i]~dnorm(mu[i],sigma2inv[nation1[i]])		## DV: life satisfaction; distributed normal. 
                                                        ## BUT: variance is indexed by NATION, mean is indexed by individual. 
                                                        ## Each country gets its own within-group variance..
     mu[i]<-alpha[nation1[i]] +beta[nation1[i]]*proud1[i]    ## mu gets its own intercept by nation (country-specific intercepts)
														## If we want to get different slopes, do this: mu[i]<-alpha[nation1[i]] +beta[nation1[i]]*proud1[i]
    }
for(i in 1:4){
  alpha[i]~dnorm(malpha,tau2inv)	## Note: nothing indexed here... IF this were [0;1], it'd be a fixed effects model...
  beta[i]~dnorm(mbeta,tau3inv)
  
}			## Closing the forloop for the four countries
mbeta~dnorm(0,1.0E-4)		## Mean for beta; note that scientific writing is permitted in model code. 
malpha~dnorm(0,1.0E-4)		## Mbeta and Malpha are stochastic parent nodes. If those were to be modeled as a function of country-level covariates, we'd have #  additional mean functions for these!
tau2inv~dgamma(.1,.1)		## Between-group variance
tau2<-1/sqrt(tau2inv)
tau3inv~dgamma(.1,.1)
tau3<-1/sqrt(tau2inv)


for(i in 1:4){
sigma2inv[i]~dgamma(.1,.1)	## Prior for within-country variance
sigma2[i]<-1/sqrt(sigma2inv[i])
}
}

## Set wd

setwd("~/R/Bayes/wvs")

## Name the JAGS parameters to be monitored

wvs.params <- c("alpha", "beta")

## Define the starting values for JAGS

wvs.inits <- function(){
	list("alpha"=c(0, 0, 0, 0), "beta"=c(0, 0, 0, 0))
}

## Fit the model in JAGS

wvsfit <- jags(data=wvs.dat, inits=wvs.inits, wvs.params, n.chains=2, n.iter=500, n.burnin=100, model.file=wvs.mod)