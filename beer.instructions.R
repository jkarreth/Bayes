#################################################################
## R2jags example using the Beer example - ordered logit model ##
#################################################################

## Johannes Karreth
## ICPSR Summer Program 2016

## Required libraries
library(foreign)
library(R2jags)

setwd("~/R/Bayes/beer")

## Read the data

beer.data <- list(price = c(7.19, 3.15, 3.35, 2.59, 4.59, 4.39, 2.99, 2.49, 3.65, 2.59, 2.89, 2.99, 2.29, 4.75, 1.69, 2.55, 4.22, 2.63, 2.73, 1.79, 2.65, 2.39, 2.49, 4.55, 2.65, 1.79, 1.59, 2.79, 2.75, 2.59, 2.59, 2.15, 4.59, 2.59, 2.29), calories = c( 154, 147, 154, 144, 152, 170, 162, 149, 149, 151, 157, 135, 152, 149, 145, 99, 145, 113, 102, 147, 140, 175, 149, 150, 153, 144, 145, 97, 72, 155, 136, 144, 144, 144, 68), sodium = c( 17, 17, 17, 15, 11, 7, 10, 17, 7, 19, 15, 11, 8, 6, 23, 10, 14, 8, 15, 7, 18, 24, 27, 19, 27, 13, 18, 7, 6, 13, 19, 8, 21, 24, 15), alcohol = c( 4.7, 5, 5.1, 4.7, 5, 5.2, 5, 4.7, 4.7, 4.9, 4.9, 4.2, 4.9, 5, 4.6, 4.3, 4.5, 3.7, 4.1, 4.7, 4.6, 5.5, 4.7, 4.7, 4.6, 4.6, 4.5, 4.2, 2.9, 5, 4.4, 4.7, 4.7, 4.9, 2.3), quality = c( 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), N=35 )

price <- beer.data$price
calories <- beer.data$calories
sodium <- beer.data$sodium
alcohol <- beer.data$alcohol
quality <- beer.data$quality
N <- length(beer.data$price)

## Model: Compare the BUGS language to the JAGS version

## BUGS model

#  model{  
#  for(i in 1:N){
#  for(j in 1:2){
#  logit(gamma[i,j]) <- theta[j] - mu[i]}
#  p[i,1]<- gamma[i,1]  				## Probability that each beer is in cat1, or the probability that tau > xbeta. Compare: Pr(y_i=1) = logit(tau_1 - X_i \beta) - 0
#  p[i,2] <- gamma[i,2] - gamma[i,1]	## Probability that each beer is in cat2. Compare: Pr(y_i=2) = logit(tau_2 - X_i \beta) - logit(tau_1 - X_i \beta)
#  p[i,3] <- 1-gamma[i,2]				## Probability that each beer is in cat3. Compare: Pr(y_i=1) = 1 - logit(tau_2 - X_i \beta)
#  mu[i] <-  b[1]*price[i] + b[2]*sodium[i] + b[3]*alcohol[i]+b[4]*calories[i]
#  quality[i] ~ dcat(p[i,1:3])	## Categorical distribution (check WinBUGS help for more info)
#  
#  
#  
#  for(j in 1:3){
#  pred[i,j] <- equals(p[i,j], ranked(p[i,1:3],3))	## pred[i,j] is always 0 or 1
#  }
#  predcat[i] <- pred[i,1] + 2*pred[i,2] + 3*pred[i,3]
#  }
#  for(m in 1:4){
#  b[m] ~ dnorm(0, .0001)
#  }
#  theta[1] ~ dnorm(0,.1)I(0, theta[2])	## the capital I restricts the values of theta[1] to be between 0 and theta[2]
#  theta[2] ~ dnorm(0,.1)I(theta[1], )
#  
#  }
#  
#

## JAGS model !!
## Note the differences in handling the cutpoints AND the pred. category code. 
## For more info: jags.tutorial.pdf on http://www.jkarreth.net/Bayes2013.html

beer.model.jags <- function()  {
  
 	for (i in 1:N){
 		for (j in 1:2){
 			logit(gamma[i,j]) <- theta1[j] - mu[i]
 			}
 		quality[i] ~ dcat(p[i,1:3])
 		p[i,1]<- gamma[i,1]
 		p[i,2] <- gamma[i,2] - gamma[i,1]
 		p[i,3] <- 1-gamma[i,2]
 		mu[i] <-  b1*price[i] + b2*sodium[i] + b3*alcohol[i]+b4*calories[i]
 		
 		pred[i,1] <- equals(p[i,1], max(p[i,1], p[i,2], p[i,3])) # "1 if p[i,1] = max(p[i,1], p[i,2], p[i,3]), 0 otherwise"
 		pred[i,2] <- equals(p[i,2], max(p[i,1], p[i,2], p[i,3]))
 		pred[i,3] <- equals(p[i,3], max(p[i,1], p[i,2], p[i,3]))
 				
 		predcat[i] <- pred[i,1] + 2*pred[i,2] + 3*pred[i,3]
 				
 		}
 		for (i in 1:2) {
 			theta[i] ~ dnorm(0, 1.0E-3)
 			}
 		theta1[1:2] <- sort(theta)
 		b1 ~ dnorm(0, .1)
 		b2 ~ dnorm(0, .1)
 		b3 ~ dnorm(0, .1)
		b4 ~ dnorm(0, .1)
		
		}


## Now define the vectors of the data matrix for JAGS:

## Read in the Beer data for JAGS

beer.data  <- list("price", "calories", "sodium", "alcohol", "quality", "N")

## Name the JAGS parameters to be monitored

beer.params <- c("b1", "b2", "b3", "b4", "predcat")

## Define the starting values for JAGS
## Note that we give starting values to theta, not theta1

beer.inits <- function(){
  list("b1" = c(0.56), 
       "b2" = c(-0.05), 
       "b3" = c(0.95), 
       "b4" = c(0.03), 
       "theta" = c(7, 10))
}

## Fit the model in JAGS, having previously copied the BUGS model into your working directory as "beer.mod"

beerfit <- jags(data=beer.data, inits=beer.inits, beer.params, n.chains=2, n.iter=2000, n.burnin=500, model.file=beer.model.jags)

## Update your model if necessary - e.g. if there is no/little convergence:

beerfit.upd <- update(beerfit, n.iter=1000)
beerfit.upd <- autojags(beerfit)  # this function will auto-update until convergence - check R2jags documentation how this is defined

## Obtain info on predicted categories, i.e.
## which category does your model predict each observation to be in?

# Monitor the "predcat" node from the code above
# Then inspect the MEDIAN (not mean) of predcat
beer.mcmc <- as.mcmc(beerfit)
summary(beer.mcmc)

# For a table of the distribution of predicted categories:
chains <- do.call(rbind, beer.mcmc)
pc <- chains[ , grep("predcat", colnames(chains))]
tab <- apply(pc, 2, function(x)table(factor(x, levels = 1:3)))
tab <- t(tab)
library(car)
prop.table(tab, 1)