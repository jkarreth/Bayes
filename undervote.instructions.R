########################################################
## Undervoting: Analyze difference of two proportions ##
########################################################

## Johannes Karreth
## ICPSR Summer Program 2015

## Compare to Simon Jackman's BUGS code: 
## http://jackman.stanford.edu/mcmc/mainFrameWinBugs.php#Undervote

library(R2jags)

setwd("~/R/Bayes/undervote")

und.dat <- list(r=c(26,91,10,57),n=c(6537,44531,1101,9827))

und.params <- c("delta", "good")

# The model (save in your WD as "undervote.mod"; do not run in R)

und.model.jags <- function()  {
    
	for (i in 1:4){
		r[i] ~ dbin(p[i],n[i])
	}
	
	delta[1] <- p[2] - p[1]    ## difference
	good[1] <- step(delta[1])  ## sign of the difference

	delta[2] <- p[4] - p[3]    ## difference
	good[2] <- step(delta[2])  ## sign of the difference	

	## priors
	for(i in 1:4){
		p[i] ~ dunif(0,1)
	}
}

#und.inits <- function(){
#  list("delta"=c(0, 0), "good" =c(0, 0))
#}

# Have JAGS generate initial values

und.fit <- jags(data=und.dat, inits=NULL, und.params, model.file=und.model.jags, n.chains=2, n.iter=1000, n.burnin=100)

print(und.fit)