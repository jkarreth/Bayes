########################################################################
## WinBUGS example Pumps - conjugate gamma-Poisson hierarchical model ##
########################################################################

## Johannes Karreth
## ICPSR Summer Program 2016

## Data

pumps.data <- list(t = c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5),
	     x = c(5, 1, 5, 14, 3, 19, 1, 1, 4, 22), N = 10)
					
t <- pumps.data$t
x <- pumps.data$x
N <- pumps.data$N

pumps.data <- list("t", "x", "N")

## Model

## Save the following model separately in "pumps.model.jags" in your WD. Don't type it into R.

pumps.mod <- function()  {
		for (i in 1:N) {
			theta[i] ~ dgamma(alpha, beta)
			lambda[i] <- theta[i] * t[i]	## t is an independent variable
			x[i] ~ dpois(lambda[i])			## x is a stochastic node, lambda is a deterministic node.
		}
		alpha ~ dexp(1)						## alpha and beta are the hyperpriors (and parental nodes)
		beta ~ dgamma(0.1, 1.0)
}

## Set WD:
setwd("~/R/Bayes/pumps")

## Model parameters

pumps.params <- c("theta")

## Run JAGS

pumpsfit <- jags(data=pumps.data, inits=NULL, pumps.params, n.chains=2, n.iter=5000, n.burnin=1000, model.file=pumps.mod)
				
## Results & diagnostics

print(pumpsfit)

plot(pumpsfit)

traceplot(pumpsfit)
## if we want to keep these:
pdf("pumps.traceplot.pdf") 	## open PDF device
traceplot(pumpsfit)			## write the plot
dev.off()					## finish the plotting command

## Generate MCMC object for analysis
pumpsfit.mcmc <- as.mcmc(pumpsfit)

xyplot(pumpsfit.mcmc)
## large plot (many variables): use different output technique - print as PDF
pdf("pumps.xyplot.pdf") # open pdf device
xyplot(pumpsfit.mcmc, layout = c(5,4,3), aspect="fill")	## provide commands for the layout to fit all plots into one graph
dev.off() # close PDF device; graph is in the WD

densityplot(pumpsfit.mcmc) 
## large plot (many variables): use different output technique
pdf("pumps.densityplot.pdf") # open pdf device
densityplot(pumpsfit.mcmc, layout = c(5,4,3), aspect="fill")
dev.off() # close PDF device; graph is in the WD

## More diagnostics:

gelman.plot(pumpsfit.mcmc)
geweke.diag(pumpsfit.mcmc)
geweke.plot(pumpsfit.mcmc)
raftery.diag(pumpsfit.mcmc)
heidel.diag(pumpsfit.mcmc)


## Update models:

pumpsfit.upd <- update(pumpsfit, n.iter=1000)
pumpsfit.upd <- autojags(pumpsfit)

## Run JAGS such that it produces coda files for analysis with boa or coda packages:

pumpsfit <- jags2(data=pumps.data, inits=pumps.inits, pumps.params, n.chains=2, n.iter=5000, n.burnin=1000, model.file="pumps.model.jags", clearWD=FALSE)


