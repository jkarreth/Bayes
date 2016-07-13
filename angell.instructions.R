############################################################
## R2jags example using Angell data - Simple linear model ##
############################################################

## Johannes Karreth
## ICPSR Summer Program 2016

## Required packages
library(foreign)
library(R2jags)
## calls:
# library(coda)
# library(lattice)
# library(R2WinBUGS)
# library(rjags)

#### R2jags help
?jags

## Read the data in car library
library(car)
data(Angell)
angell.1 <- Angell[, -4] ## take off the fourth column (remember, the order is (row, column))

## What do the data look like?
head(angell.1)

## Save the model as "angell.model.jags" in your working directory, using a text editor. Make sure you specify the full name below (check if your text editor gives the file a .txt extension). Do not run this model from within R. You can set your working directory in R's preferences, or via:

setwd("/Users/johanneskarreth/R/Bayes/angell")

#  model {
#  for(i in 1:N){
#  moral[i]~dnorm(mu[i], tau)
#  mu[i]<-alpha + beta1*hetero[i] + beta2*mobility[i]
#  }
#  
#  alpha~dnorm(0, .01)
#  beta1~dunif(-100,100)
#  beta2~dunif(-100,100)
#  tau~dgamma(.01,.01)
#  }

## Instead, you can also write the model directly in your R script. For use with R2jags, do this:

angell.model.jags <- function()  {

 for(i in 1:N){
 moral[i]~dnorm(mu[i], tau)
 mu[i]<-alpha + beta1*hetero[i] + beta2*mobility[i]
 }
 
 alpha~dnorm(0, .01)
 beta1~dunif(-100,100)
 beta2~dunif(-100,100)
 tau~dgamma(.01,.01)

}

## Now define the vectors of the data matrix for JAGS:

moral <- angell.1$moral
hetero <- angell.1$hetero
mobility <- angell.1$mobility
N <- length(angell.1$moral)

## Read in the Angell data for JAGS

angell.data  <- list("moral", "hetero", "mobility", "N")
angell.data  <- list(moral = Angell$moral, hetero = Angell$hetero, mobility = Angell$mobility, N = length(Angell$moral))

## Name the parameters of the JAGS model you will monitor

angell.params <- c("alpha", "beta1", "beta2")

## Define the starting values for JAGS

angell.inits <- function(){
  list("alpha"=c(20), "beta1"=c(-0.1), "beta2" =c(-.02))
}

## Alternatively, provide different starting values for each chain:
inits1 <- list("alpha"=0, "beta1"=0, "beta2"=0) 
inits2 <- list("alpha"=1, "beta1"=1, "beta2"=1)
angell.inits <- list(inits1, inits2)

## Fit the model in JAGS, having previously copied the BUGS model into your working directory as "angell.model.jags"

angellfit <- jags(data=angell.data, inits=angell.inits, angell.params, n.chains=2, n.iter=9000, n.burnin=1000, model.file=angell.model.jags)

## If we wanted JAGS to generate initial values:
angellfit <- jags(data=angell.data, inits=NULL, angell.params, n.chains=2, n.iter=9000, n.burnin=1000, model.file= angell.model.jags)

## Note: If you had saved the model file in your working directory, put the file name in quotation marks:
# angellfit <- jags(data=angell.data, inits=NULL, angell.params, n.chains=2, n.iter=9000, n.burnin=1000, model.file="angell.model.jags")

## Update your model if necessary - e.g. if there is no/little convergence:

angellfit.upd <- update(angellfit, n.iter=1000)
angellfit.upd <- autojags(angellfit)  # this function will auto-update until convergence - check R2jags documentation how this is defined

## Model summary

print(angellfit)

## For a function to create a barebones summary table, use
## https://github.com/jkarreth/JKmisc/blob/master/mcmctab.R
## install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(angellfit)

plot(angellfit)

## Re-specify the model to include a distribution for R-squared

angell.model.jags <- function()  {

 for(i in 1:N){
 moral[i]~dnorm(mu[i], tau)
 mu[i]<-alpha + beta1*hetero[i] + beta2*mobility[i]
 y_ybar[i] <- pow(moral[i] - mean(moral[]), 2)
 y_yhat[i] <- pow(moral[i] - mu[i], 2)
 }
 
 r2 <- (sum(y_ybar[]) - sum(y_yhat[])) / sum(y_ybar[])

 alpha~dnorm(0, .01)
 beta1~dunif(-100,100)
 beta2~dunif(-100,100)
 tau~dgamma(.01,.01)

}

angell.params <- c("alpha", "beta1", "beta2", "r2")
angellfit <- jags(data=angell.data, inits=angell.inits, angell.params, n.chains=2, n.iter=9000, n.burnin=1000, model.file= angell.model.jags)

## Diagnostics

traceplot(angellfit)

## if you want to print & save the plot, you can use the following set of commands:
pdf("angell.trace.pdf")    # defines that the plot will be saved as a PDF file with the name "angell.trace.pdf" in your working directory
traceplot(angellfit)	# creates the plot in the background (you will not see it)
dev.off()				# finishes the printing process and creates the PDF file of the plot. If successful, R will display the message "null device 1" 

## Generate MCMC object for analysis
angellfit.mcmc <- as.mcmc(angellfit)

summary(angellfit.mcmc)

xyplot(angellfit.mcmc)
## maybe better display (you can use other Lattice options here as well):
xyplot(angellfit.mcmc, layout=c(2,2), aspect="fill")

densityplot(angellfit.mcmc)
## maybe better:
densityplot(angellfit.mcmc, layout=c(2,2), aspect="fill") 

## and more plots... all using the MCMC object and the CODA package:

## Trace- and density in one plot, print directly to your working directory:
pdf("angellfit.mcmc.plot.pdf")
plot(angellfit.mcmc)
dev.off()

## Autocorrelation plot, print directly to your working directory:
pdf("angellfit.mcmc.autocorr.pdf")
autocorr.plot(angellfit.mcmc)
dev.off()

## Other diagnostics using CODA:

gelman.plot(angellfit.mcmc)
geweke.diag(angellfit.mcmc)
geweke.plot(angellfit.mcmc)
raftery.diag(angellfit.mcmc)
heidel.diag(angellfit.mcmc)

## Another option for diagnostics and results: the ggmcmc package
library(ggmcmc)

## All plots into one PDF file in your working directory (!):
ggmcmc(angellfit.mcmc)
angellfit.gg <- ggs(angellfit.mcmc)
ggmcmc(angellfit.gg)

## Individual plots:
ggs_histogram(angellfit.gg)
ggs_density(angellfit.gg)
ggs_traceplot(angellfit.gg)
ggs_running(angellfit.gg)

## For more, see http://xavier-fim.net/packages/ggmcmc/

## Yet another option with a similar look, and many diagnostics printed into one file: the mcmcplots package
## If you like the look of ggplot2, check out the package "mcmcplots":
library(mcmcplots) 
denplot(angellfit.mcmc, parms = c("alpha", "beta1", "beta2"))
traplot(angellfit.mcmc, parms = c("alpha", "beta1", "beta2"))
caterplot(angellfit.mcmc)
caterplot(angellfit.mcmc, parms = c("alpha", "beta1", "beta2"), labels = c("Intercept", "Heterogeneity", "Mobility"))
## The following command prints diagnostic plots into a (temporary) HTML file for quick viewing:
mcmcplot(angellfit.mcmc)

## Another convenient function to obtain many diagnostics at once:
## superdiag
library(superdiag)
superdiag(angellfit.mcmc, burnin = 100)