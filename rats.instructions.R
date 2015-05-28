######################################################
## WinBUGS example Rats - Normal hierarchical model ##
######################################################

## Johannes Karreth
## ICPSR Summer Program 2015

## Required libraries
library(R2jags)

## Model

rats.model <- function()  {
		for( i in 1 : N ) {
			for( j in 1 : T ) {
				Y[i , j] ~ dnorm(mu[i , j],tau.c)
				mu[i , j] <- alpha[i] + beta[i] * (x[j] - xbar)
			}
			alpha[i] ~ dnorm(alpha.c,tau.alpha)
			beta[i] ~ dnorm(beta.c,tau.beta)
		}
		tau.c ~ dgamma(0.001,0.001)
		sigma <- 1 / sqrt(tau.c)
		alpha.c ~ dnorm(0.0,1.0E-6)	 
		sigma.alpha~ dunif(0,100)
		sigma.beta~ dunif(0,100)
		tau.alpha<-1/(sigma.alpha*sigma.alpha)
		tau.beta<-1/(sigma.beta*sigma.beta)
		beta.c ~ dnorm(0.0,1.0E-6)
		alpha0 <- alpha.c - xbar * beta.c	
	}

## Copy the data from the WinBUGS code:

rats.data  <- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,	
		Y = structure(
			.Data =   c(151, 199, 246, 283, 320,
							 145, 199, 249, 293, 354,
							 147, 214, 263, 312, 328,
							 155, 200, 237, 272, 297,
							 135, 188, 230, 280, 323,
							 159, 210, 252, 298, 331,
							 141, 189, 231, 275, 305,
							 159, 201, 248, 297, 338,
							 177, 236, 285, 350, 376,
							 134, 182, 220, 260, 296,
							 160, 208, 261, 313, 352,
							 143, 188, 220, 273, 314,
							 154, 200, 244, 289, 325,
							 171, 221, 270, 326, 358,
							 163, 216, 242, 281, 312,
							 160, 207, 248, 288, 324,
							 142, 187, 234, 280, 316,
							 156, 203, 243, 283, 317,
							 157, 212, 259, 307, 336,
							 152, 203, 246, 286, 321,
							 154, 205, 253, 298, 334,
							 139, 190, 225, 267, 302,
							 146, 191, 229, 272, 302,
							 157, 211, 250, 285, 323,
							 132, 185, 237, 286, 331,
							 160, 207, 257, 303, 345,
							 169, 216, 261, 295, 333,
							 157, 205, 248, 289, 316,
							 137, 180, 219, 258, 291,
							 153, 200, 244, 286, 324),
						.Dim = c(30,5)))
						
## Now define the vectors of the data matrix for JAGS:

Y <- rats.data$Y
T <- rats.data$T
x <-  rats.data$x
xbar <- rats.data$xbar
N <- rats.data$N

## Read in the rats data for JAGS

rats.data  <- list("Y", "x", "T", "N", "xbar")

## Name the JAGS parameters

rats.params <- c("tau.c", "sigma", "alpha.c", "sigma.alpha", "sigma.beta", "tau.alpha", "tau.beta", "beta.c", "alpha0")

## Define the starting values for JAGS

rats.inits <- function(){
	list("alpha"=c(250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250, 250), 
		"beta"=c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6), 
		"alpha.c"=c(150), "beta.c"=c(10), "tau.c"=c(1), "sigma.alpha"=c(1), "sigma.beta"=c(1))
}

## Fit the model in JAGS, having previously copied the BUGS model in my working directory as "rats.model.jags"

ratsfit <- jags(data=rats.data, inits=rats.inits, rats.params, n.chains=2, n.iter=2000, n.burnin=1000, model.file=rats.model)

## If we wanted to specify different initial values for the different chains, specify:
## inits = c() ???

## Updating?
ratsfit.upd <- update(ratsfit, n.iter=1000)
ratsfit.upd <- autojags(ratsfit)

## Diagnoses

print(ratsfit)

plot(ratsfit)

traceplot(ratsfit)
## if you want to save them:
pdf("rats.trace.pdf")
traceplot(ratsfit)
dev.off()

## Generate MCMC object for analysis
ratsfit.mcmc <- as.mcmc(ratsfit)

summary(ratsfit.mcmc)

xyplot(ratsfit.mcmc)
## maybe better (you can use other Lattice options here as well):
xyplot(ratsfit.mcmc, layout=c(2,6), aspect="fill")

densityplot(ratsfit.mcmc)
## maybe better:
densityplot(ratsfit.mcmc, layout=c(2,6), aspect="fill") 

## and more plots... all using the MCMC object and the CODA package:

## Trace- and density in one plot
pdf("ratsfit.mcmc.plot.pdf")
plot(ratsfit.mcmc)
dev.off()

## Autocorrelation plot
pdf("ratsfit.mcmc.autocorr.pdf")
autocorr.plot(ratsfit.mcmc)
dev.off()

## Others (not working with this example - yet. Convergence problems?)

gelman.plot(ratsfit.mcmc)
geweke.diag(ratsfit.mcmc)
geweke.plot(ratsfit.mcmc)
raftery.diag(ratsfit.mcmc)
heidel.diag(ratsfit.mcmc)

## If we want to examine the Coda files via Boa or Coda, use the function jags2:

ratsfit <- jags2(data=rats.data, inits=rats.inits, rats.params, n.iter=5000, model.file="rats.model.jags")

print(ratsfit)
plot(ratsfit)
## etc.

## This will create the following files in your working directory:
/Users/johanneskarreth/R/CODAchain1.txt
/Users/johanneskarreth/R/CODAchain2.txt
/Users/johanneskarreth/R/CODAchain3.txt
/Users/johanneskarreth/R/CODAindex.txt
/Users/johanneskarreth/R/jagsdata.txt
/Users/johanneskarreth/R/jagsinits1.txt
/Users/johanneskarreth/R/jagsinits2.txt
/Users/johanneskarreth/R/jagsinits3.txt
/Users/johanneskarreth/R/jagsscript.txt

## ... which can then be analyzed 
library(boa)
boa.menu()

## remember to rename the index file's extension to .ind, and the chain files to .out

library(coda)
codamenu()