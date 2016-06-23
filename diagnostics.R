#############################################
## Example code to fit Bayesian models and 
## generate convergence diagnostics in R 
#############################################

## Johannes Karreth
## ICPSR Summer Program 2016

## This code is mostly extracted from my tutorial, 
## "Using JAGS and BUGS via R", posted online at 
## <http://www.jkarreth.net/files/Lab2_JAGS-BUGS.pdf>

## Please refer to that file for documentation of the code below.

########################################
## Install required packages (after having installed JAGS separately)
########################################


## install.packages("R2jags", dependencies = TRUE, repos = "https://cloud.r-project.org")
## install.packages("runjags", dependencies = TRUE, repos = "https://cloud.r-project.org")
## install.packages("MCMCpack", dependencies = TRUE, repos = "https://cloud.r-project.org")

## -------------------------------------
library("R2jags")

########################################
## Fit example model to check if R2jags is properly installed
########################################

# An example model file is given in:
model.file <- system.file(package = "R2jags", "model", "schools.txt")

# data
J <- 8.0
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sd <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)

jags.data <- list("y","sd","J")
jags.params <- c("mu","sigma","theta")
jags.inits <- function(){
  list("mu"=rnorm(1),"sigma"=runif(1),"theta"=rnorm(J))
}

# Fit the model
jagsfit <- jags(data=list("y","sd","J"), inits = jags.inits, 
  jags.params, n.iter = 10, model.file = model.file)

########################################
## Simulate data and fit a linear regression model
########################################


## -------------------------------------
n.sim <- 100; set.seed(123)
x1 <- rnorm(n.sim, mean = 5, sd = 2)
x2 <- rbinom(n.sim, size = 1, prob = 0.3)
e <- rnorm(n.sim, mean = 0, sd = 1)

## -------------------------------------
b1 <- 1.2
b2 <- -3.1
a <- 1.5
y <- a + b1 * x1 + b2 * x2 + e

## -------------------------------------
sim.dat <- data.frame(y, x1, x2)

## -------------------------------------
freq.mod <- lm(y ~ x1 + x2, data = sim.dat)
summary(freq.mod)

## -------------------------------------
setwd("~/Documents/Dropbox/Uni/9 - ICPSR/2016/Applied Bayes/Tutorials/2 - JAGS and R")

## -------------------------------------
bayes.mod <- function()  {

for(i in 1:N){
y[i] ~ dnorm(mu[i], tau)
mu[i] <- alpha + beta1 * x1[i] + beta2 * x2[i]
}

alpha ~ dnorm(0, .01)
beta1 ~ dunif(-100, 100)
beta2 ~ dunif(-100, 100)
tau ~ dgamma(.01, .01)

}

## -------------------------------------
y <- sim.dat$y
x1 <- sim.dat$x1
x2 <- sim.dat$x2
N <- nrow(sim.dat)

## -------------------------------------
sim.dat.jags  <- list("y", "x1", "x2", "N")

## -------------------------------------
sim.dat.jags  <- as.list(sim.dat)

## -------------------------------------
sim.dat.jags$N  <- nrow(sim.dat)

## -------------------------------------
bayes.mod.params <- c("alpha", "beta1", "beta2")

## -------------------------------------
bayes.mod.inits <- function(){
	list("alpha" = rnorm(1), "beta1" = rnorm(1), "beta2" = rnorm(1))
  }

## -------------------------------------
inits1 <- list("alpha" = 0, "beta1" = 0, "beta2" = 0) 
inits2 <- list("alpha" = 1, "beta1" = 1, "beta2" = 1)
inits3 <- list("alpha" = -1, "beta1" = -1, "beta2" = -1)
bayes.mod.inits <- list(inits1, inits2, inits3)

## -------------------------------------
library("R2jags")
set.seed(123) 

## -------------------------------------
bayes.mod.fit.R2jags <- jags(data = sim.dat.jags, inits = bayes.mod.inits, 
  parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000, 
  n.burnin = 1000, 
  model.file = "~/Documents/Dropbox/Uni/9 - ICPSR/2016/Applied Bayes/Tutorials/2 - JAGS and R/bayes.mod")

## -------------------------------------
bayes.mod.fit.R2jags <- jags(data = sim.dat.jags, inits = bayes.mod.inits, 
  parameters.to.save = bayes.mod.params, n.chains = 3, n.iter = 9000, 
  n.burnin = 1000, model.file = bayes.mod)

## -------------------------------------
bayes.mod.fit.R2jags.upd <- update(bayes.mod.fit.R2jags, n.iter = 1000)
bayes.mod.fit.R2jags.upd <- autojags(bayes.mod.fit.R2jags) 

## -------------------------------------
print(bayes.mod.fit.R2jags)
plot(bayes.mod.fit.R2jags)
traceplot(bayes.mod.fit.R2jags)

########################################
## Posterior plots and diagnostics
########################################

## -------------------------------------
bayes.mod.fit.mcmc <- as.mcmc(bayes.mod.fit.R2jags)
summary(bayes.mod.fit.mcmc)

## -------------------------------------
library("lattice")
xyplot(bayes.mod.fit.mcmc)

## -------------------------------------
xyplot(bayes.mod.fit.mcmc, layout = c(2, 2), aspect = "fill")

## -------------------------------------
densityplot(bayes.mod.fit.mcmc)

## -------------------------------------
densityplot(bayes.mod.fit.mcmc, layout = c(2, 2), as.table = TRUE, aspect = "fill") 

## -------------------------------------
gelman.plot(bayes.mod.fit.mcmc)
geweke.diag(bayes.mod.fit.mcmc)
raftery.diag(bayes.mod.fit.mcmc)
heidel.diag(bayes.mod.fit.mcmc)

## -------------------------------------
## install.packages("superdiag", dependencies = TRUE, repos = "http://cran.us.r-project.org")

## -------------------------------------
library("superdiag")

## -------------------------------------
superdiag(bayes.mod.fit.mcmc, burnin = 1000)

## -------------------------------------
## install.packages("mcmcplots", dependencies = TRUE, repos = "http://cran.us.r-project.org")

## -------------------------------------
library("mcmcplots")

## -------------------------------------
denplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"))
traplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"))

## -------------------------------------
## mcmcplot(bayes.mod.fit.mcmc)

## -------------------------------------
caterplot(bayes.mod.fit.mcmc, parms = c("alpha", "beta1", "beta2"), 
  labels = c("alpha", "beta1", "beta2"))

## -------------------------------------
## install.packages("ggmcmc", dependencies = TRUE, repos = "http://cran.us.r-project.org")

## -------------------------------------
library("ggmcmc")

## -------------------------------------
bayes.mod.fit.gg <- ggs(bayes.mod.fit.mcmc)
ggs_histogram(bayes.mod.fit.gg)
ggs_density(bayes.mod.fit.gg)
ggs_traceplot(bayes.mod.fit.gg)
ggs_running(bayes.mod.fit.gg)
ggs_compare_partial(bayes.mod.fit.gg)
ggs_autocorrelation(bayes.mod.fit.gg)
ggs_geweke(bayes.mod.fit.gg)
ggs_caterpillar(bayes.mod.fit.gg)

## -------------------------------------
ggmcmc(bayes.mod.fit.gg, 
       file = "~/Documents/Dropbox/Uni/9 - ICPSR/2016/Applied Bayes/Tutorials/2 - JAGS and R/bayes_fit_ggmcmc.pdf")

########################################
## Use runjags instead of R2Jags
########################################

## -------------------------------------
library("runjags")

## -------------------------------------
n.sim <- 100; set.seed(123)
x1 <- rnorm(n.sim, mean = 5, sd = 2)
x2 <- rbinom(n.sim, size = 1, prob = 0.3)
e <- rnorm(n.sim, mean = 0, sd = 1)

## -------------------------------------
b1 <- 1.2
b2 <- -3.1
a <- 1.5
y <- a + b1 * x1 + b2 * x2 + e

## -------------------------------------
sim.dat <- data.frame(y, x1, x2)

## -------------------------------------
freq.mod <- lm(y ~ x1 + x2, data = sim.dat)
summary(freq.mod)

## -------------------------------------
bayes.mod <- "model{

for(i in 1:N){
y[i] ~ dnorm(mu[i], tau)
mu[i] <- alpha + beta1 * x1[i] + beta2 * x2[i]
}

alpha ~ dnorm(0, .01)
beta1 ~ dunif(-100, 100)
beta2 ~ dunif(-100, 100)
tau ~ dgamma(.01, .01)

}"

## -------------------------------------
sim.list  <- as.list(sim.dat)

## -------------------------------------
sim.list$N  <- nrow(sim.dat)

## -------------------------------------
sim.dat.runjags <- dump.format(sim.list)

## -------------------------------------
inits1 <- list(alpha = 1, beta1 = 1, beta2 = 1)
inits2 <- list(alpha = 0, beta1 = 0, beta2 = 0)
inits3 <- list(alpha = -1, beta1 = -1, beta2 = -1)

## -------------------------------------
bayes.mod.fit.runjags <- run.jags(model = bayes.mod, monitor = c("alpha", "beta1", "beta2"), 
  data = sim.dat.runjags, n.chains = 3, inits = list(inits1, inits2, inits3),
  burnin = 1000, sample = 5000, keep.jags.files = TRUE)

## -------------------------------------
print(bayes.mod.fit.runjags)

## -------------------------------------
bayes.mod.fit.mcmc <- as.mcmc.list(bayes.mod.fit.runjags)
summary(bayes.mod.fit.mcmc)

## -------------------------------------
mcmcplot(bayes.mod.fit.mcmc)
superdiag(bayes.mod.fit.mcmc, burnin = 1000)

## -------------------------------------

########################################
## Use JAGS via terminal and analyze posterior in R using coda
########################################


## -------------------------------------
sim.dat.list <- as.list(sim.dat)
sim.dat.list$N  <- nrow(sim.dat)
dump("sim.dat.list", file = "sim.dat.dump")
bugs2jags("sim.dat.dump", "sim.dat")

## -------------------------------------
library("coda")
setwd("~/Documents/Dropbox/Uni/9 - ICPSR/2016/Applied Bayes/Tutorials/2 - JAGS and R/")
chain1 <- read.coda(output.file = "bayes_outchain1.txt", 
  index.file = "bayes_outindex.txt")
chain2 <- read.coda(output.file = "bayes_outchain2.txt", 
  index.file = "bayes_outindex.txt")
chain3 <- read.coda(output.file = "bayes_outchain3.txt", 
  index.file = "bayes_outindex.txt")
bayes.chains <- as.mcmc.list(list(chain1, chain2, chain3))

## -------------------------------------
help(package = "coda")

## -------------------------------------
summary(bayes.chains)

########################################
## Use MCMCpack
########################################

## -------------------------------------
library("MCMCpack")

## -------------------------------------
n.sim <- 100; set.seed(123)
x1 <- rnorm(n.sim, mean = 5, sd = 2)
x2 <- rbinom(n.sim, size = 1, prob = 0.3)
e <- rnorm(n.sim, mean = 0, sd = 1)

## -------------------------------------
b1 <- 1.2
b2 <- -3.1
a <- 1.5
y <- a + b1 * x1 + b2 * x2 + e

## -------------------------------------
sim.dat <- data.frame(y, x1, x2)

## -------------------------------------
freq.mod <- lm(y ~ x1 + x2, data = sim.dat)
summary(freq.mod)

## -------------------------------------
bayes.mod.fit.MCMCpack <- MCMCregress(y ~ x1 + x2, data = sim.dat, burnin = 1000,
  mcmc = 5000, seed = 123, beta.start = c(0, 0, 0),
  b0 = c(0, 0, 0), B0 = c(0.1, 0.1, 0.1))
summary(bayes.mod.fit.MCMCpack)

## -------------------------------------
mcmcplot(bayes.mod.fit.MCMCpack)
superdiag(bayes.mod.fit.MCMCpack, burnin = 1000)