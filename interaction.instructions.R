#####################################################################
## Interaction example: Conditional effects, Bayes vs. frequentist ##
#####################################################################

## Johannes Karreth
## ICPSR Summer Program 2015

## Required libraries
library(R2jags)

## Use example data from Dave Armstrong's package
library(DAMisc)
data(InteractionEx)

## What do the data look like?
head(InteractionEx)

InteractionEx2 <- as.data.frame(scale(as.matrix(InteractionEx)))

## Frequentist model w/ interaction
f.mod <- lm(y ~ x1 + x2 + x1:x2, data = InteractionEx2)
summary(f.mod)

## Look at var-cov matrix (for later)
vcov(lm(y ~ x1 + x2 + x1:x2, data = InteractionEx2))

## Bayesian model w/ interaction
int.model.jags <- function()  {
  
  for(i in 1:N){
    y[i]~dnorm(mu[i], tau)
    mu[i]<-alpha + beta1*x1[i] + beta2*x2[i] + beta3*x1[i]*x2[i]
  }
  
  alpha~dnorm(0, .01)
  beta1~dunif(-100,100)
  beta2~dunif(-100,100)
  beta3~dunif(-100,100)
  tau~dgamma(.01,.01)
  
}

## Data for JAGS

int.data  <- list(y = InteractionEx2$y, x1 = InteractionEx2$x1, x2 =  InteractionEx2$x2, N = length(InteractionEx2$y))

## Name the parameters of the JAGS model you will monitor

int.params <- c("alpha", "beta1", "beta2", "beta3")

## Define the starting values for JAGS

int.inits <- function(){
  list("alpha"=c(0), "beta1"=c(0), "beta2" =c(0), "beta3"= c(0))
}

## Fit the model in JAGS, having previously copied the BUGS model into your working directory as "angell.model.jags"

int.fit <- jags(data=int.data, inits=int.inits, int.params, n.chains=2, n.iter=20000, n.burnin=2000, model.file=int.model.jags)

int.mcmc <- as.mcmc(int.fit)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- as.data.frame(int.mcmc.mat)

x2.sim <- seq(min(InteractionEx2$x2), max(InteractionEx2$x2), by = 0.1)

## Conditional effects of X1 conditional on X2

## Bayes:
int.sim <- matrix(rep(NA, nrow(int.mcmc.dat)*length(x2.sim)), nrow = nrow(int.mcmc.dat))
for(i in 1:length(x2.sim)){
  int.sim[, i] <- int.mcmc.dat$beta1 + int.mcmc.dat$beta3 * x2.sim[i]
}

bayes.c.eff.mean <- apply(int.sim, 2, mean)
bayes.c.eff.lower <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.025)))
bayes.c.eff.upper <- apply(int.sim, 2, function(x) quantile(x, probs = c(0.975)))

## Frequentist
freq.c.eff.pe <- coef(f.mod)[2] + coef(f.mod)[4] * x2.sim
freq.c.eff.sd <- sqrt(vcov(f.mod)[2,2] + x2.sim^2 * vcov(f.mod)[4,4] + 2 * x2.sim * vcov(f.mod)[2,4])

## Combine both estimates
plot.dat <- data.frame(x2.sim, bayes.c.eff.mean, bayes.c.eff.lower, bayes.c.eff.upper, freq.c.eff.pe, freq.c.eff.sd)

## Compare
plot.dat
cor(plot.dat$bayes.c.eff.mean, plot.dat$freq.c.eff.pe)

## Plot both estimates

library(ggplot2)

## Use blue for Bayesian, red for frequentist estimates. Transparency to allow overlay; purple indicates complete overlay.
p <- ggplot(plot.dat, aes(x = x2.sim, y = bayes.c.eff.mean)) + geom_line(color = "blue", alpha = 0.8, size = 0.5)
p <- p + geom_ribbon(aes(ymin = bayes.c.eff.lower, ymax= bayes.c.eff.upper), fill = "blue", alpha = 0.2)
p <- p + geom_line(aes(x = x2.sim, y = bayes.c.eff.lower), color = "blue", alpha = 0.8, size = 0.5) + geom_line(aes(x = x2.sim, y = bayes.c.eff.upper), color = "blue", alpha = 0.8, size = 0.5)
p <- p + geom_line(aes(x = x2.sim, y = freq.c.eff.pe), color = "red", alpha = 0.5, size = 0.5)
p <- p + geom_ribbon(aes(ymin = freq.c.eff.pe - 1.96 * freq.c.eff.sd, ymax= freq.c.eff.pe + 1.96 * freq.c.eff.sd), fill = "red", alpha = 0.1)
p <- p + geom_line(aes(x = x2.sim, y = freq.c.eff.pe - 1.96 * freq.c.eff.sd), color = "red", alpha = 0.5, size = 0.5) + geom_line(aes(x = x2.sim, y = freq.c.eff.pe + 1.96 * freq.c.eff.sd), color = "red", alpha = 0.5, size = 0.5)
p <- p + xlab("X2") + ylab("Conditional effect of X1") + theme_bw()
p