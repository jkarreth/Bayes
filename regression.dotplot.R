#############################################
## Bayesian regression results as dot plot ##
######## Workflow for R2Jags/R2WinBUGS ######
#############################################

## Johannes Karreth
## jkarreth@albany.edu

## if angell.fit is your R2jags/R2WinBUGS output object, and 
## angellfit.mcmc is that object as mcmc object:

## using the mcmcplots package:

library(mcmcplots)
caterplot(angellfit.mcmc, parms = c("beta1", "beta2"), labels = c("Diversity", "Mobility"), val.lim = c(-0.27, 0.05))
abline(v = 0)

## using the arm package:

coef.vect <- angellfit$BUGSoutput$summary[2:3, 1]
sd.vect <- angellfit$BUGSoutput$summary[2:3, 2]
short.names <- rownames(angellfit$BUGSoutput$summary[2:3,])
long.names <- c("Diversity", "Mobility")

library(arm)
coefplot(coef.vect, sd.vect, varnames=long.names, main="", xlim = c(-0.3, 0.05))

## using ggplot2 from scratch
library(ggplot2)
angellfit.mat <- as.matrix(angellfit.mcmc)
angellfit.dat <- as.data.frame(angellfit.mat)
coef.vect <- apply(angellfit.dat, 2, mean)
lower.vect <- apply(angellfit.dat, 2, function(x) quantile(x, probs = c(0.025)))
upper.vect <- apply(angellfit.dat, 2, function(x) quantile(x, probs = c(0.975)))
long.names <- c("Intercept", "Diversity", "Mobility", "Deviance")
plot.dat <- data.frame(coef.vect, lower.vect, upper.vect, long.names)[c(2,3), ]

p <- ggplot(data = plot.dat, aes(x = coef.vect, y = long.names)) + geom_point() + geom_segment(aes(x = lower.vect, xend = upper.vect, y = long.names, yend = long.names))
p <- p + geom_vline(xintercept = 0, colour = "blue", linetype = 2) + xlab("Posterior estimates") + ylab("")
p