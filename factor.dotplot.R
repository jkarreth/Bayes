########################################
## Bayesian factor scores as dot plot ##
#### Workflow for R2Jags/R2WinBUGS #####
########################################

## Johannes Karreth
## jkarreth@albany.edu

library(lattice)
library(ggplot2)
library(stats)

## if fa.fit is your factor model output from R2jags or R2WinBUGS:
fa.mcmc <- as.mcmc(fa.fit)
fa.mat <- as.matrix(fa.mcmc)
fa.dat <- as.data.frame(fa.mat)

fa.dat <- fa.dat[, -1]

## name your predicted factor latent.mean, and the CI between latent.lower and latent.upper
latent.mean <- apply(fa.dat, 2, mean)
latent.lower <- apply(fa.dat, 2, function(x) quantile(x, probs = c(0.025)))
latent.upper <- apply(fa.dat, 2, function(x) quantile(x, probs = c(0.975)))
subject <- colnames(fa.dat)

dat <- data.frame(latent.mean, latent.lower, latent.upper, subject)

## here, take only 50 of the 1500 observations (just for demonstration)
plot.dat <- dat[sample(1:nrow(dat), 50, replace=FALSE),]

## order the data by the factor scores, for better visualization

plot.dat <- plot.dat[order(plot.dat$latent.mean), ]

## order the observation IDs as well (seems redundant, but is necessary)

plot.dat$subject2 <- reorder(plot.dat$subject, plot.dat$latent.mean)

## define range of the scores, for the axis limits in the plot

rg <- diff(range(c(plot.dat$latent.upper, plot.dat$latent.lower)))

## make plot using the lattice package:

dotplot(subject2 ~ latent.mean, data=plot.dat,scales=list(y=list(cex=.45)), xlim=c(min(plot.dat$latent.lower)-.1*rg, max(plot.dat$latent.upper)+.1*rg),main="Latent trait", panel=function(x,y, subscripts){
  panel.abline(h = as.numeric(y), col = "gray80", lty = 2)
  panel.segments(plot.dat$latent.lower[subscripts], y, plot.dat$latent.upper[subscripts], y, lty=1, col="gray40")
  panel.points(x,y, pch=16, col="black")})

## make plot using the ggplot2 package:
library(ggplot2)
factorplot <- ggplot(plot.dat, aes(x = latent.mean, y = subject2)) + geom_point() + geom_segment(aes(x = latent.lower, xend = latent.upper, y = subject2, yend = subject2))
factorplot <- factorplot + xlab("Latent trait") + ylab("") + theme_bw()
factorplot <- factorplot + geom_text(aes(x = latent.upper + 0.1, label = rownames(plot.dat)), size = 3) + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
factorplot
