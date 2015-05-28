###########################################
## Bayesian regression results to tables ##
###### Workflow for R2Jags/R2WinBUGS ######
###########################################

## Johannes Karreth
## jkarreth@albany.edu

## if angell.fit is your R2jags/R2WinBUGS output object

angell.fit$BUGSoutput$summary[, c(1, 2, 3, 7)]
library(xtable)
regtable1 <- xtable(angell.fit$BUGSoutput$summary[, c(1, 2, 3, 7)])
print(regtable1, type = "latex")
print(regtable1, type = "html")

## Another option:
## use John Baumgartner's jagstools package:
## http://johnbaumgartner.wordpress.com/2012/06/07/r-functions-to-filter-rjags-results/

## First, install:
install.packages("devtools")
library(devtools)
install_github(repo="jagstools", username="johnbaums")

## Use regtable() for the summary

library(jagstools)
regtable <- jagsresults(x = angellfit, params = c("alpha", "beta1", "beta2"), exact = FALSE)

## pick out the parameters of interest (here, the coefficients and SDs)

regtable1 <- regtable[, 1:2]

library(xtable)
regtable1 <- xtable(regtable1)
print(regtable1, type = "latex")
print(regtable1, type = "html")

## you can save the HTML table as a .html file and open it with MS word. You might have to make some modifications, but the basic layout should be functionable.

## pick out the parameters of interest (here, the coefficients and CIs)

regtable2 <- regtable[, c(1,3,7)]

xtable(regtable2)