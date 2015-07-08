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
# install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
regtable <- mcmctab(as.mcmc(angell.fit))

## Use regtable() for the summary

## pick out the parameters of interest (here, the coefficients and SDs)

regtable1 <- regtable[c(1:3), c(1:2)]

library(xtable)
regtable1 <- xtable(regtable1)
print(regtable1, type = "latex")
print(regtable1, type = "html")

## you can save the HTML table as a .doc file and open it with MS word. You might have to make some modifications, but the basic layout should be functionable.
print(regtable1, type = "html", 
	file = "~/Desktop/angell_table.doc")

## pick out other parameters of interest (here, the coefficients and CIs)

regtable2 <- regtable[c(1:3), c(1, 3, 4)]

xtable(regtable2)