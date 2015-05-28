#########################################################
## Economic voting in Austria: Multinomial logit model ##
#########################################################

## Johannes Karreth
## ICPSR Summer Program 2015

library(foreign)
library(R2jags)

setwd("~/R/Bayes/econvote.mnl")

econ.voting.dat <- read.dta("http://www.jkarreth.net/files/econ.voting.dta")

y <- econ.voting.dat$y
econwor <- econ.voting.dat$econwor
econbet <- econ.voting.dat$econbet
age <- econ.voting.dat$age
income <- econ.voting.dat$income
rural <- econ.voting.dat$rural
gender <- econ.voting.dat$gender
leftrt <- econ.voting.dat$leftrt
N <- length(y)
J <- length(as.numeric(levels(as.factor(y))))  ## number of categories, for our model code

econvote.dat <- list("y", "econwor", "econbet", "age", "income", "rural", "gender", "leftrt", "N", "J")

econvote.model.jags <- function()  {

   for(i in 1:N){
       y[i] ~ dcat(p[i, 1:J])
       
       for (j in 1:J){
           log(q[i,j]) <-  b[1,j] + 
                           b[2,j]*econwor[i] + 
                           b[3,j]*econbet[i] + 
                           b[4,j]*age[i] + 
                           b[5,j]*income[i] + 
                           b[6,j]*rural[i] + 
                           b[7,j]*gender[i] + 
                           b[8,j]*leftrt[i]
           
         p[i,j] <- q[i,j]/sum(q[i,1:J])  ## should be familiar from MLE notes: q is exp(Xb)
           }   # close J loop
          pred[i,1] <- equals(p[i,1], max(p[i,1], p[i,2], p[i,3], p[i,4])) # "1 if p[i,1] = max(p[i,1], p[i,2], p[i,3]), 0 otherwise"
            pred[i,2] <- equals(p[i,2], max(p[i,1], p[i,2], p[i,3], p[i,4]))
            pred[i,3] <- equals(p[i,3], max(p[i,1], p[i,2], p[i,3], p[i,4]))
          pred[i,4] <- equals(p[i,3], max(p[i,1], p[i,2], p[i,3], p[i,4]))
                 
            predcat[i] <- pred[i,1] + 2*pred[i,2] + 3*pred[i,3] + 4*pred[i,4]
       }  # close N loop
           
      for(k in 1:8){
           b[k,1] <- 0          ## MUST set the first set of covariates (for the first outcome category) to 0
               for(j in 2:J){
                    b[k,j] ~ dnorm(0, 0.1)
                   }  # close J loop
               }  # close K loop
       }  # close model loop 

econ.params <- c("b")

econ.inits <- list(b = structure(.Data = c(
            NA,2.814591768050622,2.672802250229753,-5.708160780551808,            
            NA,-2.103556728095819,-0.02465655691760306,-1.112744901881126,            
            NA,-5.534073839338205,0.377378254003351,-0.4298896403932389,            
            NA,2.397701213045189,3.25663165065113,2.910457003304038,            
            NA,-5.411360350420177,1.827996299302545,-2.788501794152787,
            NA,2.438577159694778,1.268176634066079,-0.1112438180561168,            
            NA,-2.744934104329091,3.729902592818073,1.696162903833417,            
            NA,1.624966201308485,2.559685176740751,-0.5726405182073309),
    .Dim = c(8,4)))
    
## To generate your own starting values from a MNL model, try:
library(mlogit)
econ.voting.dat$yfac <- as.factor(econ.voting.dat$y)
mnl.dat <- mlogit.data(econ.voting.dat, varying=NULL, choice="yfac", shape="wide")
mlogit.mod <- mlogit(yfac ~ 1|econwor + econbet + age + income + rural + gender + leftrt, data=mnl.dat, reflevel="1")
coefs <- as.vector(summary(mlogit.mod)$coefficients)
coefs.mat <- matrix(coefs, byrow=TRUE, 8,3)
NAs <- matrix(c(NA, NA, NA, NA, NA, NA, NA, NA), 8, 1)
inits.mat <- cbind(NAs, coefs.mat)
inits <- list(b=inits.mat)
econ.inits <- list(inits, inits)

econvote.fit <- jags(data=econvote.dat, inits=econ.inits, econ.params, n.chains=2, n.iter=10, n.burnin=1, model.file=econvote.model.jags)