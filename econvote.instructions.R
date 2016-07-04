#########################################################
## Economic voting in Germany: Multinomial logit model ##
#########################################################

## Johannes Karreth
## ICPSR Summer Program 2016

library(R2jags)

# Download the data from https://dbk.gesis.org/DBKSearch/download.asp?id=37837

d <- rio::import("2009_ZA5384_v1-0-0.dta")

# Data cleaning and recoding

d$votechoice <- NA
d$votechoice <- ifelse(d$v3c == 1, 1, d$votechoice) # CDU/CSU
d$votechoice <- ifelse(d$v3c == 2, 2, d$votechoice) # SPD
d$votechoice <- ifelse(d$v3c == 3, 3, d$votechoice) # FDP
d$votechoice <- ifelse(d$v3c == 4, 4, d$votechoice) # Linke
d$votechoice <- ifelse(d$v3c == 5, 5, d$votechoice) # Green

d$interest <- d$v15 - 3
d$interest <- ifelse(d$interest == 3, NA, d$interest)

d$leftright <- d$v56 - 6
d$leftright <- ifelse(d$leftright == 6, NA, d$leftright)

d$age <- d$vb - 5

d$children <- ifelse(d$vx1 == 1, 1, ifelse(d$vx1 == 2, 0, NA))

d$education <- ifelse(d$vf == 4 | d$vf == 5, -1, ifelse(d$vf == 1, 0, ifelse(d$vf == 2, 1, ifelse(d$vf == 3, 2, NA))))

d$union <- ifelse(d$vp == 1 | d$vp == 2 | d$vp == 3, 1, ifelse(d$vp == 4, 0, NA))

d$female <- d$va- 1

d$econcountrynow <- ifelse(d$v25 == 1, 1, ifelse(d$v25 == 2, 0, ifelse(d$v25 == 3, 0, NA)))

d$econselfnow <- ifelse(d$v27 == 1, 1, ifelse(d$v27 == 2, 0, ifelse(d$v27 == 3, 0, NA)))

d$econctryworse <- ifelse(d$v25 == 1, 0, ifelse(d$v25 == 2, 0, ifelse(d$v25 == 3, 1, NA)))

d$econselfworse <- ifelse(d$v27 == 1, 0, ifelse(d$v27 == 2, 0, ifelse(d$v27 == 3, 1, NA)))


d$econcountryfuture <- ifelse(d$v29 == 2, -1, ifelse(d$v29 == 3, 0, ifelse(d$v29 == 1, 1, NA)))

d$econselffuture <- ifelse(d$v28 == 3, -1, ifelse(d$v28 == 2, 0, ifelse(d$v29 == 1, 1, NA)))

d$fearterror <- ifelse(d$v43 == 1, 1, ifelse(d$v43 == 2, 0, NA))

d$class <- ifelse(d$vl2 == 1, -1, ifelse(d$vl2 == 2, 0, ifelse(d$vl2 == 3, 1, NA)))

d <- d[, c("votechoice", "interest", "leftright", "age", "children", "education", "union", "female", "econctryworse", "econselfworse")]

# Data for JAGS/BUGS

d_nm <- na.omit(d[, c("votechoice", "interest", "leftright", "age", "children", "education", "union", "female", "econctryworse", "econselfworse")])
dj <- as.list(d_nm)
dj$N <- nrow(d_nm)
dj$J <- length(as.numeric(levels(as.factor(dj$votechoice))))

econ.mod <- function()  {
  
  for(i in 1:N){
    votechoice[i] ~ dcat(p[i, 1:J])
    
    for (j in 1:J){
      log(q[i,j]) <-  b[1,j] + 
        b[2,j] * interest[i] + 
        b[3,j] * leftright[i] + 
        b[4,j] * age[i] + 
        b[5,j] * children[i] + 
        b[6,j] * education[i] + 
        b[7,j] * union[i] + 
        b[8,j] * female[i] +
        b[9,j] * econctryworse[i] + 
        b[10,j] * econselfworse[i]
      s
      p[i,j] <- q[i,j]/sum(q[i,1:J])  ## should be familiar from MLE notes: q is exp(Xb)
    }   # close J loop
  }  # close N loop
  
  for(k in 1:10){
    b[k,1] <- 0          ## MUST set the first set of covariates (for the first outcome category) to 0
    for(j in 2:J){
      b[k,j] ~ dnorm(0, 0.1)
    }  # close J loop
  }  # close K loop
}  # close model loop 

econ.params <- c("b")

econ.inits <- function(){
  list(b = matrix(c(rep(NA, 10), 
                    rep(0, 40)), 
                  nrow = 10, ncol = 5, byrow = FALSE))
}

library(R2jags)
econ.fit <- jags(data = dj, inits = econ.inits, econ.params, n.chains = 3, n.iter = 10, n.burnin = 5, n.thin = 1, model.file = econ.mod)
econ.mcmc <- as.mcmc(econ.fit)
library(mcmcplots)
mcmcplot(econ.mcmc)

econ.mcmc.dat <- as.data.frame(as.matrix(econ.mcmc))
devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(econ.mcmc)