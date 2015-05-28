####################################################################
## R2jags example - Bush votes in 1988 - Multilevel model example ##
####################################################################

## Johannes Karreth
## jkarreth@albany.edu

## Note: This example is taken from Gelman & Hill, Ch. 17. 
## state.mod is a simplified version with fewer interactions 
## and with categorical instead of dummy variables for the age & education categories.

## Simple model

state.mod <- function() {
  for (i in 1:n){
    y[i] ~ dbin (p.bound[i], 1)
    p.bound[i] <- max(0, min(1, p[i]))
    logit(p[i]) <- Xbeta[i]
    Xbeta[i] <- b.female*female[i] + b.black*black[i] +
      b.age*age[i] + b.edu*edu[i]
      + b.state[state[i]]
  }
  b.female ~ dnorm (0, .01)
  b.black ~ dnorm (0, .01)
  b.age ~ dnorm (0, .01)
  b.edu ~ dnorm (0, .01)

 
  for (j in 1:n.state){
    b.state[j] ~ dnorm(b.state.hat[j], tau.state)
    b.state.hat[j] <- b.state0 + b.v.prev*v.prev[j]
    }
    b.state.hat.mu<-mean(b.state.hat[])
    b.v.prev ~ dnorm(0, .01)
    b.state0 ~ dnorm(0, .01)
    tau.state <- pow(sigma.state, -2)
    sigma.state ~ dunif (0, 100)
}

setwd("~/R/Bayes/mlm.states")

library(R2jags)
library(foreign)

### Read in the data

## Option A) Use complete dataset including level-2 predictor

polls.subset <- read.dta("polls.subset.merged.dta")

y <- polls.subset$bush
female <- polls.subset$female
black <- polls.subset$black
age <- polls.subset$age
edu <- polls.subset$edu

# To get ID for state beginning with 1, do this:
# (Note that it is redundant in this case, but useful in other situations)
uniqstate <- unique(polls.subset$state)
polls.subset$stateid <- match(polls.subset$state, uniqstate)
state <- polls.subset$stateid

v.prev <- as.vector(by(polls.subset$g76_84pr, polls.subset$stateid, mean))

## Option B) Combine level-1 and level-2 dataset in R

polls.subset <- read.dta("polls.subset.JK.dta")

# Load in level-1 data

y <- polls.subset$bush
female <- polls.subset$female
black <- polls.subset$black
age <- polls.subset$age
edu <- polls.subset$edu
state <- polls.subset$state

# Load in level-2 data. Note that these data should be ordered by state,
# so you will NOT need a level-2 ID

presvote <- read.dta("presvote.dta")
v.prev  <- presvote$g76_84pr

### Data are ready

### Define number of observations on each level

n <- length(y)             # of survey respondents
n.state <- max(state)      # of states

### Fit the model in JAGS

state.dat <- list ("n", "n.state", "y", "female", "black", "age", "edu", "state", "v.prev")
 
state.params <- c ("b.female", "b.black", "b.age", "b.edu", "b.state.hat",  "b.v.prev")

state.inits <- function (){
 list(b.female = c(0), b.black = c(0), 
  b.age = c(0), b.edu = c(0), b.state = rnorm(n.state), b.v.prev = c(0))
}

state.fit <- jags(data=state.dat, inits=state.inits, state.params, model.file=state.mod, n.chains=2, n.iter=100, n.burnin=10)

### Here, in nested structures, the plot() command in R2Jags comes in handy
plot(state.fit)

### Compare to GLM estimates

state.df <- read.dta("polls.subset.merged.dta")
library(arm)
state.lmer <- lmer(bush ~ edu + age + female + black + g76_84pr + (1|state), data=state.df, family=binomial) 
summary(state.lmer)