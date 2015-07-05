#####################################################################
## R2jags example - Students' goals - Multilevel MNL model example ##
#####################################################################

## Johannes Karreth
## jkarreth@albany.edu

## Note: This example is taken from Lunn et al. (The BUGS Book), Example 10.3.4
## Their WINBUGS code can be downloaded at 
## <http://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-the-bugs-book/bugs-book-examples/>

goals.dat <- read.csv("http://www.jkarreth.net/files/goals.csv")

goals.datjags <- as.list(goals.dat)
goals.datjags$npupils <- nrow(goals.dat)
goals.datjags$nschools <- length(unique(goals.datjags$School))
goals.datjags$J <- length(unique(goals.datjags$Goals))

## Pooled model
## Only one coefficient for boys & goal 1 (sports)

mod_pool <- function() { 
  for (i in 1:npupils) {
    Goals[i] ~ dcat(p[i, 1:J])
    
    for (j in 1:J) {
      log(q[i,j]) <- a[i, j]
      p[i, j] <- q[i, j] / sum(q[i, 1:J])
    }
    a[i, 1] <- b[1] + b.boy * Gender[i] # only estimate b for Goals = 1
    a[i, 2] <- b[2]
    a[i, 3] <- 0
  }
  
  b[1] ~ dnorm(0, 0.0001)
  b[2] ~ dnorm(0, 0.0001)
  b.boy ~ dnorm(0, 0.0001)
  or.boy <- exp(b.boy)

} 

inits.pool1 <- list("b.boy" = 0, "b" = c(0, 0)) 
inits.pool2 <- list("b.boy" = 0, "b" = c(0, 0)) 
inits.pool3  <- list("b.boy" = 0, "b" = c(0, 0)) 
inits.pool <- list(inits.pool1, inits.pool2, inits.pool3)

params <- c("b.boy", "b")

fit.pool <- jags(data = goals.datjags, inits = inits.pool, parameters.to.save = params,
                 n.chains = 3, n.iter = 1000, n.burnin = 500,
                 model.file = mod_pool)

## Varying intercepts for schools
## Still only one coefficient for boys & goal 1 (sports)

mod_vi <- function() { 
  for (i in 1:npupils) {
    Goals[i] ~ dcat(p[i, 1:J])
    
    for (j in 1:J) {
      log(q[i,j]) <- a[i, j]
      p[i, j] <- q[i, j] / sum(q[i, 1:J])
    }
    a[i, 1] <- b[School[i], 1] + b.boy * Gender[i] # only estimate b for Goals = 1
    a[i, 2] <- b[School[i], 2]
    a[i, 3] <- 0
  }
  
  b.boy ~ dnorm(0, 0.0001)
  or.boy  <- exp(b.boy)
  
  for (j in 1:nschools) {
    b[j, 1] ~ dnorm(0, 0.0001)
    b[j, 2] ~ dnorm(0, 0.0001)
    b[j, 3] <- 0
  }
  
} 

inits.vi1 <- list("b.boy" = 0, "b" = structure(.Data=c(0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA),.Dim=c(9,3))) 
inits.vi2 <- list("b.boy" = 0, "b" = structure(.Data=c(0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA),.Dim=c(9,3))) 
inits.vi3  <- list("b.boy" = 0, "b" = structure(.Data=c(0, 0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA, NA, NA, NA, NA),.Dim=c(9,3))) 
inits.vi <- list(inits.vi1, inits.vi2, inits.vi3)

params <- c("b.boy", "b")

fit.vi <- jags(data = goals.datjags, inits = inits.vi, parameters.to.save = params,
                 n.chains = 3, n.iter = 1000, n.burnin = 500,
                 model.file = mod_vi)