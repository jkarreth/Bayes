#################################################
## Run OpenBUGS through R2OpenBUGS on Mac OS X ##
#################################################

# Johannes Karreth
# jkarreth@albany.edu

# The code below uses the help file for bugs()
# Adjust file paths to your system

library(R2OpenBUGS)

model.file <- system.file(package="R2WinBUGS", "model", "schools.txt")

# Some example data (see ?schools for details):
data(schools)

J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list(J=J, y=y, sigma.y=sigma.y)
inits <- function(){
  list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
       sigma.theta=runif(1, 0, 100))
}
## or alternatively something like:
# inits <- list(
#   list(theta=rnorm(J, 0, 90), mu.theta=rnorm(1, 0, 90),
#        sigma.theta=runif(1, 0, 90)),
#   list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
#        sigma.theta=runif(1, 0, 100))
#   list(theta=rnorm(J, 0, 110), mu.theta=rnorm(1, 0, 110),
#        sigma.theta=runif(1, 0, 110)))

parameters <- c("theta", "mu.theta", "sigma.theta")

schools.sim <- bugs(data = data, 
                    inits = inits, 
                    parameters = parameters, 
                    model.file = model.file,
                    n.chains=3, 
                    n.burnin = 2500,
                    n.iter=5000,
                    debug = FALSE,
                    OpenBUGS.pgm = "/Applications/Wineskin/OpenBUGS.app/Contents/Resources/drive_c/Program Files/OpenBUGS/OpenBUGS323/OpenBUGS.exe",
                    working.directory = "/Users/johanneskarreth/R/Bayes/R2WinBUGS",
                    useWINE = TRUE,
                    bugs.seed = 12)

print(schools.sim)
plot(schools.sim)

# For a function to create a barebones summary table, use
# https://github.com/jkarreth/JKmisc/blob/master/mcmctab.R
# install.packages("devtools")
devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(schools.sim)

# Access posterior draws for postestimation
# Rows are draws, columns are parameters

schools.out <- schools.sim$sims.matrix