###############################################
## Run WinBUGS through R2WinBUGS on Mac OS X ##
###############################################

# Johannes Karreth
# jkarreth@albany.edu

# The code below uses the help file for bugs()
# Adjust file paths to your system

library(R2WinBUGS)

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

schools.sim <- bugs(data, inits, parameters, model.file,
                    n.chains=3, n.iter=5000,
                    bugs.directory="/Applications/Wineskin/WinBUGS.app/Contents/Resources/drive_c/Program Files/WinBUGS14")
print(schools.sim)
plot(schools.sim)

# For a summary table, use
# https://github.com/jkarreth/JKmisc/blob/master/mcmctab.R
devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/mcmctab.R")
mcmctab(schools.sim, jags = FALSE)

# Access posterior draws for postestimation
# Rows are draws, columns are parameters

schools.out <- schools.sim$sims.matrix