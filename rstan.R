############################
## Run Stan through rstan ##
############################

# Johannes Karreth
# jkarreth@albany.edu

# The code below adapts the help file from jags() for use with stan()
# Adjust file paths to your system

# data
y <- c(28.4,7.9,-2.8,6.8,-0.6,0.6,18.0,12.2)
sigma <- c(14.9,10.2,16.3,11.0,9.4,11.4,10.4,17.6)

schools_data <- as.list(data.frame(y, sigma))
schools_data$J <- 8


schools_mod <- "
data {
      int<lower=0> J; // number of schools
      real y[J];      // estimated treatment effects
      real<lower=0> sigma[J]; // s.d. of effect estimates
    }
parameters {
      real mu;
      real<lower=0> tau;
      vector[J] eta;
    }
transformed parameters {
      vector[J] theta;
      theta <- mu + tau * eta;
    }
model {
      eta ~ normal(0, 1);
      y ~ normal(theta, sigma);
}

"

schools_fit <- stan(model_code = schools_mod, 
                data = schools_data,
                chains = 4, iter = 2000, warmup = 1000, thin = 1,
                init = "0", seed = 123, verbose = FALSE)

## Same with Angell data & OLS

# Read the data in car library
library(car)
data(Angell)
angell_dat <- Angell[, -4] ## take off the fourth column (remember, the order is (row, column))
angell_dat <- as.list(angell_dat)
angell_dat$N <- length(angell_dat$moral)

## Model

angell_mod <- "
  
data{
  int<lower = 0> N;
  vector[N] moral;
  vector[N] hetero;
  vector[N] mobility;
}

parameters{
  vector[3] beta;
  real<lower = 0> sigma;
}

model{
  moral ~ normal(beta[1] + beta[2] * hetero + beta[3] * mobility,
                  sigma);
}

"

## Fit the model in rstan

angell_fit <- stan(model_code = angell_mod, 
                    data = angell_dat,
                    chains = 4, iter = 2000, warmup = 1000, thin = 1,
                    init = "0", seed = 123, verbose = FALSE)