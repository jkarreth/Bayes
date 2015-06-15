JKmcmctab <- function(sims, mcmc = TRUE, jags = TRUE) 
{
  if(mcmc == FALSE & jags == TRUE){
    sims <- as.mcmc(sims)
  }
  
  if(jags == FALSE){
    require(mcmcplots)
    sims <- convert.mcmc.list(sims)
  }
  
  dat <- t(as.matrix(sims))
    mcmctab <- apply(dat, 1, 
    	function(x) c(Mean = round(mean(x), digits = 3), 
    		SD = round(sd(x), digits = 3), 
    		Lower = as.numeric(round(quantile(x, probs = c(0.025)), digits = 3)), 
    		Upper = as.numeric(round(quantile(x, probs = c(0.975)), digits = 3))
    		))
    return(t(mcmctab))
}