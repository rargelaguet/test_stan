
# Specify dimensions
N <- 1000  # Number of samples
D <- 10    # Number of features

Y <- list()
X <- list()
for (i in 1:opts$ntrials) {
  
  # Sample precision and inactivate some covariates elements
  alpha <- rep(0.1,D)
  alpha[sample(c(TRUE,FALSE),size=D, replace=TRUE)] <- 1e6 
  
  # Sample weights (per feature)
  w <- sapply(1/sqrt(alpha), function(a) rnorm(1, mean=0, sd=a))
  
  # Sample covariates
  X[[i]] <- matrix(rnorm(N*D), N, D)
  
  # Sample observations and add noise to every sample
  Y[[i]] <- rbinom(N, 1, boot::inv.logit(c(X[[i]]%*%w + rnorm(N))))
}


