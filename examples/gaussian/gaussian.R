
# https://www.cs.helsinki.fi/u/sakaya/tutorial/
  
# Consider, for instance, inferring a Gaussian with an unknown mean and variance
#   x ~ N(mu, sigma)
#     mu ~ N(0,10)

gaussian <- "

data {
  int<lower=1> n;
  vector[n] x;
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  mu ~ normal(0, 10);
  x ~ normal(mu, sigma);
} 
"

library("rstan")

# simulate gaussian data with mean=5 and sd=10
n <- 1000
x <- rnorm(n, mean=5, sd=10)
data <- list(x=x, n=n)

# Fit stan model via MCMC
m <- stan_model(model_code = gaussian)
samples <- sampling(m, data=data, iter=10000, chains=1)

# Extract mean and standard deviation from the posterior
mu <- mean(extract(samples)$mu)
sigma <- mean(extract(samples)$sigma)
