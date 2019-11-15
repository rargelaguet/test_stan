# https://cran.r-project.org/web/packages/bridgesampling/vignettes/bridgesampling_example_stan.html

# Model:
# y_i ~ N(theta_i, sigma^2)   - observations
#   theta_i ~ N(mu,tau^2)     - group_specific mean
#     mu ~ N(m0,tau0^2)       - common mean
#     tau ~ InvGamma(a,b)     - common variance

# H0: mu = 0
# H1: mu != 0

### generate data ###

library(bridgesampling)
set.seed(12345)

mu <- 0
tau2 <- 0.5
sigma2 <- 1

n <- 20
theta <- rnorm(n, mu, sqrt(tau2))
y <- rnorm(n, theta, sqrt(sigma2))


### set prior parameters ###
mu0 <- 0
tau20 <- 1
alpha <- 1
beta <- 1

### models ###

# Null model
stancodeH0 <- 'data {
  int<lower=1> n; // number of observations
  vector[n] y; // observations
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma2;
  }
  parameters {
  real<lower=0> tau2;
  vector[n] theta;
  }
  model {
  target += inv_gamma_lpdf(tau2 | alpha, beta);
  target += normal_lpdf(theta | 0, sqrt(tau2));
  target += normal_lpdf(y | theta, sqrt(sigma2));
}'


# Alternative model
stancodeH1 <- 'data {
  int<lower=1> n; // number of observations
  vector[n] y; // observations
  real mu0;
  real<lower=0> tau20;
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> sigma2;
  }
  parameters {
  real mu;
  real<lower=0> tau2;
  vector[n] theta;
  }
  model {
  target += normal_lpdf(mu | mu0, sqrt(tau20));
  target += inv_gamma_lpdf(tau2 | alpha, beta);
  target += normal_lpdf(theta | mu, sqrt(tau2));
  target += normal_lpdf(y | theta, sqrt(sigma2));
}'

### compile models ###
stanmodelH0 <- stan_model(model_code = stancodeH0, model_name="stanmodel")
stanmodelH1 <- stan_model(model_code = stancodeH1, model_name="stanmodel")

### fitting models: gibbs sampling ###

stanfitH0 <- sampling(stanmodelH0, data = list(y = y, n = n, alpha = alpha, beta = beta),
                      iter = 50000, warmup = 1000, chains = 2, cores = 2)
stanfitH1 <- sampling(stanmodelH1, data = list(y = y, n = n, mu0 = mu0, tau20 = tau20, alpha = alpha, beta = beta),
                      iter = 50000, warmup = 1000, chains = 2, cores = 2)

### fitting models: variational bayes ###
stanfitH0 <- vb(stanmodelH0, data = list(y = y, n = n, alpha = alpha, beta = beta), 
                algorithm="meanfield")
stanfitH1 <- vb(stanmodelH1, data = list(y = y, n = n, mu0 = mu0, tau20 = tau20, alpha = alpha, beta = beta), 
                algorithm="meanfield")

### Computing the (Log) Marginal Likelihoods ###

## GIBBS: USE BRIDGE SAMPLER

# compute log marginal likelihood via bridge sampling for H0
H0.bridge <- bridge_sampler(stanfitH0, silent = TRUE)
print(H0.bridge)

# compute log marginal likelihood via bridge sampling for H1
H1.bridge <- bridge_sampler(stanfitH1, silent = TRUE)
print(H1.bridge)

## VARIATIONAL BAYES: USE THE ELBO?
# HOW TO GET THIS??

## Bayesian model comparison ##

# To compare the null model and the alternative model, we can compute the Bayes factor by using the bf function. 
BF01 <- bf(H0.bridge, H1.bridge)
print(BF01)
