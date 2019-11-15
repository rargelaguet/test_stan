require(rstan)

# Model:
#   t ~ Bernoulli(theta)
#     theta ~ Beta(1,1)

bernoulli <- "

data { 
  int<lower=1> N;
  int<lower=0,upper=1> x[N]; // HOW IS THIS DIFFERENT FROM 'vector[n] x'?
} 

parameters {
  real<lower=0,upper=1> theta;
} 

model {
//  x ~ bernoulli(theta); // WHAT IS THE DIFFERENCE WITH THE LINE BELOW?
    for (n in 1:N) 
      x[n] ~ bernoulli(theta);
  theta ~ beta(1,1);
}"


  


# Simulate binary data
N <- 100
x <- rbinom(N, 1, .4)
data <- list(x=x, N=N)

# Fit model
m <- stan_model(model_code = bernoulli)

# Generate samples from the posterior
samples <- sampling(m, data=data, iter=1000, chains=1)

# Extract results
theta <- mean(extract(samples)$theta)
