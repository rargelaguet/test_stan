# https://www.cs.helsinki.fi/u/sakaya/tutorial/
  
require(rstan)
require(gplots)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(100)

pca <- "

data {
  int<lower=0> N;   // Number of samples
  int<lower=0> D;   // The original dimension
  int<lower=0> K;   // The latent dimension
  matrix[N, D] X;   // The data matrix
}

parameters {
  matrix[N, K] Z;            // The latent matrix
  matrix[D, K] W;            // The weight matrix
  real<lower=0> tau;         // Noise term 
  vector<lower=0>[K] alpha;  // ARD prior
}

transformed parameters{
  vector<lower=0>[K] t_alpha;   // declare standard deviation of ARD
  real<lower=0> t_tau;          // declare standard deviation of Noise
  t_alpha = inv(sqrt(alpha));   // standard deviation
  t_tau = inv(sqrt(tau));       // standard deviation of Noise
}

model {
  tau ~ gamma(1,1);			        // noise prior
  to_vector(Z) ~ normal(0,1);   // latent variables prior. WHAT IS to_vector()?? 
  alpha ~ gamma(1e-3,1e-3);			// ARD prior	
  for(k in 1:K)                 
    W[,k] ~ normal(0, t_alpha[k]); // Weights
  to_vector(X) ~ normal(to_vector(Z*W'), t_tau);  // Likelihood. WHAT IS to_vector()?? 
} "


N <- 200
D <- 20
K <- 5
Z <- matrix(rnorm(N*K,0,1),N,K)    # Latent components
tau <- 3
alpha <- rep(1,K)    # Component precisions for the two data sets

W <- matrix(0,D,K)   # The weights
for(k in 1:K)  W[,k] <- rnorm(D,0,1/sqrt(alpha[k]))
X <- Z %*% t(W) + matrix(rnorm(N*D,0,1/sqrt(tau)),N,D)   
data <- list(N = N, D = D, K = 10, X = X)

# Define model
m <- stan_model(model_code = pca)

# Fit gibbs sampling
#stan.fit.sampling <- sampling(m, data = data, chains=1, iter=1000); 
#W.sampling <- t(apply(extract(stan.fit.sampling,"W")[[1]], c(2,3), mean))
#alpha.sampling <- apply(extract(stan.fit.sampling,"alpha")[[1]], c(2), mean)
#heatmap.2(W.sampling, col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)

# Fit VB
stan.fit.vb <- vb(m, data = data, algorithm = "meanfield")
W.vb <- apply(extract(stan.fit.vb,"W")[[1]], c(2,3), mean)
alpha.vb <- apply(extract(stan.fit.vb,"alpha")[[1]], c(2), mean)
# heatmap.2(W.vb, col = bluered(70), dendrogram='none',trace='none', Rowv = FALSE, Colv = FALSE, key=FALSE)

###############################
## Fit using a real data set ##
###############################

load("http://cs.helsinki.fi/u/sakaya/tutorial/data/UML.RData")
X <- GeneExpression.HL60
N <- dim(X)[1]
D <- dim(X)[2]
K <- 5
data <- list(N = N, D = D, K = K, X = X)

m <- stan_model(model_code = pca)
stan.fit.vb.real <- vb(m, data = data, algorithm = "meanfield", iter = 5000)
alpha.vb.real <- apply(extract(stan.fit.vb.real,"alpha")[[1]], c(2), mean)
Z.vb <- apply(extract(stan.fit.vb.real,"Z")[[1]], c(2,3), mean)
W.vb <- apply(extract(stan.fit.vb.real,"W")[[1]], c(2,3), mean)
