data {
  int<lower=1> N;                // number of 
  int<lower=1> D;                // number of 
  matrix[N,D] Y;                 // data matrix of order [N,D]
  int<lower=1> K;              // number of latent dimensions 
}
transformed data {
  int<lower=1> M;
  vector[D] mu;
  M  = K*(D-K)+ K*(K-1)/2;  // number of non-zero loadings
  mu = rep_vector(0.0,D);
}

parameters {    
  vector[M] W_t;            // lower diagonal elements of W
  vector<lower=0>[K] W_k;   // lower diagonal elements of W
  vector<lower=0>[D] psi;   // vector of variances
  real<lower=0>   mu_psi;   // mean of prior on the variance
  real<lower=0>  sigma_psi; // variance of prior on the variance
  real   mu_lt;             // mean of prior on the weights
  real<lower=0>  sigma_lt;  // variance of prior on the weights
}

transformed parameters{
  cholesky_factor_cov[D,K] W;  //lower triangular factor loadings Matrix 
  cov_matrix[D] Q;   //Covariance mat
  {
  int idx1;
  int idx2;
  real zero; 
  zero = 0;
  for(i in 1:D){
    for(j in (i+1):K){
      idx1 = idx1 + 1;
      W[i,j] = zero; //constrain the upper triangular elements to zero 
    }
  }
  for (j in 1:K) {
      W[j,j] = W_k[j];
    for (i in (j+1):D) {
      idx2 = idx2 + 1;
      W[i,j] = W_t[idx2];
    } 
  }
  } 
  Q = W*W'+diag_matrix(psi); 
}

model {

// the hyperpriors 
   mu_psi ~ cauchy(0, 1);
   sigma_psi ~ cauchy(0,1);
   mu_lt ~ cauchy(0, 1);
   sigma_lt ~ cauchy(0,1);

// the priors 
  W_k ~ cauchy(0,3);
  W_t ~ cauchy(mu_lt,sigma_lt);
  psi ~ cauchy(mu_psi,sigma_psi);

// The likelihood
for( j in 1:N)
    Y[j] ~ multi_normal(mu,Q); 
}
