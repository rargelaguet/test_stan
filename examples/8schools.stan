// saved as 8schools.stan

// the data block specifies the observed variables
data {
  int<lower=0> J; // number of schools 
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J]; // s.e. of effect estimates 
}

// the parameters block specifies the unobserved variables
parameters {
  real mu;            // overall mean
  real<lower=0> tau;  // standard deviation
  real eta[J];        // standardized school-level effects
}

// we let the unstandardized school-level effects, theta, be a transformed parameter constructed by scaling the standardized effects by tau and shifting them by mu rather than directly declaring theta as a parameter
transformed parameters {
  real theta[J];
  for (j in 1:J)
    theta[j] = mu + tau * eta[j];
}

// the model block specified the underlying distributions
// normal_lpdf(mu,sd) is normal distribution with mean mu and standard deviation sd
// We have written the model in vector notation, which allows Stan to make use of more efficient algorithmic differentiation (AD). It would also be possible — but less efficient — to write the model by replacing normal_lpdf(y | theta,sigma) with a loop over the J schools
model {
  target += normal_lpdf(eta | 0, 1);
  target += normal_lpdf(y | theta, sigma);
}