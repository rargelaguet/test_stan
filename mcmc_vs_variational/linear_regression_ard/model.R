#####################################################
## Bayesian linear regression model with ARD prior ##
#####################################################

bayesian_linear_ard <- "
  data {
    int<lower=0> N;
    int<lower=0> D;
    matrix[N,D] X;
    vector[N] y;
  }

  parameters {
    vector[D] w;
    vector<lower=0>[D] alpha;
    real<lower=0> tau;
  }

  transformed parameters {
    vector<lower=0>[D] t_alpha;
    real<lower=0> t_tau;
    for (d in 1:D) t_alpha[d] = 1/sqrt(alpha[d]);
    t_tau =  1/sqrt(tau);
  }

  model {
    tau ~ gamma(1, 1);
    alpha ~ gamma(1e-3,1e-3);
    w ~ normal(0,  t_alpha);
    y ~ normal(X*w, t_tau);
  }
"
