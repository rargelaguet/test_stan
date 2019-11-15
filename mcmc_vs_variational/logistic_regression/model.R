#######################################################
## Logistic regression model with a single covariate ##
#######################################################

# WHERE IS THE PRIOR FOR BETA?
# WHY THIS PRIOR ON ALPHA?
# logistic = "
# data {
#   int N;      // number of samples
#   int y[N];   // binary outcome for item n
#   real x[N];  // predictive feature for item n
# }
# 
# parameters {
#   real alpha;  // intercept
#   real beta;  // slope
# }
# 
# model {
#   alpha ~ normal(0,5);  // weakly informative
#   for (n in 1:N)
#     y[n] ~ bernoulli(inv_logit(alpha + beta * x[n]));
# }
# "

# WHERE IS THE NOISE?
# ADD ARD PRIOR
logistic = "
  data {
    int<lower=1> N;      // number of samples
    int<lower=1> D;      // number of covariates
    int y[N];            // binary outcome 
    matrix[N, D] X;      // covariate matrix
  }
  
  parameters {
    real alpha;  // intercept
    vector[D] beta;  // slope
  }

  model {
    alpha ~ normal(1,100);  //  uninformative
    for (d in 1:D) {
      beta[d] ~ normal(1,100);  //  uninformative
    }
    for (n in 1:N)
      y[n] ~ bernoulli(inv_logit(alpha + X[n]*beta));
  }
"
