#########################
## Logistic regression ##
#########################

# https://lingpipe-blog.com/2012/10/30/upgrading-from-beta-binomial-to-logistic-regression/

# Recall that the logistic sigmoid (inverse of the logit, or log odds function) maps (-inf,inf) to (0,1)

logistic <- "

data {
  int N;  // number of items
  int y[N];  // binary outcome for item n
  real x[N];  // predictive feature for item n
}

parameters {
  real alpha;  // intercept
  real beta;  // slope
}

model {
  alpha ~ normal(0,5);  // weakly informative
  for (n in 1:N)
    y[n] ~ bernoulli(inv_logit(alpha + beta * x[n]));
}
"