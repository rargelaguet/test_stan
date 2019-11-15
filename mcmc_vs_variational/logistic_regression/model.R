############################################
## Multivaritae logistic regression model ##
############################################

bayesian_logistic_ard = "
  data {
    int<lower=1> N;      // number of samples
    int<lower=1> D;      // number of covariates
    int<lower=0,upper=1> y[N];            // binary outcome 
    matrix[N, D] X;      // covariate matrix
  }
  
  parameters {
		vector[D] w;
		vector<lower=0>[D] alpha;
  }
  
	transformed parameters {
		vector<lower=0>[D] t_alpha;
		for (d in 1:D) t_alpha[d] = 1/sqrt(alpha[d]);
	}
	
  model {
		alpha ~ gamma(1e-3,1e-3);
		w ~ normal(0, t_alpha);
		y ~ bernoulli_logit(X*w);
  }
"