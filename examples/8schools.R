
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

###########
## RStan ##
###########

library(rstan)

## Example 1: Eight Schools ##

# A hierarchical model is used to model the effect of coaching programs on college admissions tests. 
# The data, shown in the table below, summarize the results of experiments conducted in eight high schools, with an estimated standard error for each
#   School	Estimate (yj)	Standard Error (σj)
#   A	28	15
#   B	8	10

# Statistical model: the performance of each school j (y_j) is modelled as a normal distribution.
#   a hierarchical prior is put on the mean of the normal distribution
# y_j ~ Normal(theta_j, sigma_j)
# theta_j ~ Normal(mu,tau)
# sigma_j is assumed to be known

schools_data <- list(
  J = 8,
  y = c(28,  8, -3,  7, -1,  1, 18, 12),
  sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)


# stan function:
# - warmup specifies the number of iterations that are used by the NUTS sampler for the adaptation phase before sampling begins
# - The optional init argument can be used to specify initial values for the Markov chains
# The stan function returns a stanfit object, which is an S4 object of class "stanfit"
fit1 <- stan(
  file = "/Users/ricard/Downloads/stan/8schools.stan",  # Stan program
  data = schools_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (using 2 just for the vignette)
  refresh = 1000          # show progress every 'refresh' iterations
)

# validate results
# mu <- get_posterior_mean(fit1)[,"mean-all chains"]["mu"]
# eta1 <- get_posterior_mean(fit1)[,"mean-all chains"]["eta[1]"]
# tau <- get_posterior_mean(fit1)[,"mean-all chains"]["tau"]
# theta1 <- get_posterior_mean(fit1)[,"mean-all chains"]["theta[1]"]
# theta2 <- get_posterior_mean(fit1)[,"mean-all chains"]["theta[2]"]
# theta3 <- get_posterior_mean(fit1)[,"mean-all chains"]["theta[3]"]
# schools_data$y[1]
# schools_data$y[2]
# schools_data$y[3]

# Plot credibility intervals
plot(fit1)

# The traceplot method is used to plot the time series of the posterior draws. 
# If we include the warmup draws by setting inc_warmup=TRUE, the background color of the warmup area is different from the post-warmup phase:
traceplot(fit1, pars = c("mu", "tau"), inc_warmup = TRUE, nrow = 2)


# To assess the convergence of the Markov chains, we can calculate the split Rhat statistic.
print(fit1, pars = c("mu", "tau"))


### TRY VARIATIONAL INFERENCE ###

stan_model <- stan_model(
  file = "/Users/ricard/Downloads/stan/8schools.stan",
  model_name="vb"
)
vbmodel1 <- vb(stan_model, data = schools_data, pars = NA, algorithm="meanfield")
vbmodel2 <- vb(stan_model, data = schools_data, pars = NA, algorithm="fullrank")

# ????
get_posterior_mean(fit1)["mu","mean-all chains"]
get_posterior_mean(vbmodel1)["mu",1]
get_posterior_mean(vbmodel2)["mu",1]
