library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rstan)
rstan_options(auto_write = TRUE)

# https://www.cs.helsinki.fi/u/sakaya/tutorial/#cca

######################
## Define settings  ##
######################

## I/O ##
io <- list()
io$basedir <- "/Users/ricard/stan/mcmc_vs_variational/factor_analysis/"
io$model <- paste0(io$basedir,"/model.stan")

## Options ##
opts <- list()

# Number of cores
opts$cores <- 2
options(mc.cores = opts$cores)

# Initialisation for parameters
# opts$alpha <- 1
# opts$beta <- 1


###############
## Load data ##
###############

# Load data set
source(paste0(io$basedir,"/load_data.R"))

# Utils
source("/Users/ricard/stan/mcmc_vs_variational/utils.R")


#########################
## Fit Bayesian models ##
#########################

# fa.model<- stan("fa.stan", 
#                 data = fa.data,
#                 chains =0, 
#                 pars=c("L","psi","sigma_psi","mu_psi","sigma_lt","mu_lt"))


# Create stan model
st_model = stan_model(
  file = io$model, 
  model_name="factor_analysis"
)

# Create data object for Stan
dat <- list(D=D, N=N, Y=Y, K=K)

# Define initialisation

initf <- function(chain_id = 1) {
  list(
    W_t = rep(0,24)+runif(1,-.1,.1),
    W_k = rep(.5,K)+runif(1,-.1,.1),
    psi = rep(.2,D)+runif(1,-.1,.1),
    mu_psi = 0.2++runif(1,-.1,.1),
    sigma_psi = 0.15+runif(1,-.1,.1),
    mu_lt = 0.0+runif(1,-.1,.1),
    sigma_lt = 0.5+runif(1,-.1,.1)
  )
}

# generate a list of lists to specify initial values
n_chains <- 1
init.values <- lapply(1:n_chains, function(i) initf(chain_id = i))


# Perform inference using HMC sampling
fit.mcmc = sampling(st_model,  data = dat, init = init.values, chains = 1, iter = 500)

# Perform inference using ADVI
fit.vb = vb(st_model,  data = dat)

# ERROR Chain 1: Exception: []: accessing element out of range. index -2147483647 out of range; expecting index to be between 1 and 24; index position = 1W_t  (in 'modelafcf2b903bb9_factor_analysis' at line 42)
stop()

################################
## Compare summary statistics ##
################################

# MCMC: Extract summary statistics for the posterior distributions
dt.mcmc <- data_summary(do.call("cbind", extract(fit.mcmc)[c("alpha","beta")])) %>%
  setnames(c("mean","sd") ) %>%
  .[,parameter:=c("alpha",paste0("beta",1:ncol(X)))] %>%
  .[,inference:="MCMC"]

# VI: Extract summary statistics for the posterior distributions
dt.vb <- data_summary(do.call("cbind", extract(fit.vb)[c("alpha","beta")])) %>%
  setnames(c("mean","sd") ) %>%
  .[,parameter:=c("alpha",paste0("beta",1:ncol(X)))] %>%
  .[,inference:="ADVI"]

to.plot <- rbind(dt.vb, dt.mcmc) %>%
  melt(id.vars=c("parameter","inference"), variable.name="estimate")

stop()


###############
## STOP HERE ##
###############

# mu
p1 <- ggscatter(to.plot, x="mu.mean.variational", y="mu.mean.mcmc") +
  labs(x="mean of mu (variational)", y="mean of mu (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")

p2 <- ggscatter(out, x="mu.sd.variational", y="mu.sd.mcmc") +
  labs(x="sd of mu (variational)", y="sd of mu (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")

# gamma
p3 <- ggscatter(out, x="gamma.mean.variational", y="gamma.mean.mcmc") +
  labs(x="mean of gamma (variational)", y="mean of gamma (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")

p4 <- ggscatter(out, x="gamma.sd.variational", y="gamma.sd.mcmc") +
  labs(x="sd of gamma (variational)", y="sd of gamma (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")

cowplot::plot_grid(plotlist=list(p1,p2,p3,p4), nrow=2)

###########################
## Compare distributions ##
###########################

# mu

variational <- rstan::extract(stan.models[[i]][["variational"]])[["mu"]] %>%
  as.data.frame %>% tibble::rownames_to_column("iteration") %>%
  as.data.table %>% melt(id.vars="iteration", variable.name="id") %>%
  .[,inference:="variational"]

mcmc <- rstan::extract(stan.models[[i]][["mcmc"]])[["mu"]] %>%
  as.data.frame %>% tibble::rownames_to_column("iteration") %>%
  as.data.table %>% melt(id.vars="iteration", variable.name="id") %>%
  .[,inference:="mcmc"]

to.plot <- rbind(variational,mcmc) %>%
  .[iteration>10] %>%
  .[id%in%paste0("V",1:6)]

ggdensity(to.plot, x="value", y="..density..", fill="inference", color="black") +
  labs(x="", y="Density") +
  facet_wrap(~id, scales="free")


# gamma

variational <- rstan::extract(stan.models[[i]][["variational"]])[["gamma"]] %>%
  as.data.frame %>% tibble::rownames_to_column("iteration") %>%
  as.data.table %>% melt(id.vars="iteration", variable.name="id") %>%
  .[,inference:="variational"]

mcmc <- rstan::extract(stan.models[[i]][["mcmc"]])[["gamma"]] %>%
  as.data.frame %>% tibble::rownames_to_column("iteration") %>%
  as.data.table %>% melt(id.vars="iteration", variable.name="id") %>%
  .[,inference:="mcmc"]

to.plot <- rbind(variational,mcmc) %>%
  .[iteration>10] %>%
  .[id%in%paste0("V",1:6)]

ggdensity(to.plot, x="value", y="..density..", fill="inference", color="black") +
  labs(x="gamma", y="Density") +
  facet_wrap(~id, scales="free")
  