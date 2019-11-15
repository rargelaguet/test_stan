library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rstan)
rstan_options(auto_write = TRUE)

# https://www.cs.helsinki.fi/u/sakaya/tutorial

######################
## Define settings  ##
######################

## I/O ##
io <- list()
io$basedir <- "/Users/ricard/stan/mcmc_vs_variational/betabinomial"
io$model <- paste0(io$basedir,"/model.R")

## Options ##
opts <- list()

# Number of cores
opts$cores <- 2
options(mc.cores = opts$cores)

# (TO-DO) Initialisation 

###############
## Load data ##
###############

# Load model
source(io$model)

# Load data set
source(paste0(io$basedir,"/load_data.R"))

# Utils
source("/Users/ricard/stan/mcmc_vs_variational/utils.R")


#########################
## Fit Bayesian models ##
#########################

# Create stan model
st_model <- stan_model(model_code = beta_binomial, model_name="beta_binomial")

# Create data object for Stan
data <- list(y=y, N=N, s = s)

# Perform inference using HMC sampling
fit.mcmc <- sampling(st_model,  data = data, chains = 1, iter=1000)

# Perform inference using ADVI
fit.vb <- vb(st_model,  data = data)


################################
## Compare summary statistics ##
################################

# theta <- mean(extract(samples)$theta)

# MCMC: Extract summary statistics for the posterior distributions
dt.mcmc <- data_summary(as.data.frame(extract(fit.mcmc)["theta"])) %>%
  setnames(c("mean","sd") ) %>%
  .[,parameter:="theta"] %>%
  .[,inference:="MCMC"]

# VI: Extract summary statistics for the posterior distributions
dt.vb <- data_summary(as.data.frame(extract(fit.vb)["theta"])) %>%
  setnames(c("mean","sd") ) %>%
  .[,parameter:="theta"] %>%
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
  