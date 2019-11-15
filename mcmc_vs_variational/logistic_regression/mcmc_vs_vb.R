library(data.table)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rstan)
rstan_options(auto_write = TRUE)



# data_summary <- function(data){
#   mean <- colMeans(data)
#   sd <- apply(data, 2, sd)
#   return(data.table("pred_mean" = mean, "pred_sd" = sd))
# }

######################
## Define settings  ##
######################

## I/O ##
io <- list()
io$basedir <- "/Users/ricard/stan/mcmc_vs_variational/logistic_regression"
io$model <- paste0(io$basedir,"/model.R")

## Options ##
opts <- list()

# Number of cores
opts$cores <- 2
# options(mc.cores = parallel::detectCores())
options(mc.cores = opts$cores)

# Initialisation for alpha (intercept) and beta (slope)
# opts$alpha <- 1
# opts$beta <- 1

###############
## Load data ##
###############

# Load model
source(io$model)

# Load data set
source(paste0(io$basedir,"/load_data.R"))

# Utils
source("/Users/ricard/stan/mcmc_vs_variational/utils.R")


##################################
## Fit Maximum Likelihood model ##
##################################

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)

summary(glm.fit)$coefficients

#########################
## Fit Bayesian models ##
#########################

# Create stan model
st_model <- stan_model(model_code = logistic, model_name="logistic_regression")

# Create data object for Stan
# dat <- list(N = nrow(X), D=ncol(X), X = X, y = as.numeric(y)-1, alpha = opts$alpha, beta = opts$beta)
dat <- list(N = nrow(X), D=ncol(X), X = X, y = as.numeric(y)-1)

# Perform inference using HMC sampling
fit.mcmc <- sampling(st_model,  data = dat, chains = 1, iter=500)

# Perform inference using ADVI
fit.vb <- vb(st_model,  data = dat)


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
  