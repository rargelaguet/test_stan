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
io$outdir <- paste0(io$basedir,"/out"); dir.create(io$outdir)

## Options ##
opts <- list()

# Number of trials
opts$ntrials <- 25

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

fit.mcmc <- list()
fit.vb <- list()
for (i in 1:opts$ntrials) {
  
  # Create data object for Stan
  data <- list(y=Y[[i]], N=N, s=s)
  
  # Perform inference using HMC sampling
  fit.mcmc[[i]] <- sampling(st_model,  data = data, chains = 1, iter=3000)
  
  # Perform inference using ADVI
  fit.vb[[i]] <- vb(st_model,  data = data, algorithm="meanfield", tol_rel_obj=0.001)
  
}

##################
## Save results ##
##################

io$outfile <- paste0(io$outdir,"/fitted_models.rds")
saveRDS(list("MCMC"=fit.mcmc, "VB"=fit.vb), io$outfile)

stop()

###############################
## Load pre-computed results ##
###############################

# tmp <- readRDS(io$outfile)
# fit.mcmc <- tmp$MCMC
# fit.vb <- tmp$VB

################################
## Compare summary statistics ##
################################

params <- c("theta")

# MCMC: Extract summary statistics for the posterior distributions
dt.mcmc <- lapply(1:opts$ntrials, function(i) {
  lapply(params, function(j) {
    data_summary(as.data.frame(extract(fit.mcmc[[i]])[[j]])) %>% 
      as.data.table %>%
      setnames(c("mean","sd")) %>%
      .[,parameter:=paste0(j,1:.N)] %>%
      .[,inference:="MCMC"] %>%
      .[,trial:=i]
  }) %>% rbindlist
}) %>% rbindlist

# VI: Extract summary statistics for the posterior distributions
dt.vb <- lapply(1:opts$ntrials, function(i) {
  lapply(params, function(j) {
    data_summary(as.data.frame(extract(fit.vb[[i]])[[j]])) %>% 
      as.data.table %>%
      setnames(c("mean","sd")) %>%
      .[,parameter:=paste0(j,1:.N)] %>%
      .[,inference:="ADVI"] %>%
      .[,trial:=i]
  }) %>% rbindlist
}) %>% rbindlist

dt <- rbind(dt.vb, dt.mcmc) %>%
  melt(id.vars=c("parameter","inference","trial"), variable.name="estimate")

##########
## Plot ##
##########

to.plot <- dt %>% dcast(parameter+trial+estimate ~ inference)

# posterior mean
p1 <- ggscatter(to.plot[estimate=="mean"], x="ADVI", y="MCMC") +
  facet_wrap(~parameter) +
  labs(x="Posterior mean (ADVI)", y="Posterior mean (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")
p1


# posterior sd
p1 <- ggscatter(to.plot[estimate=="sd"], x="ADVI", y="MCMC") +
  facet_wrap(~parameter) +
  labs(x="Posterior sd (ADVI)", y="Posterior sd (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")
p1
