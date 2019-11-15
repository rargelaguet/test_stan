############################################
## Multivaritae logistic regression model ##
############################################

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
io$basedir <- "/Users/ricard/stan/mcmc_vs_variational/logistic_regression"
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
st_model <- stan_model(model_code = bayesian_logistic_ard, model_name="bayesian_logistic_ard")

fit.mcmc <- list()
fit.vb <- list()
for (i in 1:opts$ntrials) {
  print(sprintf("Fitting trial %d...",i))

  # Create data object for Stan
  data <- list(N=N, D=D, X=X[[i]], y=Y[[i]])

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

params <- c("w")

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

to.plot <- rbind(dt.vb, dt.mcmc) %>%
  melt(id.vars=c("parameter","inference","trial"), variable.name="estimate") %>%
  dcast(parameter+trial+estimate ~ inference)

# plot comparison of posterior means
p1 <- ggscatter(to.plot[estimate=="mean"], x="ADVI", y="MCMC") +
  facet_wrap(~parameter) +
  labs(x="Posterior mean (ADVI)", y="Posterior mean (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")
p1

# plot comparison of posterior sd
p1 <- ggscatter(to.plot[estimate=="sd"], x="ADVI", y="MCMC") +
  facet_wrap(~parameter) +
  labs(x="Posterior sd (ADVI)", y="Posterior sd (MCMC)") +
  geom_abline(slope=1, intercept=0, linetype="solid")
p1

# cowplot::plot_grid(plotlist=list(p1,p2,p3,p4), nrow=2)

stop()








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
  