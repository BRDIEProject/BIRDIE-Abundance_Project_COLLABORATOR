# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#                                                                              #
#                Model code to run random walk with covariate                  #
#                                                                              #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Libraries and functions
# ---------------------------------------------------------------------------- #
library(tidyverse)
library(rjags)
library(coda)
library(daymetr)
library(ecoforecastR)
source("./project_code/functions.R")
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Load data
# ---------------------------------------------------------------------------- #
# Counts
gannet <- read.csv("./project_data/sp_counts_gannets_filtered.csv")
gannet$StartDate <- as.POSIXct(gannet$StartDate, 
                               format = "%Y-%m-%d")
gannet$Date <- format(gannet$StartDate, 
                      "%Y-%m")
# Covariates
covs <- read.csv("./project_data/norm_covar.csv")

time = as.Date(gannet$StartDate)
y = gannet$Count
plot(time,
     y,
     type = 'l',
     ylab = "Gannet count",
     lwd = 2,
     log = 'y')

# Check for outliers:
# Look for outliers in data using a simple IQR test
outliers <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}
outliers(x=gannet$Count)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Specify jags model
# ---------------------------------------------------------------------------- #
model_covs <- "
model {
  
  #### Data Model
  for (t in 1:n) {
    y[t] ~ dnorm(x[t], tau_obs)
  }

  #### Process Model
  for (t in 2:n) {
    mu[t] <- x[t-1] + beta1 * cov[t]
    x[t] ~ dnorm(mu[t], tau_add)
  }

  #### Priors
  x[1] ~ dnorm(x_ic, tau_ic)
  tau_obs ~ dgamma(a_obs, r_obs)
  tau_add ~ dgamma(a_add, r_add)
  beta1 ~ dnorm(0, 0.01)
}"
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# specify data
# ---------------------------------------------------------------------------- #
data_jags <- list(
  y = log(y),
  cov = covs$norm_tmax,
  # cov = covs$norm_pred,
  # cov = covs$norm_ws,
  # cov = covs$norm_ppt,  
  n = length(y),
  x_ic = log(1000),
  tau_ic = 100,
  a_obs = 1, r_obs = 1,
  a_add = 1, r_add = 1
)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# model diagnostics
# ---------------------------------------------------------------------------- #
nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(data_jags$y,
                  length(data_jags$y),
                  replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),  ## initial guess on process precision
                    tau_obs=5/var(log(y.samp)))        ## initial guess on obs precision
}
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Create bayesian model object
# ---------------------------------------------------------------------------- #
j.model   <- jags.model(file = textConnection(model_covs),
                        data = data_jags,
                        inits = init,
                        n.chains = 3)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Run mcmc
# ---------------------------------------------------------------------------- #
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("x", "
                                               tau_add", 
                                               "tau_obs", 
                                               "beta1"),
                            n.iter = 10000)
# Plot trace and distributions
plot(jags.out)
dic.samples(j.model, 2000)
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# Plotting some more stuff
# ---------------------------------------------------------------------------- #
time.rng = c(1, length(time))       ## adjust to zoom in and out
out <- as.matrix(jags.out)         ## convert from coda to matrix  
x.cols <- grep("^x",
               colnames(out)) ## grab all columns that start with the letter x
ci <- apply(exp(out[,x.cols]),
            2,
            quantile,
            c(0.025,
              0.5,
              0.975)) ## model was fit on log scale
# Plot stuff
plot(time,
     ci[2,],
     type='n',
     ylim=range(y,na.rm=TRUE),
     ylab="Gannet Counts",
     log='y',
     xlim=time[time.rng])

# adjust x-axis label to be monthly if zoomed
if(diff(time.rng) < 100){ 
  axis.Date(1, 
            at=seq(time[time.rng[1]],
                   time[time.rng[2]],
                   by='month'), 
            format = "%Y-%m")
}

ecoforecastR::ciEnvelope(time,
                         ci[1,],
                         ci[3,],
                         col=ecoforecastR::col.alpha("lightBlue",
                                                     0.75))
points(time,y,pch="+",cex=0.5)

hist(1/sqrt(out[,1]),
     main=colnames(out)[1])
hist(1/sqrt(out[,2]),
     main=colnames(out)[2])

plot(out[,1],
     out[,2],
     pch=".",
     xlab=colnames(out)[1],
     ylab=colnames(out)[2])
cor(out[,1:2])
# END ------------------------------------------------------------------------ #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #