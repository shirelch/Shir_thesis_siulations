library(tidyverse)
load("tau_estimation_simulations/data/config.rdata")

Nobs_values = config$Nobs

for (current_Nobs in Nobs_values) {
  cat("Processing Nobs =", current_Nobs, "\n")
  
  mu    = runif(Nsubj, 0, 2000)
  sigma = runif(Nsubj, 1, 1000)
  tau   = runif(Nsubj, 0, 2000)
  
  rt_list = vector("list", Nsubj)
  for (i in 1:Nsubj) {
    rt_list[[i]] = rnorm(current_Nobs, mu[i], sigma[i]) + rexp(current_Nobs, rate = 1 / tau[i])
  }
  
  # Save all parameters and raw RTs
  save(mu, sigma, tau, rt_list, file = paste0("tau_estimation_simulations/data/sim_data_raw_Nobs_", current_Nobs, ".RData"))
  cat("Saved raw RT data for Nobs =", current_Nobs, "\n")
}
