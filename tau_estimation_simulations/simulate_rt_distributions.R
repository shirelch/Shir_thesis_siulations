library(tidyverse)
load("tau_estimation_simulations/data/config.rdata")

Nobs_values = config$Nobs
Nsubj = config$Nsubj

for (current_Nobs in Nobs_values) {
  cat("Processing Nobs =", current_Nobs, "\n")
  
  mu    = runif(Nsubj, config$mu_range[1], config$mu_range[2])
  sigma = runif(Nsubj, config$sigma_range[1], config$sigma_range[2])
  tau   = runif(Nsubj, config$tau_range[1], config$tau_range[2])
  
  rt_list = vector("list", Nsubj)
  for (i in 1:Nsubj) {
    rt_raw = rnorm(current_Nobs, mu[i], sigma[i]) + rexp(current_Nobs, rate = 1 / tau[i])

  }
  
  all_rts = unlist(rt_list)
  mean_rts_by_subject = sapply(rt_list, mean)
  
  cat("  Generated RT statistics:\n")
  cat("    Overall RT range: [", round(min(all_rts)), ",", round(max(all_rts)), "] ms\n")
  cat("    Mean RT across all: ", round(mean(all_rts)), " ms\n")
  cat("    Subject mean RT range: [", round(min(mean_rts_by_subject)), ",", round(max(mean_rts_by_subject)), "] ms\n")
  cat("    Parameter ranges used:\n")
  cat("      mu: [", round(min(mu)), ",", round(max(mu)), "] ms\n")
  cat("      sigma: [", round(min(sigma)), ",", round(max(sigma)), "] ms\n") 
  cat("      tau: [", round(min(tau)), ",", round(max(tau)), "] ms\n")
  
  # Save all parameters and raw RTs
  save(mu, sigma, tau, rt_list, 
       file = paste0("tau_estimation_simulations/data/sim_data_raw_Nobs_", current_Nobs, ".RData"))
  cat("  ✓ Saved raw RT data for Nobs =", current_Nobs, "\n\n")
}

cat("=== Data generation completed successfully! ===\n")