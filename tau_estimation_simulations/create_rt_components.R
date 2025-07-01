library(tidyverse)

load("tau_estimation_simulations/data/config.rdata")
Nobs_values = config$Nobs
Nsubj = config$Nsubj

for (current_Nobs in Nobs_values) {
  load(paste0("tau_estimation_simulations/data/sim_data_raw_Nobs_", as.character(current_Nobs), ".RData"))
  rt_list                   = vector("list", Nsubj)
  mean_rt                   = numeric(Nsubj)
  sd_rt                     = numeric(Nsubj)
  sd_median                 = numeric(Nsubj)
  sd_by_mean_rt             = numeric(Nsubj)
  mean_derv                 = numeric(Nsubj)
  sd_derv                   = numeric(Nsubj)
  tau_est_mean_median       = numeric(Nsubj)
  q1_mean                   = numeric(Nsubj)
  q2_mean                   = numeric(Nsubj)
  q3_mean                   = numeric(Nsubj)
  q4_mean                   = numeric(Nsubj)
  q1 = q2 = q3 = q4 = q5 = q6 = q7 = q8 = q9 = numeric(Nsubj)
  
  for (i in 1:Nsubj) {
    rt          = rnorm(current_Nobs, mu[i], sigma[i]) + rexp(current_Nobs, rate = 1 / tau[i])
    rt_list[[i]] = rt
    mean_rt[i]  = mean(rt)
    rt_derv     = diff(rt, differences = 2)
    rt_derv_quartiles = cut(rt_derv,
                            quantile(rt_derv, probs = seq(0, 1, 0.25), na.rm = TRUE),
                            include.lowest = TRUE)
    q1_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[1]], na.rm = TRUE)
    q2_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[2]], na.rm = TRUE)
    q3_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[3]], na.rm = TRUE)
    q4_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[4]], na.rm = TRUE)
    mean_derv[i] = mean(abs(rt_derv))
    sd_derv[i]  = sd(abs(rt_derv))
    sd_rt[i]    = sd(rt)
    sd_median[i] = mean_diff_from_median(rt)
    tau_est_mean_median[i]  = mean(rt) - median(rt)
    q1[i]  = quantile(rt, 0.1)
    q2[i]  = quantile(rt, 0.2)
    q3[i]  = quantile(rt, 0.3)
    q4[i]  = quantile(rt, 0.4)
    q5[i]  = quantile(rt, 0.5)
    q6[i]  = quantile(rt, 0.6)
    q7[i]  = quantile(rt, 0.7)
    q8[i]  = quantile(rt, 0.8)
    q9[i]  = quantile(rt, 0.9)
  }
  
  df = data.frame(
    subject             = 1:Nsubj,
    mu                  = mu,
    sigma               = sigma,
    tau                 = tau,
    mean_rt             = mean_rt,
    sd_rt               = sd_rt,
    sd_by_mean_rt       = sd_by_mean_rt,
    mean_derv           = mean_derv,
    sd_derv             = sd_derv,
    tau_est_mean_median = tau_est_mean_median,
    sd_median           = sd_median,
    q1_mean             = q1_mean,
    q2_mean             = q2_mean,
    q3_mean             = q3_mean,
    q4_mean             = q4_mean,
    q1 = q1,
    q2 = q2,
    q3 = q3,
    q4 = q4,
    q5 = q5,
    q6 = q6,
    q7 = q7,
    q8 = q8,
    q9 = q9
  )
  
  save(df, file = paste0("tau_estimation_simulations/data/sim_rt_components_", current_Nobs, ".RData"))
  cat("Saved data for Nobs =", current_Nobs, "\n")
}
