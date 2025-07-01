library(tidyverse)
library(moments)
library(psych)
library(glmnet)
library(randomForest)
library(gamlss)

source("tau_estimation_simulations/common/main_functions.R")
load("tau_estimation_simulations/data/config.rdata")

Nobs = config$Nobs
df = data.frame()

for (N in Nobs) {
  load(paste0(
    "tau_estimation_simulations/models/lasso_model_Nobs_",
    N,
    ".RData"
  ))
  load(paste0(
    "tau_estimation_simulations/models/rf_model_Nobs_",
    N,
    ".RData"
  ))

  #### SIMULATE DATA ----
  
  Nsubj         = config$Nsubj
  Nsessions     = 2
  
  mu            = runif(Nsubj, 0, 2000)
  sigma         = runif(Nsubj, 1, 1000)
  tau           = runif(Nsubj, 0, 2000)
  
  session_data  = vector("list", Nsessions)
  
  # Simulate data for sessions
  for (s in 1:Nsessions) {
    results       = data.frame()
    for (i in 1:Nsubj) {
      print(i)
      rt          = rnorm(N, mu[i], sigma[i]) + rexp(N, rate = 1 / tau[i])
      metrics     = create_predictors_for_rt(rt)
      
      results  =   rbind(results,
                         cbind(
                           data.frame(
                             Nobs = N,
                             subject = i,
                             session = s,
                             mu = mu[i],
                             sigma = sigma[i],
                             tau = tau[i]
                           ),
                           metrics
                         ))
    }
    
    session_data[[s]]  = results
    
  }
  
  # Combine session data into one data frame
  df_this_N = bind_rows(session_data)
  df = bind_rows(df, df_this_N)
}

save(df,
     file = "tau_estimation_simulations/results/simulation_estimate_test_retest.rdata")



#### CHECK TEST-RETEST CORRELATIONS ----
load("tau_estimation_simulations/results/simulation_estimate_test_retest.rdata")

correlations_by_Nobs <- lapply(unique(df$Nobs), function(nobs_val) {
  df_nobs <- df |> filter(Nobs == nobs_val)
  N_correlations = df_nobs |>
    select(
      subject,
      session,
      mu,
      sigma,
      tau,
      "mean_rt",
      "sd_rt",
      "sd_by_mean_rt",
      "mean_derv",
      "sd_derv",
      "tau_est_mean_median",
      "sd_median",
      "q1_mean",
      "q2_mean",
      "q3_mean",
      "q4_mean",
      "q1",
      "q2",
      "q3",
      "q4",
      "q5",
      "q5",
      "q6",
      "q7",
      "q8",
      "q9",
      "exGaus_fit",
      "lasso_factor",
      "rf_factor"
    )
  metrics <-
    names(N_correlations)[!names(N_correlations) %in% c("subject", "session", "mu", "sigma", "tau")]
  corrs <- sapply(metrics, function(metric) {
    session1 = N_correlations |> filter(session == 1) |> select(subject,!!sym(metric))
    session2 = N_correlations |> filter(session == 2) |> select(subject,!!sym(metric))
    merged = merge(
      session1,
      session2,
      by = "subject",
      suffixes = c("_session1", "_session2")
    )
    col1 <- paste0(metric, "_session1")
    col2 <- paste0(metric, "_session2")
    if (sd(merged[[col1]], na.rm = TRUE) == 0 ||
        sd(merged[[col2]], na.rm = TRUE) == 0)
      return(NA)
    cor(merged[[col1]], merged[[col2]], use = "complete.obs")
  })
  tibble(Nobs = nobs_val,
         metric = names(corrs),
         correlation = corrs)
}) |> bind_rows()

print(correlations_by_Nobs)

correaltion_df = data.frame(correlations_by_Nobs) |>
  filter(metric %in% c("lasso_factor", "rf_factor", "exGaus_fit", "tau_est_mean_median", "sd_by_mean_rt")) |>
  pivot_wider(names_from = metric, values_from = correlation)


#plot
df_long = df |>
  select(-c(mu, sigma, tau)) |>
  pivot_longer(
    cols = -c(subject, session, Nobs),
    names_to = "metric",
    values_to = "value"
  )

df_wide = df_long |>
  pivot_wider(names_from = session,
              values_from = value,
              names_prefix = "session")

for (n in unique(df_wide$Nobs)) {
  df_subset <- df_wide %>% filter(Nobs == n)
  
  p <-
    ggplot(df_subset, aes(x = session1, y = session2, color = metric)) +
    geom_point(alpha = 0.5) +
    facet_wrap( ~ metric, scales = 'free') +
    labs(
      x = 'Session 1',
      y = 'Session 2',
      color = 'Metric',
      title = paste("Test-Retest by Metric (Nobs =", n, ")")
    ) +
    theme_minimal()
  print(p)
  # ggsave(filename = paste0("plot_Nobs_", n, ".png"), plot = p, width = 8, height = 6)
}
