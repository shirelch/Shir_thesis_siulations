library(tidyverse)
library(moments)
library(psych)
library(glmnet)
library(pls)
library(randomForest)
library(caret)
source('functions.R')

load("tau_estimation_simulations/data/config.rdata")

#### SIMULATE DATA AND CREATE MODELS FOR DIFFERENT Nobs ----

# Define the different values of Nobs
Nobs_values = config$Nobs

# Iterate over each value of Nobs
for (current_Nobs in Nobs_values) {
  cat("Processing Nobs =", current_Nobs, "\n")
  
  # Define number of subjects
  Nsubj = config$Nsubj
  
  # Simulate parameters
  mu    = runif(Nsubj, 0, 2000)
  sigma = runif(Nsubj, 1, 1000)
  tau   = runif(Nsubj, 0, 2000)
  
  # Initialize storage for metrics
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
  
  # Simulate data for each subject
  for (i in 1:Nsubj) {
    rt          = rnorm(current_Nobs, mu[i], sigma[i]) + rexp(current_Nobs, rate = 1 / tau[i])
    rt_list[[i]] = rt  # Store raw rt values
    mean_rt[i]  = mean(rt)
    rt          = rt
    rt_derv     = diff(rt, differences = 2)
    
    # Compute derivative metrics
    rt_derv_quartiles = cut(rt_derv,
                            quantile(rt_derv, probs = seq(0, 1, 0.25), na.rm = TRUE),
                            include.lowest = TRUE)
    q1_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[1]], na.rm = TRUE)
    q2_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[2]], na.rm = TRUE)
    q3_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[3]], na.rm = TRUE)
    q4_mean[i]  = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[4]], na.rm = TRUE)
    
    mean_derv[i] = mean(abs(rt_derv))
    sd_derv[i]  = sd(abs(rt_derv))
    
    # Compute quantiles and statistics metrics
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
  
  # Create data frame for the single session
  df = data.frame(
    subject             = 1:Nsubj,
    #rt                  = rt_list,
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
  
  #### PREPARE FOR PREDICTION ----
  X = df |>  select(
    mean_rt,
    sd_rt,
    mean_derv,
    sd_derv ,
    sd_by_mean_rt,
    tau_est_mean_median,
    sd_median,
    q1_mean,
    q2_mean,
    q3_mean,
    q4_mean,
    q1,
    q2,
    q3,
    q4,
    q5,
    q6,
    q7,
    q8,
    q9
  )
  
  X     = as.matrix(X)
  mu    = df$mu
  sigma = df$sigma
  tau   = df$tau
  
  #### LASSO ----
  lasso_model = cv.glmnet(X, tau, alpha = 1) # Lasso regression
  save(lasso_model,
       file = paste0("models/lasso_model_Nobs_", current_Nobs, ".RData"))
  
  #### RANDOM FOREST ----
  train_control = trainControl(method = "cv")
  rf_model_cv = train(
    x = X,
    y = tau,
    method = "rf",
    trControl = train_control,
    importance = TRUE
  )
  save(rf_model_cv,
       file = paste0("models/rf_model_Nobs_", current_Nobs, ".RData"))
  
  cat("Finished processing Nobs =", current_Nobs, "\n")
}
