library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)

load("tau_estimation_simulations/data/config.rdata")
source("tau_estimation_simulations/common/main_functions.R")

Nobs_values = config$Nobs

for (current_Nobs in Nobs_values) {
  load(paste0("tau_estimation_simulations/data/sim_rt_components_", current_Nobs, ".RData")) # loads 'df'
  
  X = df |> select(
    mean_rt, sd_rt, mean_derv, sd_derv, sd_by_mean_rt, tau_est_mean_median,
    sd_median, q1_mean, q2_mean, q3_mean, q4_mean,
    q1, q2, q3, q4, q5, q6, q7, q8, q9
  )
  X     = as.matrix(X)
  mu    = df$mu
  sigma = df$sigma
  tau   = df$tau
  
  lasso_model = cv.glmnet(X, tau, alpha = 1)
  save(lasso_model, file = paste0("tau_estimation_simulations/models/lasso_model_Nobs_", current_Nobs, ".RData"))
  
  train_control = trainControl(method = "cv")
  rf_model_cv = train(
    x = X,
    y = tau,
    method = "rf",
    trControl = train_control,
    importance = TRUE
  )
  save(rf_model_cv, file = paste0("tau_estimation_simulations/models/rf_model_Nobs_", current_Nobs, ".RData"))
  
  cat("Finished training for Nobs =", current_Nobs, "\n")
}
