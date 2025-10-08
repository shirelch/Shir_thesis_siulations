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
  mu    = df$mu
  sigma = df$sigma
  tau   = df$tau
  
  
  X_matrix = as.matrix(X)
  
  # Check for constant columns
  constant_cols = apply(X_matrix, 2, function(x) var(x, na.rm = TRUE) == 0)
  if (any(constant_cols)) {
    cat("  Removing", sum(constant_cols), "constant columns:", 
        paste(names(which(constant_cols)), collapse = ", "), "\n")
    X_matrix = X_matrix[, !constant_cols, drop = FALSE]
    X = X[, !constant_cols, drop = FALSE]
  }
  
  #### TRAIN LASSO MODEL ####
  cat("  Training LASSO model...\n")
  
  # Use more folds for better estimates, especially with small samples
  nfolds = min(10, nrow(X_matrix))
  
  lasso_model = cv.glmnet(
    X_matrix, tau, 
    alpha = 1, 
    nfolds = nfolds,
    standardize = TRUE,  # Standardize predictors
    type.measure = "mse"
  )
  
  # Report LASSO performance
  min_mse_idx = which.min(lasso_model$cvm)
  cat("    Best CV MSE:", round(lasso_model$cvm[min_mse_idx], 2), "\n")
  cat("    Lambda min:", round(lasso_model$lambda.min, 6), "\n")
  cat("    Non-zero coefficients:", sum(coef(lasso_model, s = "lambda.min") != 0) - 1, "\n")
  
  save(lasso_model, file = paste0("tau_estimation_simulations/models/lasso_model_Nobs_", current_Nobs, ".RData"))
  
  #### TRAIN RANDOM FOREST MODEL ####
  cat("  Training Random Forest model...\n")
  
  # Enhanced train control
  train_control = trainControl(
    method = "cv",
    number = nfolds,
    verboseIter = FALSE,
    savePredictions = "final"
  )
  
  # Tune hyperparameters for better performance
  rf_grid = expand.grid(
    mtry = c(max(1, floor(ncol(X)/3)), max(1, floor(sqrt(ncol(X)))), max(1, floor(ncol(X)/2)))
  )
  
  # Tune hyperparameters for better performance
  rf_grid = expand.grid(
    mtry = c(max(1, floor(ncol(X)/3)), max(1, floor(sqrt(ncol(X)))), max(1, floor(ncol(X)/2)))
  )
  
  rf_model_cv = train(
    x = X,
    y = tau,
    method = "rf",
    trControl = train_control,
    tuneGrid = rf_grid,
    importance = TRUE,
    ntree = 500,  # More trees for stability
    nodesize = max(1, floor(nrow(X)/50))  # Adjust node size based on sample
  )
  
  # Report Random Forest performance
  cat("    Best CV RMSE:", round(min(rf_model_cv$results$RMSE), 2), "\n")
  cat("    Best mtry:", rf_model_cv$bestTune$mtry, "\n")
  cat("    R-squared:", round(max(rf_model_cv$results$Rsquared, na.rm = TRUE), 3), "\n")
  
  save(rf_model_cv, file = paste0("tau_estimation_simulations/models/rf_model_Nobs_", current_Nobs, ".RData"))
  
  cat("Finished training for Nobs =", current_Nobs, "\n")
}
