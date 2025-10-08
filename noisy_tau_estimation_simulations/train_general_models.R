library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(gamlss)

# Load configuration and functions
load("noisy_tau_estimation_simulations/data_general/config_general.RData")
source("tau_estimation_simulations/common/main_functions.R")

# Set seed for reproducibility
set.seed(config_general$random_seed)

cat("=== TRAINING GENERAL MODELS ===\n")

# Load training data
load("noisy_tau_estimation_simulations/data_general/training_data_general.RData")

cat("Processing", length(training_data$subjects), "training subjects...\n")

# For the first pass, we need dummy models since create_predictors_for_rt expects them
# We'll extract basic features without the model predictions first
cat("First pass: Extracting basic features without model predictions...\n")

# Create dummy models to prevent errors
lasso_model <- NULL
rf_model_cv <- NULL

# Extract basic features from training data
training_features_basic <- data.frame()

for (i in 1:length(training_data$subjects)) {
  if (i %% 100 == 0) cat("  Extracting features for subject", i, "\n")
  
  rt <- training_data$rt_data[[i]]
  
  # Skip subjects with too few trials
  if (length(rt) < 10) {
    cat("    Skipping subject", i, "(too few trials:", length(rt), ")\n")
    next
  }
  
  tryCatch({
    # Calculate features manually without model predictions
    mean_rt = mean(rt)
    sd_rt = sd(rt)
    sd_by_mean_rt = sd_rt / mean_rt
    
    sd_median = mean(abs(rt - median(rt)))
    tau_est_mean_median = mean(rt) - median(rt)
    
    # Compute derivative metrics
    rt_derv = diff(rt, differences = 2)
    if(length(rt_derv) > 3) {
      rt_derv_quartiles = cut(rt_derv,
                              quantile(rt_derv, probs = seq(0, 1, 0.25), na.rm = TRUE),
                              include.lowest = TRUE)
      q1_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[1]], na.rm = TRUE)
      q2_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[2]], na.rm = TRUE)
      q3_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[3]], na.rm = TRUE)
      q4_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[4]], na.rm = TRUE)
    } else {
      q1_mean = q2_mean = q3_mean = q4_mean = NA
    }
    mean_derv = mean(abs(rt_derv), na.rm = TRUE)
    sd_derv = sd(abs(rt_derv), na.rm = TRUE)
    
    # Compute quantiles
    q1 = quantile(rt, 0.1)
    q2 = quantile(rt, 0.2)
    q3 = quantile(rt, 0.3)
    q4 = quantile(rt, 0.4)
    q5 = quantile(rt, 0.5)
    q6 = quantile(rt, 0.6)
    q7 = quantile(rt, 0.7)
    q8 = quantile(rt, 0.8)
    q9 = quantile(rt, 0.9)
    
    # Create feature vector
    metrics = data.frame(
      mean_rt, sd_rt, mean_derv, sd_derv, sd_by_mean_rt, tau_est_mean_median,
      sd_median, q1_mean, q2_mean, q3_mean, q4_mean,
      q1, q2, q3, q4, q5, q6, q7, q8, q9
    )
    
    # Add subject information
    subject_data <- data.frame(
      subject = training_data$subjects[i],
      n_trials = training_data$n_trials[i],
      mu = training_data$mu[i],
      sigma = training_data$sigma[i],
      tau = training_data$tau[i]
    )
    
    # Combine subject info with features
    subject_features <- cbind(subject_data, metrics)
    training_features_basic <- rbind(training_features_basic, subject_features)
    
  }, error = function(e) {
    cat("    Error processing subject", i, ":", e$message, "\n")
  })
}

cat("Basic features extracted for", nrow(training_features_basic), "subjects\n")

# Remove subjects with missing features
complete_subjects <- complete.cases(training_features_basic)
training_features_clean <- training_features_basic[complete_subjects, ]

cat("After removing incomplete cases:", nrow(training_features_clean), "subjects\n")

if (nrow(training_features_clean) < 100) {
  stop("Too few subjects with complete features. Check data generation.")
}

# Prepare predictors and target
predictor_columns <- c("mean_rt", "sd_rt", "sd_by_mean_rt", "mean_derv", "sd_derv",
                       "tau_est_mean_median", "sd_median", "q1_mean", "q2_mean", 
                       "q3_mean", "q4_mean", "q1", "q2", "q3", "q4", "q5", 
                       "q6", "q7", "q8", "q9")

# Check which predictors are available
available_predictors <- predictor_columns[predictor_columns %in% names(training_features_clean)]
cat("Available predictors:", length(available_predictors), "out of", length(predictor_columns), "\n")

if (length(available_predictors) < 10) {
  stop("Too few predictors available. Check feature extraction.")
}

# Prepare training matrices
X_train <- training_features_clean[, available_predictors, drop = FALSE]
y_train <- training_features_clean$tau

# Handle any remaining NAs
X_train[is.na(X_train)] <- 0

cat("Training data prepared:\n")
cat("  Subjects:", nrow(X_train), "\n")
cat("  Predictors:", ncol(X_train), "\n")
cat("  Tau range: [", round(min(y_train)), ",", round(max(y_train)), "]\n")

#### TRAIN LASSO MODEL ####
cat("\n=== TRAINING LASSO MODEL ===\n")

lasso_model_general <- cv.glmnet(
  as.matrix(X_train), y_train,
  alpha = 1,
  nfolds = config_general$cv_folds,
  standardize = TRUE,
  type.measure = "mse"
)

# Report LASSO performance
lasso_cv_error <- min(lasso_model_general$cvm)
lasso_n_features <- sum(coef(lasso_model_general, s = "lambda.min") != 0) - 1

cat("LASSO model trained:\n")
cat("  CV MSE:", round(lasso_cv_error, 2), "\n")
cat("  Lambda min:", round(lasso_model_general$lambda.min, 6), "\n")
cat("  Selected features:", lasso_n_features, "\n")

#### TRAIN RANDOM FOREST MODEL ####
cat("\n=== TRAINING RANDOM FOREST MODEL ===\n")

# Set up cross-validation
train_control <- trainControl(
  method = "cv",
  number = config_general$cv_folds,
  verboseIter = FALSE,
  savePredictions = "final"
)

# Hyperparameter grid
rf_grid <- expand.grid(
  mtry = c(
    max(1, floor(ncol(X_train)/3)),
    max(1, floor(sqrt(ncol(X_train)))),
    max(1, floor(ncol(X_train)/2))
  )
)

cat("Testing mtry values:", paste(rf_grid$mtry, collapse = ", "), "\n")

rf_model_general <- train(
  x = X_train,
  y = y_train,
  method = "rf",
  trControl = train_control,
  tuneGrid = rf_grid,
  importance = TRUE,
  ntree = 500,
  nodesize = max(1, floor(nrow(X_train)/50))
)

# Report Random Forest performance
rf_cv_error <- min(rf_model_general$results$RMSE)^2  # Convert RMSE to MSE
rf_best_mtry <- rf_model_general$bestTune$mtry
rf_rsquared <- max(rf_model_general$results$Rsquared, na.rm = TRUE)

cat("Random Forest model trained:\n")
cat("  CV MSE:", round(rf_cv_error, 2), "\n")
cat("  Best mtry:", rf_best_mtry, "\n")
cat("  R-squared:", round(rf_rsquared, 3), "\n")

#### SAVE MODELS AND METADATA ####
cat("\n=== SAVING MODELS ===\n")

# Create directory if it doesn't exist
if (!dir.exists("noisy_tau_estimation_simulations/models_general")) {
  dir.create("noisy_tau_estimation_simulations/models_general", recursive = TRUE)
  cat("Created models_general directory\n")
}

# Save models
save(lasso_model_general, file = "noisy_tau_estimation_simulations/models_general/lasso_model_general.RData")
cat("Saved LASSO model\n")

save(rf_model_general, file = "noisy_tau_estimation_simulations/models_general/rf_model_general.RData")
cat("Saved Random Forest model\n")

# Save training metadata
training_metadata <- list(
  n_subjects = nrow(X_train),
  n_predictors = ncol(X_train),
  predictor_names = available_predictors,
  tau_range = range(y_train),
  trials_range = range(training_features_clean$n_trials),
  
  # LASSO results
  lasso_cv_mse = lasso_cv_error,
  lasso_lambda_min = lasso_model_general$lambda.min,
  lasso_n_features = lasso_n_features,
  
  # Random Forest results
  rf_cv_mse = rf_cv_error,
  rf_best_mtry = rf_best_mtry,
  rf_rsquared = rf_rsquared,
  
  # Training info
  training_date = Sys.time(),
  config = config_general
)

save(training_metadata, file = "noisy_tau_estimation_simulations/models_general/training_metadata_general.RData")
cat("Saved training metadata\n")

# Save cleaned training features for reference
save(training_features_clean, file = "noisy_tau_estimation_simulations/data_general/training_features_clean.RData")
cat("Saved training features\n")

# Verify files were saved
saved_files <- list.files("noisy_tau_estimation_simulations/models_general", full.names = TRUE)
cat("\nFiles saved in models_general:\n")
print(saved_files)

cat("\nModels and metadata saved successfully!\n")
cat("\n=== MODEL TRAINING COMPLETED ===\n")
cat("Model performance summary:\n")
cat("  LASSO CV MSE:", round(lasso_cv_error, 2), "\n")
cat("  Random Forest CV MSE:", round(rf_cv_error, 2), "\n")
cat("  Models saved to: models_general/\n")