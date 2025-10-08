# Master Script for General Model Analysis (Part 2)
# This script runs the complete general model analysis pipeline

cat("=== TAU ESTIMATION GENERAL MODEL ANALYSIS ===\n")
cat("Part 2: Variable trials + Noisy data\n\n")

# Record start time
start_time <- Sys.time()

# Set working directory (adjust as needed)
# setwd("path/to/your/noisy_tau_estimation_simulations")

# Check if required directories exist
required_dirs <- c("tau_estimation_simulations/common", 
                   "tau_estimation_simulations/common/functions")

missing_dirs <- required_dirs[!sapply(required_dirs, dir.exists)]
if (length(missing_dirs) > 0) {
  stop("Missing required directories: ", paste(missing_dirs, collapse = ", "), 
       "\nPlease ensure the common functions from Part 1 are available.")
}

# ============================================================================
# STEP 1: CONFIGURATION
# ============================================================================
cat("STEP 1: Setting up configuration...\n")
tryCatch({
  source("./noisy_tau_estimation_simulations/general_model_configuration.R")
  cat("✓ Configuration completed\n\n")
}, error = function(e) {
  stop("ERROR in configuration: ", e$message)
})

# ============================================================================
# STEP 2: DATA GENERATION
# ============================================================================
cat("STEP 2: Generating noisy RT data...\n")
tryCatch({
  source("./noisy_tau_estimation_simulations/generate_noisy_rt_data.R")
  cat("✓ Data generation completed\n\n")
}, error = function(e) {
  stop("ERROR in data generation: ", e$message)
})

# ============================================================================
# STEP 3: MODEL TRAINING
# ============================================================================
cat("STEP 3: Training machine learning models...\n")
tryCatch({
  source("./noisy_tau_estimation_simulations/train_general_models.R")
  cat("✓ Model training completed\n\n")
}, error = function(e) {
  stop("ERROR in model training: ", e$message)
})

# ============================================================================
# STEP 4: TEST-RETEST ANALYSIS
# ============================================================================
cat("STEP 4: Running test-retest analysis...\n")
tryCatch({
  source("./noisy_tau_estimation_simulations/general_test_retest_analysis.R")
  cat("✓ Test-retest analysis completed\n\n")
}, error = function(e) {
  stop("ERROR in test-retest analysis: ", e$message)
})

# ============================================================================
# COMPLETION SUMMARY
# ============================================================================
end_time <- Sys.time()
total_time <- end_time - start_time

cat("=== GENERAL MODEL ANALYSIS COMPLETED SUCCESSFULLY! ===\n")
cat("Total execution time:", round(total_time, 2), attr(total_time, "units"), "\n")
cat("Completion time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n\n")

# Show generated files
cat("Generated files:\n")
cat("Configuration:\n")
cat("  - data_general/config_general.RData\n")
cat("Data files:\n")
cat("  - data_general/training_data_general.RData\n")
cat("  - data_general/test_data_general.RData\n")
cat("  - data_general/training_features_clean.RData\n")
cat("Model files:\n")
cat("  - models_general/lasso_model_general.RData\n")
cat("  - models_general/rf_model_general.RData\n")
cat("  - models_general/training_metadata_general.RData\n")
cat("Results files:\n")
cat("  - results_general/test_retest_results_general.RData\n")
cat("  - results_general/correlation_analysis_general.RData\n")
cat("Visualization files:\n")
cat("  - results_general/test_retest_plot_general.png\n")
cat("  - results_general/prediction_accuracy_plot_general.png\n")
cat("  - results_general/trial_distribution_plot_general.png\n")

# Load and display final results
tryCatch({
  load("noisy_tau_estimation_simulations/results_general/correlation_analysis_general.RData")
  
  cat("\n=== FINAL RESULTS SUMMARY ===\n")
  cat("Test-Retest Reliability:\n")
  reliable_metrics <- correlation_summary[!is.na(correlation_summary$test_retest_correlation), ]
  for (i in 1:nrow(reliable_metrics)) {
    cat("  ", reliable_metrics$metric[i], ": r =", 
        round(reliable_metrics$test_retest_correlation[i], 3), "\n")
  }
  
  cat("\nPrediction Accuracy (correlation with true tau):\n")
  accurate_metrics <- correlation_summary[!is.na(correlation_summary$correlation_with_true_tau), ]
  for (i in 1:nrow(accurate_metrics)) {
    cat("  ", accurate_metrics$metric[i], ": r =", 
        round(accurate_metrics$correlation_with_true_tau[i], 3), "\n")
  }
  
  cat("\nPrediction Error (RMSE):\n")
  for (i in 1:nrow(accuracy_metrics)) {
    cat("  ", accuracy_metrics$metric[i], ": RMSE =", 
        round(accuracy_metrics$rmse[i], 2), "\n")
  }
  # 
}, error = function(e) {
  cat("Could not load final results for summary\n")
})

cat("\n=== ANALYSIS READY FOR INTERPRETATION! ===\n")
cat("Compare these results with Part 1 (fixed trials, clean data) to assess:\n")
cat("1. Impact of variable trial numbers on reliability\n")
cat("2. Robustness to realistic noise and outliers\n")
cat("3. Generalizability of ML approaches\n")