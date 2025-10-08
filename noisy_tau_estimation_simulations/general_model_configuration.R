# General Model Configuration for Part 2
# This model works with variable trial numbers and noisy data

config_general = list(
  # Variable trial numbers (around 150 ± 20)
  Nobs_mean = 150,
  Nobs_sd = 20,
  Nobs_min = 100,  # Minimum trials per subject
  Nobs_max = 200,  # Maximum trials per subject
  
  # Number of subjects for training and testing
  Nsubj_train = 1000,
  Nsubj_test = 500,   # Separate test set for generalization
  
  # Same parameter ranges as Part 1 for comparability
  mu_range = c(200, 2500),
  sigma_range = c(20, 800),
  tau_range = c(10, 1500),
  
  # Noise parameters for realistic data
  noise_types = c("gaussian", "outliers", "missing_trials"),
  
  # Gaussian noise added to RTs
  gaussian_noise_sd = 50,  # Additional noise SD in ms
  
  # Outlier contamination
  outlier_proportion = 0.05,  # 5% of trials are outliers
  outlier_multiplier = c(3, 5), # Outliers are 3-5x slower than expected
  
  # Missing/invalid trials (mimics real data)
  missing_proportion = 0.02,  # 2% of trials are missing/invalid
  
  # RT constraints (more realistic than Part 1)
  rt_min = 100,    # Minimum valid RT
  rt_max = 5000,   # Maximum valid RT (remove extreme outliers)
  
  # Cross-validation and model settings
  cv_folds = 10,
  random_seed = 789
)

# Create directories
if (!dir.exists("noisy_tau_estimation_simulations/data_general")) {
  dir.create("noisy_tau_estimation_simulations/data_general", recursive = TRUE)
}

if (!dir.exists("noisy_tau_estimation_simulations/models_general")) {
  dir.create("noisy_tau_estimation_simulations/models_general", recursive = TRUE)
}

if (!dir.exists("noisy_tau_estimation_simulations/results_general")) {
  dir.create("noisy_tau_estimation_simulations/results_general", recursive = TRUE)
}

save(config_general, file = "noisy_tau_estimation_simulations/data_general/config_general.RData")

cat("General model configuration saved successfully!\n")
cat("Configuration summary:\n")
cat("  Variable trials: ", config_general$Nobs_mean, "±", config_general$Nobs_sd, 
    " (range: ", config_general$Nobs_min, "-", config_general$Nobs_max, ")\n")
cat("  Training subjects:", config_general$Nsubj_train, "\n")
cat("  Test subjects:", config_general$Nsubj_test, "\n")
cat("  Noise components:", paste(config_general$noise_types, collapse = ", "), "\n")