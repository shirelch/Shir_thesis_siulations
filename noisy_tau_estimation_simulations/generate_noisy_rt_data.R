library(tidyverse)

# Load configuration
load("noisy_tau_estimation_simulations/data_general/config_general.RData")

# Set seed for reproducibility
set.seed(config_general$random_seed)

# Function to add realistic noise to RT data
add_realistic_noise <- function(rt, config) {
  original_length <- length(rt)
  
  # 1. Add Gaussian noise to all RTs
  rt_noisy <- rt + rnorm(length(rt), mean = 0, sd = config$gaussian_noise_sd)
  
  # 2. Add outliers (replace some RTs with extremely slow responses)
  n_outliers <- round(length(rt) * config$outlier_proportion)
  if (n_outliers > 0) {
    outlier_indices <- sample(length(rt_noisy), n_outliers)
    outlier_multiplier <- runif(n_outliers, config$outlier_multiplier[1], config$outlier_multiplier[2])
    rt_noisy[outlier_indices] <- rt_noisy[outlier_indices] * outlier_multiplier
  }
  
  # 3. Remove some trials (simulate missing/invalid responses)
  n_missing <- round(length(rt_noisy) * config$missing_proportion)
  if (n_missing > 0 && length(rt_noisy) > n_missing) {
    missing_indices <- sample(length(rt_noisy), n_missing)
    rt_noisy <- rt_noisy[-missing_indices]
  }
  
  # 4. Apply realistic RT constraints
  rt_noisy <- rt_noisy[rt_noisy >= config$rt_min & rt_noisy <= config$rt_max]
  
  # 5. Ensure minimum number of trials
  if (length(rt_noisy) < config$Nobs_min) {
    # If too many trials removed, be less aggressive with filtering
    rt_conservative <- rt + rnorm(original_length, 0, config$gaussian_noise_sd/2)
    rt_conservative <- rt_conservative[rt_conservative >= config$rt_min & rt_conservative <= config$rt_max]
    
    if (length(rt_conservative) >= config$Nobs_min) {
      rt_noisy <- rt_conservative
    } else {
      # Last resort: keep original with minimal noise
      rt_noisy <- pmax(rt + rnorm(original_length, 0, 20), config$rt_min)
    }
  }
  
  return(rt_noisy)
}

# Generate training data
cat("=== GENERATING TRAINING DATA ===\n")

# Generate variable trial numbers for each subject
training_trials <- round(rnorm(config_general$Nsubj_train, 
                               config_general$Nobs_mean, 
                               config_general$Nobs_sd))
training_trials <- pmax(pmin(training_trials, config_general$Nobs_max), config_general$Nobs_min)

# Generate parameters for training subjects
train_mu <- runif(config_general$Nsubj_train, config_general$mu_range[1], config_general$mu_range[2])
train_sigma <- runif(config_general$Nsubj_train, config_general$sigma_range[1], config_general$sigma_range[2])
train_tau <- runif(config_general$Nsubj_train, config_general$tau_range[1], config_general$tau_range[2])

# Store training data
training_data <- list(
  subjects = 1:config_general$Nsubj_train,
  n_trials = training_trials,
  mu = train_mu,
  sigma = train_sigma,
  tau = train_tau,
  rt_data = vector("list", config_general$Nsubj_train)
)

# Generate noisy RT data for training
cat("Generating noisy RT data for", config_general$Nsubj_train, "training subjects...\n")

for (i in 1:config_general$Nsubj_train) {
  if (i %% 100 == 0) cat("  Subject", i, "\n")
  
  # Generate clean ex-Gaussian RT data
  n_trials <- training_trials[i]
  rt_clean <- rnorm(n_trials, train_mu[i], train_sigma[i]) + rexp(n_trials, rate = 1/train_tau[i])
  
  # Add realistic noise
  rt_noisy <- add_realistic_noise(rt_clean, config_general)
  
  # Store the noisy data
  training_data$rt_data[[i]] <- rt_noisy
  
  # Update actual trial count after noise processing
  training_data$n_trials[i] <- length(rt_noisy)
}

# Save training data
save(training_data, file = "noisy_tau_estimation_simulations/data_general/training_data_general.RData")

cat("Training data generated successfully!\n")
cat("  Mean trials per subject:", round(mean(training_data$n_trials)), "\n")
cat("  Trial range:", range(training_data$n_trials), "\n")

# Generate test data (for test-retest analysis)
cat("\n=== GENERATING TEST DATA ===\n")

# Generate variable trial numbers for test subjects
test_trials <- round(rnorm(config_general$Nsubj_test, 
                           config_general$Nobs_mean, 
                           config_general$Nobs_sd))
test_trials <- pmax(pmin(test_trials, config_general$Nobs_max), config_general$Nobs_min)

# Generate parameters for test subjects (independent from training)
test_mu <- runif(config_general$Nsubj_test, config_general$mu_range[1], config_general$mu_range[2])
test_sigma <- runif(config_general$Nsubj_test, config_general$sigma_range[1], config_general$sigma_range[2])
test_tau <- runif(config_general$Nsubj_test, config_general$tau_range[1], config_general$tau_range[2])

# Store test data structure
test_data <- list(
  subjects = 1:config_general$Nsubj_test,
  n_trials = test_trials,
  mu = test_mu,
  sigma = test_sigma,
  tau = test_tau,
  session1_rt = vector("list", config_general$Nsubj_test),
  session2_rt = vector("list", config_general$Nsubj_test)
)

# Generate test-retest data (two sessions with same parameters, different noise)
cat("Generating test-retest data for", config_general$Nsubj_test, "test subjects...\n")

for (i in 1:config_general$Nsubj_test) {
  if (i %% 50 == 0) cat("  Subject", i, "\n")
  
  n_trials <- test_trials[i]
  
  # Session 1
  rt_clean_s1 <- rnorm(n_trials, test_mu[i], test_sigma[i]) + rexp(n_trials, rate = 1/test_tau[i])
  test_data$session1_rt[[i]] <- add_realistic_noise(rt_clean_s1, config_general)
  
  # Session 2 (same parameters, different random sampling and noise)
  rt_clean_s2 <- rnorm(n_trials, test_mu[i], test_sigma[i]) + rexp(n_trials, rate = 1/test_tau[i])
  test_data$session2_rt[[i]] <- add_realistic_noise(rt_clean_s2, config_general)
}

# Save test data
save(test_data, file = "noisy_tau_estimation_simulations/data_general/test_data_general.RData")

cat("Test data generated successfully!\n")
cat("  Test subjects:", config_general$Nsubj_test, "\n")
cat("  Mean trials per session:", round(mean(test_trials)), "\n")

cat("\n=== DATA GENERATION COMPLETED ===\n")
cat("Files created:\n")
cat("  - training_data_general.RData\n")
cat("  - test_data_general.RData\n")