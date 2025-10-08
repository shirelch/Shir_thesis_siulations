library(tidyverse)
library(moments)
library(psych)
library(glmnet)
library(randomForest)
library(gamlss)

# Load functions and configuration
source("tau_estimation_simulations/common/main_functions.R")
load("tau_estimation_simulations/data/config.rdata")

# Set seed for reproducibility

# Extract configuration
Nobs_values = config$Nobs
Nsubj = config$Nsubj
df = data.frame()

for (N in Nobs_values) {
  cat("Processing Nobs =", N, "\n")
  
  # Load trained models
  load(paste0("tau_estimation_simulations/models/lasso_model_Nobs_", N, ".RData"))
  load(paste0("tau_estimation_simulations/models/rf_model_Nobs_", N, ".RData"))
  
  #### SIMULATE DATA ----
  Nsessions = 2
  
  # Use config ranges for consistency
  mu    = runif(Nsubj, config$mu_range[1], config$mu_range[2])
  sigma = runif(Nsubj, config$sigma_range[1], config$sigma_range[2])
  tau   = runif(Nsubj, config$tau_range[1], config$tau_range[2])
  
  session_data = vector("list", Nsessions)
  
  # Simulate data for sessions
  for (s in 1:Nsessions) {
    cat("  Session", s, "\n")
    results = data.frame()
    
    for (i in 1:Nsubj) {
      if (i %% 100 == 0) cat("    Subject", i, "\n")  # Progress every 100 subjects
      
      # Generate RT data
      rt = rnorm(N, mu[i], sigma[i]) + rexp(N, rate = 1 / tau[i])
      
      # Create predictors with error handling
      tryCatch({
        metrics = create_predictors_for_rt(rt)
        
        # Make predictions using trained models
        # Prepare predictors for models
        model_predictors = metrics %>% 
          select(mean_rt, sd_rt, sd_by_mean_rt, mean_derv, sd_derv,
                 tau_est_mean_median, sd_median, q1_mean, q2_mean, q3_mean, q4_mean,
                 q1, q2, q3, q4, q5, q6, q7, q8, q9, lasso_factor, rf_factor, exGaus_fit)
        
        # Combine with subject info
        result_row = cbind(
          data.frame(
            Nobs = N,
            subject = i,
            session = s,
            mu = mu[i],
            sigma = sigma[i],
            tau = tau[i]
          ),
          metrics
        )
        
        results = rbind(results, result_row)
        
      }, error = function(e) {
        # If everything fails, create a row with NAs
        cat("    Error for subject", i, ":", e$message, "\n")
        
        # Create minimal result with NAs
        na_metrics = data.frame(
          Nobs = N,
          subject = i,
          session = s,
          mu = mu[i],
          sigma = sigma[i],
          tau = tau[i],
          mean_rt = NA,
          sd_rt = NA,
          sd_by_mean_rt = NA,
          mean_derv = NA,
          sd_derv = NA,
          tau_est_mean_median = NA,
          sd_median = NA,
          q1_mean = NA,
          q2_mean = NA,
          q3_mean = NA,
          q4_mean = NA,
          q1 = NA, q2 = NA, q3 = NA, q4 = NA, q5 = NA,
          q6 = NA, q7 = NA, q8 = NA, q9 = NA,
          exGaus_fit = NA,
          lasso_factor = NA,
          rf_factor = NA
        )
        
        results = rbind(results, na_metrics)
      })
    }
    
    session_data[[s]] = results
    cat("  Completed session", s, "with", nrow(results), "subjects\n")
  }
  
  # Combine session data
  df_this_N = bind_rows(session_data)
  df = bind_rows(df, df_this_N)
  
  cat("Completed Nobs =", N, "\n\n")
}

# Save results
save(df, file = "tau_estimation_simulations/results/simulation_estimate_test_retest.rdata")
cat("Test-retest simulation completed successfully!\n")

#### DEBUG: CHECK WHAT COLUMNS WE ACTUALLY HAVE ----
cat("=== CHECKING DATAFRAME STRUCTURE ===\n")
cat("Dataframe dimensions:", dim(df), "\n")
cat("Column names:", paste(names(df), collapse = ", "), "\n")

# Check first few rows
if (nrow(df) > 0) {
  cat("\nFirst few rows:\n")
  print(head(df, 3))
} else {
  stop("ERROR: No data was generated!")
}

#### CHECK TEST-RETEST CORRELATIONS ----
cat("\n=== CALCULATING TEST-RETEST CORRELATIONS ===\n")

# Check what columns actually exist before filtering
available_columns = names(df)
cat("Available columns:", paste(available_columns, collapse = ", "), "\n")

# Define the columns we want to check (only use those that exist)
key_columns_to_check = c("mean_rt", "tau_est_mean_median", "lasso_factor", "rf_factor")
existing_key_columns = key_columns_to_check[key_columns_to_check %in% available_columns]

cat("Key columns found:", paste(existing_key_columns, collapse = ", "), "\n")

# Remove subjects with missing data (only for columns that exist)
if (length(existing_key_columns) > 0) {
  df_clean = df
  
  # Filter out rows with missing data in key columns
  for (col in existing_key_columns) {
    df_clean = df_clean[!is.na(df_clean[[col]]), ]
  }
  
  cat("Removed", nrow(df) - nrow(df_clean), "observations with missing data\n")
} else {
  df_clean = df
  cat("No key columns to filter on\n")
}

# Define all metrics we want to analyze (only those that exist)
all_possible_metrics = c("mean_rt", "sd_rt", "sd_by_mean_rt", "mean_derv", "sd_derv",
                         "tau_est_mean_median", "sd_median", "q1_mean", "q2_mean", 
                         "q3_mean", "q4_mean", "q1", "q2", "q3", "q4", "q5", 
                         "q6", "q7", "q8", "q9", "exGaus_fit", "exgaus_fit", 
                         "lasso_factor", "rf_factor")

# Only keep metrics that actually exist in our data
metrics_to_analyze = all_possible_metrics[all_possible_metrics %in% available_columns]
cat("Metrics to analyze:", paste(metrics_to_analyze, collapse = ", "), "\n")

# Calculate correlations
if (length(metrics_to_analyze) > 0 && nrow(df_clean) > 0) {
  correlations_by_Nobs <- lapply(unique(df_clean$Nobs), function(nobs_val) {
    cat("Processing Nobs =", nobs_val, "\n")
    df_nobs <- df_clean %>% filter(Nobs == nobs_val)
    
    corrs <- sapply(metrics_to_analyze, function(metric) {
      tryCatch({
        session1 = df_nobs %>% filter(session == 1) %>% select(subject, !!sym(metric))
        session2 = df_nobs %>% filter(session == 2) %>% select(subject, !!sym(metric))
        
        merged = merge(session1, session2, by = "subject", suffixes = c("_session1", "_session2"))
        
        col1 <- paste0(metric, "_session1")
        col2 <- paste0(metric, "_session2")
        
        # Handle ex-Gaussian specially (check for convergence)
        if (metric %in% c("exgaus_fit", "exGaus_fit")) {
          non_converged <- (is.na(merged[[col1]]) | is.na(merged[[col2]]))
          percent_non_converged <- mean(non_converged, na.rm = TRUE) * 100
          cat(sprintf("  Nobs = %s: %s non-converged in %.1f%% of subjects\n", 
                      nobs_val, metric, percent_non_converged))
          
          merged <- merged[!non_converged, ]
          if (nrow(merged) < 2) return(NA)
        }
        
        # Check for sufficient variation
        if (nrow(merged) < 2 || 
            sd(merged[[col1]], na.rm = TRUE) == 0 || 
            sd(merged[[col2]], na.rm = TRUE) == 0) {
          return(NA)
        }
        
        cor(merged[[col1]], merged[[col2]], use = "complete.obs")
        
      }, error = function(e) {
        cat("  Error with metric", metric, ":", e$message, "\n")
        return(NA)
      })
    })
    
    tibble(Nobs = nobs_val, metric = names(corrs), correlation = corrs)
  }) %>% bind_rows()
  
  cat("\n=== CORRELATION RESULTS ===\n")
  print(correlations_by_Nobs)
  
  # Create summary table for key metrics (only those that exist)
  summary_metrics = intersect(c("lasso_factor", "rf_factor", "exgaus_fit", "exGaus_fit", 
                                "tau_est_mean_median", "sd_by_mean_rt"), 
                              metrics_to_analyze)
  
  if (length(summary_metrics) > 0) {
    correlation_summary = correlations_by_Nobs %>%
      filter(metric %in% summary_metrics) %>%
      pivot_wider(names_from = metric, values_from = correlation)
    
    cat("\n=== CORRELATION SUMMARY ===\n")
    print(correlation_summary)
    
    # Save correlation results
    save(correlations_by_Nobs, correlation_summary, 
         file = "tau_estimation_simulations/results/test_retest_correlations.rdata")
    
    cat("\nCorrelation results saved successfully!\n")
  } else {
    cat("No key metrics available for summary table\n")
  }
  
} else {
  cat("No data available for correlation analysis\n")
}

cat("=== Test-retest analysis completed! ===\n")