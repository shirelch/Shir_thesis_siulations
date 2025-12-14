library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(gamlss)
library(moments)

# Load configuration and functions
load("noisy_tau_estimation_simulations/data_general/config_general.RData")
source("tau_estimation_simulations/common/main_functions.R")

# Set seed
set.seed(config_general$random_seed + 100)

cat("=== GENERAL MODEL TEST-RETEST ANALYSIS ===\n")

# Load trained models and test data
load("noisy_tau_estimation_simulations/models_general/lasso_model_general.RData")
load("noisy_tau_estimation_simulations/models_general/rf_model_general.RData")
load("noisy_tau_estimation_simulations/models_general/training_metadata_general.RData")
load("noisy_tau_estimation_simulations/data_general/test_data_general.RData")

cat("Loaded models and test data\n")
cat("Test subjects:", length(test_data$subjects), "\n")

lasso_model <- structure(
  list(model = lasso_model_general),
  class = "lasso_vector_wrapper"
)

# Define predict method that handles vectors correctly
predict.lasso_vector_wrapper <- function(object, newx, ...) {
  if (is.numeric(newx) && is.null(dim(newx))) {
    # Convert vector to 1-row matrix
    newx <- matrix(newx, nrow = 1)
  }
  predict(object$model, newx = newx, ...)
}

# RF model can use the original
rf_model_cv <- rf_model_general$finalModel

cat("Created model aliases with vector-to-matrix fix for LASSO\n")

# Check that models exist
if (!exists("lasso_model_general")) {
  stop("lasso_model_general not found. Please run train_general_models.R first.")
}
if (!exists("rf_model_general")) {
  stop("rf_model_general not found. Please run train_general_models.R first.")
}

cat("Created model aliases for compatibility with create_predictors_for_rt function\n")

# Process test data for both sessions
process_test_session <- function(session_rt_data, session_name) {
  cat("Processing", session_name, "...\n")
  
  session_results <- data.frame()
  
  for (i in 1:length(test_data$subjects)) {
    if (i %% 50 == 0) cat("  Subject", i, "\n")
    
    rt <- session_rt_data[[i]]
    
    # Skip subjects with too few trials
    if (length(rt) < 10) {
      cat("    Skipping subject", i, "(too few trials:", length(rt), ")\n")
      next
    }
    
    tryCatch({
      # Extract features using same method as training
      metrics <- create_predictors_for_rt(rt)
      
      # Get the predictions that were made by create_predictors_for_rt
      #lasso_pred <- metrics$lasso_factor
      # --- FIX: compute LASSO prediction with training-aligned predictors ---
      pred_names <- training_metadata$predictor_names
      
      # select predictors in the exact training order
      x_row <- metrics %>% dplyr::select(dplyr::all_of(pred_names))
      
      # ensure numeric matrix (glmnet requires matrix)
      x_mat <- as.matrix(x_row)
      storage.mode(x_mat) <- "double"
      
      # handle any NAs (should be rare, but keeps pipeline stable)
      x_mat[is.na(x_mat)] <- 0
      
      lasso_pred <- as.numeric(predict(lasso_model_general, newx = x_mat, s = "lambda.min"))
      
      rf_pred <- metrics$rf_factor
      exgaus_fit <- metrics$exGaus_fit
      
      # Combine results
      subject_result <- data.frame(
        subject = test_data$subjects[i],
        session = session_name,
        n_trials = length(rt),
        true_mu = test_data$mu[i],
        true_sigma = test_data$sigma[i],
        true_tau = test_data$tau[i],
        lasso_pred = lasso_pred,
        rf_pred = rf_pred,
        exgaus_fit = exgaus_fit
      )
      
      # Add basic RT metrics for comparison
      subject_result$mean_rt <- mean(rt)
      subject_result$sd_rt <- sd(rt)
      subject_result$cv_rt <- sd(rt) / mean(rt)
      subject_result$tau_est_mean_median <- mean(rt) - median(rt)
      
      session_results <- rbind(session_results, subject_result)
      
    }, error = function(e) {
      cat("    Error processing subject", i, ":", e$message, "\n")
    })
  }
  
  return(session_results)
}

# Process both sessions
session1_results <- process_test_session(test_data$session1_rt, "session1")
session2_results <- process_test_session(test_data$session2_rt, "session2")

# Combine sessions
all_results <- rbind(session1_results, session2_results)

cat("Test-retest processing completed:\n")
cat("  Session 1 subjects:", nrow(session1_results), "\n")
cat("  Session 2 subjects:", nrow(session2_results), "\n")

# Save results
save(all_results, session1_results, session2_results,
     file = "noisy_tau_estimation_simulations/results_general/test_retest_results_general.RData")

#### CALCULATE CORRELATIONS ####
cat("\n=== CALCULATING CORRELATIONS ===\n")

# Merge sessions for correlation analysis
merged_results <- merge(session1_results, session2_results, 
                        by = "subject", suffixes = c("_s1", "_s2"))

cat("Subjects with both sessions:", nrow(merged_results), "\n")

if (nrow(merged_results) > 10) {
  # Calculate test-retest correlations
  metrics_to_analyze <- c("lasso_pred", "rf_pred", "exgaus_fit", "mean_rt", 
                          "sd_rt", "cv_rt", "tau_est_mean_median")
  
  correlations <- sapply(metrics_to_analyze, function(metric) {
    col1 <- paste0(metric, "_s1")
    col2 <- paste0(metric, "_s2")
    
    if (col1 %in% names(merged_results) && col2 %in% names(merged_results)) {
      # Handle ex-Gaussian specially
      if (metric == "exgaus_fit") {
        valid_data <- merged_results[!is.na(merged_results[[col1]]) & !is.na(merged_results[[col2]]), ]
        if (nrow(valid_data) > 2) {
          non_converged_pct <- (nrow(merged_results) - nrow(valid_data)) / nrow(merged_results) * 100
          cat("  exgaus_fit non-converged:", round(non_converged_pct, 1), "%\n")
          cor(valid_data[[col1]], valid_data[[col2]], use = "complete.obs")
        } else {
          NA
        }
      } else {
        if (sd(merged_results[[col1]], na.rm = TRUE) > 0 && sd(merged_results[[col2]], na.rm = TRUE) > 0) {
          cor(merged_results[[col1]], merged_results[[col2]], use = "complete.obs")
        } else {
          NA
        }
      }
    } else {
      NA
    }
  })
  
  # Calculate correlations with true tau
  true_tau_correlations <- sapply(metrics_to_analyze, function(metric) {
    col_s1 <- paste0(metric, "_s1")
    if (col_s1 %in% names(merged_results)) {
      if (sd(merged_results[[col_s1]], na.rm = TRUE) > 0) {
        cor(merged_results[[col_s1]], merged_results$true_tau_s1, use = "complete.obs")
      } else {
        NA
      }
    } else {
      NA
    }
  })
  
  # Create summary
  correlation_summary <- data.frame(
    metric = names(correlations),
    test_retest_correlation = correlations,
    correlation_with_true_tau = true_tau_correlations,
    stringsAsFactors = FALSE
  )
  
  cat("\n=== CORRELATION RESULTS ===\n")
  print(correlation_summary)
  
  # Calculate prediction accuracy (RMSE and MAE)
  accuracy_metrics <- data.frame(
    metric = c("lasso_pred_s1", "rf_pred_s1", "tau_est_mean_median_s1"),
    rmse = c(
      sqrt(mean((merged_results$lasso_pred_s1 - merged_results$true_tau_s1)^2, na.rm = TRUE)),
      sqrt(mean((merged_results$rf_pred_s1 - merged_results$true_tau_s1)^2, na.rm = TRUE)),
      sqrt(mean((merged_results$tau_est_mean_median_s1 - merged_results$true_tau_s1)^2, na.rm = TRUE))
    ),
    mae = c(
      mean(abs(merged_results$lasso_pred_s1 - merged_results$true_tau_s1), na.rm = TRUE),
      mean(abs(merged_results$rf_pred_s1 - merged_results$true_tau_s1), na.rm = TRUE),
      mean(abs(merged_results$tau_est_mean_median_s1 - merged_results$true_tau_s1), na.rm = TRUE)
    )
  )
  
  # =========================
  # ADD: Bias / MAE / n_valid vs true tau (Session 1)
  # =========================
  cat("\n=== BIAS + MAE VS TRUE TAU (SESSION 1) ===\n")
  
  bias_mae_summary <- lapply(metrics_to_analyze, function(metric) {
    pred_col <- paste0(metric, "_s1")
    true_col <- "true_tau_s1"
    
    if (!(pred_col %in% names(merged_results))) {
      return(data.frame(metric = metric, n_valid = NA_integer_,
                        bias_mean = NA_real_, mae = NA_real_,
                        stringsAsFactors = FALSE))
    }
    
    valid_idx <- complete.cases(merged_results[[pred_col]], merged_results[[true_col]])
    n_valid <- sum(valid_idx)
    
    if (n_valid < 2) {
      return(data.frame(metric = metric, n_valid = n_valid,
                        bias_mean = NA_real_, mae = NA_real_,
                        stringsAsFactors = FALSE))
    }
    
    diff_tau <- merged_results[[pred_col]][valid_idx] - merged_results[[true_col]][valid_idx]
    
    data.frame(metric = metric,
               n_valid = n_valid,
               bias_mean = mean(diff_tau),
               mae = mean(abs(diff_tau)),
               stringsAsFactors = FALSE)
  }) %>% bind_rows()
  
  print(bias_mae_summary)
  
  # Attach to correlation_summary (so everything is in one place for reporting)
  correlation_summary <- correlation_summary %>%
    left_join(bias_mae_summary, by = "metric")
  
  # Extra: ex-Gaussian convergence counts (both sessions)
  if ("exgaus_fit" %in% metrics_to_analyze) {
    ex_s1 <- "exgaus_fit_s1"
    ex_s2 <- "exgaus_fit_s2"
    if (ex_s1 %in% names(merged_results) && ex_s2 %in% names(merged_results)) {
      conv_both <- complete.cases(merged_results[[ex_s1]], merged_results[[ex_s2]])
      n_conv_both <- sum(conv_both)
      n_total <- length(conv_both)
      nonconv_pct <- (1 - mean(conv_both)) * 100
      
      cat(sprintf("\nexgaus_fit convergence (both sessions): %d/%d converged (non-convergence = %.1f%%)\n",
                  n_conv_both, n_total, nonconv_pct))
    }
  }
  
  
  cat("\n=== PREDICTION ACCURACY ===\n")
  print(accuracy_metrics)
  
  # Save correlation results
  save(correlation_summary, accuracy_metrics, merged_results,
       file = "noisy_tau_estimation_simulations/results_general/correlation_analysis_general.RData")
  
} else {
  cat("Too few subjects for correlation analysis\n")
}

#### CREATE VISUALIZATIONS ####
cat("\n=== CREATING VISUALIZATIONS ===\n")

if (nrow(merged_results) > 10) {
  # 1. Test-retest scatter plots
  plot_data <- merged_results %>%
    select(subject, lasso_pred_s1, lasso_pred_s2, rf_pred_s1, rf_pred_s2, 
           tau_est_mean_median_s1, tau_est_mean_median_s2, true_tau_s1) %>%
    pivot_longer(cols = -c(subject, true_tau_s1), 
                 names_to = c("metric", "session"), 
                 names_pattern = "(.+)_s([12])",
                 values_to = "predicted_tau") %>%
    pivot_wider(names_from = session, values_from = predicted_tau, 
                names_prefix = "session") %>%
    filter(!is.na(session1), !is.na(session2))
  
  if (nrow(plot_data) > 0) {
    p1 <- ggplot(plot_data, aes(x = session1, y = session2)) +
      geom_point(alpha = 0.6, color = "steelblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      facet_wrap(~ metric, scales = "free", 
                 labeller = labeller(metric = c(
                   "lasso_pred" = "LASSO Prediction",
                   "rf_pred" = "Random Forest Prediction", 
                   "tau_est_mean_median" = "Mean-Median Difference"
                 ))) +
      labs(x = "Session 1", y = "Session 2",
           title = "Test-Retest Reliability of Tau Estimation Methods",
           subtitle = "General Model (Variable Trials + Noise)") +
      theme_minimal() +
      theme(strip.text = element_text(size = 10))
    
    print(p1)
    ggsave("noisy_tau_estimation_simulations/results_general/test_retest_plot_general.png", 
           p1, width = 10, height = 6, dpi = 300)
  }
  
  # 2. Prediction accuracy plot
  pred_accuracy_data <- merged_results %>%
    select(subject, true_tau_s1, lasso_pred_s1, rf_pred_s1, tau_est_mean_median_s1) %>%
    pivot_longer(cols = c(lasso_pred_s1, rf_pred_s1, tau_est_mean_median_s1),
                 names_to = "method", values_to = "predicted_tau") %>%
    mutate(method = case_when(
      method == "lasso_pred_s1" ~ "LASSO",
      method == "rf_pred_s1" ~ "Random Forest",
      method == "tau_est_mean_median_s1" ~ "Mean-Median Difference"
    )) %>%
    filter(!is.na(predicted_tau))
  
  if (nrow(pred_accuracy_data) > 0) {
    p2 <- ggplot(pred_accuracy_data, aes(x = true_tau_s1, y = predicted_tau, color = method)) +
      geom_point(alpha = 0.6) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      facet_wrap(~ method, scales = "free") +
      labs(x = "True Tau", y = "Predicted Tau",
           title = "Prediction Accuracy vs True Tau Values",
           subtitle = "General Model (Variable Trials + Noise)",
           color = "Method") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(p2)
    ggsave("noisy_tau_estimation_simulations/results_general/prediction_accuracy_plot_general.png", 
           p2, width = 10, height = 6, dpi = 300)
  }
  
  # 3. Distribution of trial numbers
  trial_dist_data <- all_results %>%
    select(subject, session, n_trials) %>%
    distinct()
  
  p3 <- ggplot(trial_dist_data, aes(x = n_trials)) +
    geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7, color = "black") +
    facet_wrap(~ session) +
    labs(x = "Number of Trials", y = "Number of Subjects",
         title = "Distribution of Trial Numbers Across Sessions",
         subtitle = paste0("Target: ", config_general$Nobs_mean, " ± ", config_general$Nobs_sd, " trials")) +
    theme_minimal()
  
  print(p3)
  ggsave("noisy_tau_estimation_simulations/results_general/trial_distribution_plot_general.png", 
         p3, width = 8, height = 6, dpi = 300)
  
  cat("Plots saved to results_general/\n")
}

cat("\n=== ANALYSIS COMPLETED ===\n")
cat("Results saved to:\n")
cat("  - test_retest_results_general.RData\n")
cat("  - correlation_analysis_general.RData\n")
cat("  - Various visualization plots\n")

# Print final summary
if (exists("correlation_summary")) {
  cat("\nFinal Summary:\n")
  cat("Test-Retest Correlations:\n")
  for (i in 1:nrow(correlation_summary)) {
    if (!is.na(correlation_summary$test_retest_correlation[i])) {
      cat("  ", correlation_summary$metric[i], ": r =", 
          round(correlation_summary$test_retest_correlation[i], 3), "\n")
    }
  }
  
  cat("\nCorrelations with True Tau:\n")
  for (i in 1:nrow(correlation_summary)) {
    if (!is.na(correlation_summary$correlation_with_true_tau[i])) {
      cat("  ", correlation_summary$metric[i], ": r =", 
          round(correlation_summary$correlation_with_true_tau[i], 3), "\n")
    }
  }
}