library(tidyverse)
library(moments)
library(psych)
library(glmnet)
library(gamlss)
library(ggplot2)
library(dplyr)
library(ggtext)
library(randomForest)
library(caret)

source("tau_estimation_simulations/common/main_functions.R")
load("tau_estimation_simulations/data/config.rdata")

# Extract configuration
Nobs_values = config$Nobs
Nsubj = config$Nsubj

# Define metrics to analyze (consistent with other code)
metrics = c("mean_rt",
            "sd_rt", 
            "sd_by_mean_rt",
            "tau_est_mean_median",
            "exGaus_fit",
            "lasso_factor",
            "rf_factor")

# Use consistent parameter ranges from config
mu    = runif(Nsubj, config$mu_range[1], config$mu_range[2])
sigma = runif(Nsubj, config$sigma_range[1], config$sigma_range[2])
tau   = runif(Nsubj, config$tau_range[1], config$tau_range[2])

corr_results = data.frame()

for (N in Nobs_values) {
  cat("Processing Nobs =", N, "\n")
  
  # Load trained models for this N
  load(paste0("tau_estimation_simulations/models/lasso_model_Nobs_", N, ".RData"))
  load(paste0("tau_estimation_simulations/models/rf_model_Nobs_", N, ".RData"))
  
  results = data.frame()
  
  # Simulate data
  for (i in 1:Nsubj) {
    if (i %% 100 == 0) cat("  Subject", i, "\n")
    
    # Generate RT data with minimum threshold like other code
    rt_raw = rnorm(N, mu[i], sigma[i]) + rexp(N, rate = 1 / tau[i])
    rt = pmax(rt_raw, 50)  # Apply 50ms minimum threshold
    
    # Create predictors using updated function with models
    tryCatch({
      metric_vals = create_predictors_for_rt(rt)
      
      results = rbind(results,
                      cbind(
                        data.frame(
                          Nobs = N,
                          subject = i,
                          mu = mu[i],
                          sigma = sigma[i],
                          tau = tau[i]
                        ),
                        metric_vals
                      ))
    }, error = function(e) {
      cat("    Error for subject", i, ":", e$message, "\n")
      
      # Create row with NAs if prediction fails
      na_row = data.frame(
        Nobs = N,
        subject = i,
        mu = mu[i],
        sigma = sigma[i],
        tau = tau[i],
        mean_rt = NA,
        sd_rt = NA,
        sd_by_mean_rt = NA,
        tau_est_mean_median = NA,
        exGaus_fit = NA,
        lasso_factor = NA,
        rf_factor = NA
      )
      
      results = rbind(results, na_row)
    })
  }
  
  # Calculate correlations for available metrics
  available_metrics = intersect(metrics, names(results))
  
  for (metric in available_metrics) {
    if (sum(!is.na(results[[metric]])) > 10) {  # Need minimum valid observations
      r_mu = cor(results[[metric]], results$mu, use = "complete.obs")
      r_sigma = cor(results[[metric]], results$sigma, use = "complete.obs")
      r_tau = cor(results[[metric]], results$tau, use = "complete.obs")
      
      corr_results = rbind(corr_results,
                           data.frame(
                             Nobs = N,
                             Metric = metric,
                             Correlation_with_mu = round(r_mu, 3),
                             Correlation_with_sigma = round(r_sigma, 3),
                             Correlation_with_tau = round(r_tau, 3),
                             n_valid = sum(!is.na(results[[metric]]))
                           ))
    } else {
      # Not enough valid data
      corr_results = rbind(corr_results,
                           data.frame(
                             Nobs = N,
                             Metric = metric,
                             Correlation_with_mu = NA,
                             Correlation_with_sigma = NA,
                             Correlation_with_tau = NA,
                             n_valid = sum(!is.na(results[[metric]]))
                           ))
    }
  }
  
  cat("Completed Nobs =", N, "\n\n")
}

# Save correlation results
save(corr_results, file = "tau_estimation_simulations/results/correlation_with_true_parameters.rdata")

# Print summary
cat("=== CORRELATION RESULTS SUMMARY ===\n")
print(corr_results)

# Create summary table (average across all Nobs values)
correlation_summary <- corr_results %>%
  filter(!is.na(Correlation_with_tau)) %>%
  group_by(Metric) %>%
  summarise(
    avg_corr_mu = round(mean(Correlation_with_mu, na.rm = TRUE), 3),
    avg_corr_sigma = round(mean(Correlation_with_sigma, na.rm = TRUE), 3),
    avg_corr_tau = round(mean(Correlation_with_tau, na.rm = TRUE), 3),
    min_corr_tau = round(min(Correlation_with_tau, na.rm = TRUE), 3),
    max_corr_tau = round(max(Correlation_with_tau, na.rm = TRUE), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(avg_corr_tau))

cat("\n=== CORRELATION SUMMARY ACROSS ALL SAMPLE SIZES ===\n")
print(correlation_summary)

# Create visualization data
plot_data <- corr_results %>%
  filter(!is.na(Correlation_with_tau)) %>%
  pivot_longer(
    cols = c(Correlation_with_mu, Correlation_with_sigma),
    names_to = "Parameter",
    values_to = "Correlation_x"
  ) %>%
  mutate(
    Correlation_y = Correlation_with_tau,
    Parameter = case_when(
      Parameter == "Correlation_with_mu" ~ "μ (Mu)",
      Parameter == "Correlation_with_sigma" ~ "σ (Sigma)"
    ),
    Metric_clean = case_when(
      Metric == "mean_rt" ~ "Mean RT",
      Metric == "sd_rt" ~ "SD RT", 
      Metric == "sd_by_mean_rt" ~ "CV",
      Metric == "tau_est_mean_median" ~ "Mean-Median",
      Metric == "exGaus_fit" ~ "Ex-Gaussian",
      Metric == "lasso_factor" ~ "LASSO",
      Metric == "rf_factor" ~ "Random Forest",
      TRUE ~ Metric
    )
  )

# Create correlation scatter plot
correlation_plot <- ggplot(plot_data, 
                           aes(x = Correlation_x, y = Correlation_y, 
                               color = Metric_clean, shape = factor(Nobs))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ Parameter, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Correlation of τ Estimation Methods with Ex-Gaussian Parameters",
    subtitle = "Points show correlations across different sample sizes (25-200 trials)",
    x = "Correlation with μ or σ",
    y = "Correlation with τ (target parameter)",
    color = "Estimation Method",
    shape = "Sample Size"
  ) +
  xlim(-1, 1) + ylim(-1, 1) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(
    color = guide_legend(title = "Method", override.aes = list(size = 4)),
    shape = guide_legend(title = "N trials")
  )

print(correlation_plot)

# Save plot
ggsave("tau_estimation_simulations/results/correlation_scatter_plot.png", 
       correlation_plot, width = 12, height = 8, dpi = 300)

# Create heatmap of correlations with tau across sample sizes
heatmap_data <- corr_results %>%
  filter(!is.na(Correlation_with_tau)) %>%
  mutate(
    Metric_clean = case_when(
      Metric == "mean_rt" ~ "Mean RT",
      Metric == "sd_rt" ~ "SD RT", 
      Metric == "sd_by_mean_rt" ~ "CV",
      Metric == "tau_est_mean_median" ~ "Mean-Median",
      Metric == "exGaus_fit" ~ "Ex-Gaussian",
      Metric == "lasso_factor" ~ "LASSO",
      Metric == "rf_factor" ~ "Random Forest",
      TRUE ~ Metric
    )
  )

heatmap_plot <- ggplot(heatmap_data, 
                       aes(x = factor(Nobs), y = reorder(Metric_clean, Correlation_with_tau), 
                           fill = Correlation_with_tau)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation_with_tau)), 
            color = "white", size = 3, fontface = "bold") +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                       midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  labs(
    title = "Correlation with True τ Parameter Across Sample Sizes",
    x = "Number of Trials",
    y = "Estimation Method",
    fill = "Correlation\nwith τ"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 10)
  )

print(heatmap_plot)

# Save heatmap
ggsave("tau_estimation_simulations/results/correlation_heatmap.png", 
       heatmap_plot, width = 10, height = 6, dpi = 300)

cat("\n=== ANALYSIS COMPLETED ===\n")
cat("Results saved to:\n")
cat("- tau_estimation_simulations/results/correlation_with_true_parameters.rdata\n")
cat("- tau_estimation_simulations/results/correlation_scatter_plot.png\n")
cat("- tau_estimation_simulations/results/correlation_heatmap.png\n")