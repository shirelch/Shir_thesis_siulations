library(dplyr)
library(tidyr)
library(ggplot2)

# === Load files ===
file_paths <- list.files(path = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results",
                         full.names = TRUE)

all_models_df <- data.frame()

for (file_path in file_paths) {
  load(file_path)
  if (exists("all_results") && length(all_results) > 0) {
    raw_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Extract d value using regex
    d_val <- sub(".*_(0\\.[0-9]+).*", "d=\\1", raw_name)
    
    # Clean up the model name
    cleaned_name <- raw_name
    cleaned_name <- gsub("_simulation_results", "", cleaned_name)
    cleaned_name <- gsub("_100_trials_results", "", cleaned_name)
    cleaned_name <- gsub("_100_trials", "", cleaned_name)
    cleaned_name <- gsub("_simulation", "", cleaned_name)
    cleaned_name <- gsub("_", " ", cleaned_name)
    cleaned_name <- gsub("decreasing threshold.*", "", cleaned_name)
    cleaned_name <- trimws(cleaned_name)
    
    # Combine model name with d value
    dataset_name <- paste(cleaned_name, d_val)
    
    df <- all_results |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df <- rbind(all_models_df, df)
  }
  else {
    warning(paste("Results not found in", file_path))
  }
}

# === Add sim_id ===
all_models_df <- all_models_df %>%
  group_by(model, beta) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(sim_id = rep(1:(n() / 100), each = 100)) %>%
  ungroup()

# === Define convergence function ===
compute_convergence_trial <- function(estimated_thresholds, trials, true_thresholds) {
  window_size <- 5
  for (i in seq_along(estimated_thresholds)) {
    if (i + window_size - 1 <= length(estimated_thresholds)) {
      window_est <- estimated_thresholds[i:(i + window_size - 1)]
      window_true <- true_thresholds[i:(i + window_size - 1)]
      if (all(abs(window_est - window_true) <= 5)) {
        return(trials[i])
      }
    }
  }
  return(NA)
}

# === Calculate convergence, MSE, estimated threshold ===
convergence_results <- all_models_df %>%
  group_by(model, beta, sim_id) %>%
  arrange(trial) %>%
  summarise(
    convergence_trial = compute_convergence_trial(coherence, trial, t),
    final_threshold = tail(t, 1),
    mean_true_threshold_post_convergence = if (!is.na(convergence_trial)) {
      mean(t[trial >= convergence_trial])
    } else {
      NA
    },
    estimated_threshold = if (!is.na(convergence_trial)) {
      mean(coherence[trial >= convergence_trial])
    } else {
      NA
    },
    mse = if (!is.na(convergence_trial)) {
      mean((coherence[trial >= convergence_trial] - t[trial >= convergence_trial])^2)
    } else {
      NA
    },
    .groups = "drop"
  )

# === Model-level summaries ===
model_mse_summary_df <- convergence_results %>%
  filter(!is.na(mse)) %>%
  group_by(model) %>%
  summarise(mean_mse_converged = mean(mse, na.rm = TRUE), .groups = "drop")

print(model_mse_summary_df)

model_threshold_correlation_df <- convergence_results %>%
  filter(!is.na(estimated_threshold) & !is.na(mean_true_threshold_post_convergence)) %>%
  group_by(model) %>%
  summarise(
    correlation_estimated_true = cor(estimated_threshold, mean_true_threshold_post_convergence, use = "complete.obs"),
    .groups = "drop"
  )

print(model_threshold_correlation_df)

# === Combined summary ===
summary_df <- convergence_results %>%
  filter(!is.na(estimated_threshold)) %>%
  group_by(model) %>%
  summarise(
    mean_mse = mean(mse, na.rm = TRUE),
    correlation = cor(estimated_threshold, mean_true_threshold_post_convergence, use = "complete.obs"),
    convergence_rate = mean(!is.na(convergence_trial)),
    .groups = "drop"
  )

# === Plot: MSE ===
summary_long_cleaned <- summary_long %>%
  filter(Metric == "mean_mse") %>%
  mutate(
    d = str_extract(model, "d\\s*=\\s*\\.?\\d+\\.?\\d*"),               # extract d value
    model_clean = str_trim(str_remove(model, "_?d\\s*=\\s*\\.?\\d+\\.?\\d*")) # remove d=... from model name
  ) %>%
  mutate(d = str_replace(d, "d\\s*=\\s*", "d = "))  # make d nicer (optional)

# Step 2: Plot with 2 columns of facets
ggplot(summary_long_cleaned,
       aes(x = reorder(model_clean, -Value), y = Value, fill = model_clean)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Value, 3)),
            vjust = -0.3, size = 3.5) +
  facet_wrap(~ d, ncol = 2, scales = "free_x") +  # 2 columns of plots
  labs(
    x = "Model",
    y = "Mean Squared Error",
    title = "Model Comparison: MSE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 10, hjust = 1),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = margin(20, 20, 20, 20)
  )

# === Optional: scatter plot true vs. estimated threshold ===
ggplot(convergence_results %>% filter(!is.na(estimated_threshold)),
       aes(x = mean_true_threshold_post_convergence, y = estimated_threshold, color = model)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "True Threshold (mean post-convergence)",
    y = "Estimated Threshold",
    title = "True vs Estimated Thresholds (Converged Agents Only)",
    color = "Model"
  ) +
  theme_minimal()
