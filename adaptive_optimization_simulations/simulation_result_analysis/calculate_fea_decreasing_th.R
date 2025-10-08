library(ggplot2)
library(dplyr)
library(ggridges)
library(stringr)


# Step 1: Load and combine all simulation result files
file_paths <- list.files(
  path = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results",
  full.names = TRUE
)

all_models_df <- data.frame()

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results") && length(all_results) > 0) {
    # Extract raw file name
    raw_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Extract d value and format
    d_val <- sub(".*_(0\\.[0-9]+).*", "d=\\1", raw_name)
    
    # Clean up model name
    cleaned_name <- raw_name
    cleaned_name <- gsub("_simulation_results", "", cleaned_name)
    cleaned_name <- gsub("_100_trials_results", "", cleaned_name)
    cleaned_name <- gsub("_100_trials", "", cleaned_name)
    cleaned_name <- gsub("_simulation", "", cleaned_name)
    cleaned_name <- gsub("_", " ", cleaned_name)
    cleaned_name <- gsub("decreasing threshold.*", "", cleaned_name)
    cleaned_name <- trimws(cleaned_name)
    
    dataset_name <- paste(cleaned_name, d_val)
    
    # Ensure required columns exist
    df <- all_results |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, acc, model)
    
    all_models_df <- bind_rows(all_models_df, df)
  } else {
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
# Step 3: Compute convergence trial per simulation
convergence_info  <- all_models_df %>%
  group_by(model, beta, sim_id) %>%
  arrange(trial) |>
  summarise(
    convergence_trial = compute_convergence_trial(coherence, trial, t),
    .groups = "drop"
  )

# Step 4: Merge convergence info into full data
all_models_df <- all_models_df |>
  left_join(convergence_info, by = c("model", "sim_id", "beta"))

# Step 5: For converged agents, get accuracy of last 10 trials after convergence
final_accuracy_per_agent <- all_models_df |>
  filter(!is.na(convergence_trial), trial >= convergence_trial) |>
  group_by(model, sim_id, beta) |>
  arrange(trial) |>
  slice_tail(n = 10) |>
  summarise(
    mean_final_accuracy = mean(acc, na.rm = TRUE),
    .groups = "drop"
  )

# Step 6: Summarize across agents for each model
mean_final_accuracy_per_model <- final_accuracy_per_agent |>
  group_by(model) |>
  summarise(
    mean_accuracy_last10_converged = mean(mean_final_accuracy, na.rm = TRUE),
    sd_accuracy_last10_converged = sd(mean_final_accuracy, na.rm = TRUE),
    .groups = "drop"
  )

# Step 7: Print summary
print(mean_final_accuracy_per_model)

# Step 8: Plot FEA distribution
final_accuracy_per_agent <- final_accuracy_per_agent |>
  mutate(
    # Extract d value for faceting
    d = str_extract(model, "d=0\\.\\d+"),
    # Clean model name for y-axis labels by removing " d=0.xx" at the end
    model_clean = str_replace(model, " d=0\\.\\d+$", "")
  )

ggplot(final_accuracy_per_agent, aes(x = mean_final_accuracy, y = model_clean, fill = model, color = model)) +
  geom_density_ridges(alpha = 0.3, scale = 0.9, color = NA) +
  geom_jitter(width = 0, height = 0.15, alpha = 0.7, size = 1.5) +
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "red") +
  facet_wrap(~ d, ncol = 1, scales = "free_y") +
  labs(
    x = "Mean Accuracy (Last 10 Trials)",
    y = "Model",
    title = "Final Estimated Accuracy per Agent (Converged Agents)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 11)
  )
