library(ggplot2)
library(dplyr)

# Step 1: Load and combine all simulation result files
file_paths <- list.files(
  path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results",
  full.names = TRUE
)

all_models_df <- data.frame()

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results") && length(all_results) > 0) {
    # Clean up model name from filename
    dataset_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Clean out common suffixes and underscores
    dataset_name <- trimws(
      gsub("_", " ",
           gsub("_simulation", "",
                gsub("_100_trials", "",
                     gsub("_100_trials_results", "",
                          gsub("_simulation_results", "", dataset_name)
                     )
                )
           )
      )
    )
    
    
    # Add model name and keep relevant columns
    df <- all_results |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, acc, model)  # make sure 'acc' exists
    
    all_models_df <- bind_rows(all_models_df, df)
  } else {
    warning(paste("Results not found in", file_path))
  }
}

# Step 2: Define convergence detection function
compute_convergence_trial <- function(estimated_thresholds, trials) {
  final_estimate <- tail(estimated_thresholds, 1)
  window_size <- 5
  
  for (i in seq_along(estimated_thresholds)) {
    if (i + window_size - 1 <= length(estimated_thresholds)) {
      window <- estimated_thresholds[i:(i + window_size - 1)]
      if (all(abs(window - final_estimate) <= 5)) {
        return(trials[i])
      }
    }
  }
  return(NA)
}

# Step 3: Compute convergence trial per simulation
convergence_info <- all_models_df |>
  group_by(model, t, beta) |>
  arrange(trial) |>
  summarise(
    convergence_trial = compute_convergence_trial(coherence, trial),
    .groups = "drop"
  )

# Step 4: Merge convergence info into full data
all_models_df <- all_models_df |>
  left_join(convergence_info, by = c("model", "t", "beta"))

# Step 5: For converged agents, get accuracy of last 10 trials after convergence
final_accuracy_per_agent <- all_models_df |>
  filter(!is.na(convergence_trial), trial >= convergence_trial) |>
  group_by(model, t, beta) |>
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

# Step 7: Print result
print(mean_final_accuracy_per_model)


library(ggridges)

ggplot(final_accuracy_per_agent, aes(x = mean_final_accuracy, y = model, fill = model)) +
  geom_density_ridges(alpha = 0.3, scale = 0.9, color = NA) +
  geom_jitter(aes(color = model), width = 0, height = 0.15, alpha = 0.7, size = 1.5) +
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "red") +
  labs(
    x = "Mean Accuracy (Last 10 Trials)",
    y = "Model",
    title = "Final Estimated Accuracy per Agent (Converged Agents)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
