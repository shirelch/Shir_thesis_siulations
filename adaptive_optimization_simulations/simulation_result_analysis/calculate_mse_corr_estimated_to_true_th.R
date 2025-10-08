library(dplyr)
library(tidyr)

file_paths = list.files(path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results",
                        full.names = TRUE)

all_models_df = data.frame()

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
    
    df = all_results |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df = rbind(all_models_df, df)
    
  } else {
    warning(paste("Results not found in", file_path))
  }
}

# Define convergence function
compute_convergence_trial = function(estimated_thresholds, trials) {
  final_estimate = tail(estimated_thresholds, 1)  # final estimate
  window_size = 5
  
  for (i in seq_along(estimated_thresholds)) {
    if (i + window_size - 1 <= length(estimated_thresholds)) {
      window <- estimated_thresholds[i:(i + window_size - 1)]
      if (all(abs(window - final_estimate) <= 5)) {
        return(trials[i])
      }
    }
  }
  return(NA)  # no convergence found
}

convergence_results = all_models_df |>
  group_by(model, t, beta) |>
  arrange(trial) |>
  summarise(
    convergence_trial = compute_convergence_trial(coherence, trial),
    final_threshold = tail(t, 1),
    mean_threshold_post_convergence = if (!is.na(convergence_trial)) {
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
      mean((coherence[trial >= convergence_trial] - t[trial >= convergence_trial]) ^
             2)
    } else {
      NA
    },
    .groups = "drop"
  )

model_mse_summary_df <- convergence_results |>
  filter(!is.na(mse)) |>
  group_by(model) |>
  summarise(
    mean_mse_converged = mean(mse, na.rm = TRUE),
    .groups = "drop"
  )

print(model_mse_summary_df)

model_threshold_correlation_df <- convergence_results |>
  filter(!is.na(estimated_threshold)) |>
  group_by(model) |>
  summarise(
    correlation_estimated_true = cor(estimated_threshold, t),
    .groups = "drop"
  )

print(model_threshold_correlation_df)

# Filter for quest plus model only
# Load only the quest plus file
quest_plus_file <- file_paths[grepl("quest_plus_100_trials_simulation_results", file_paths)]

if (length(quest_plus_file) == 1) {
  load(quest_plus_file)
  
  if (exists("all_results") && length(all_results) > 0) {
    # Extract beta and estimated_slope only from the final trial of each simulation
    quest_plus_slope_df <- all_results |>
      group_by(simulation = interaction(t, beta)) |>  # Each unique sim
      filter(trial == max(trial)) |>
      ungroup() |>
      select(beta, estimated_slope)
    
    # Compute correlation and MSE
    quest_plus_slope_metrics <- quest_plus_slope_df |>
      summarise(
        correlation_estimated_beta = cor(estimated_slope, beta, use = "complete.obs"),
        mse_estimated_beta = mean((estimated_slope - beta)^2, na.rm = TRUE)
      )
    
    print(quest_plus_slope_metrics)
  } else {
    warning("No results found in quest plus file.")
  }
} else {
  warning("quest plus file not found or more than one match.")
}

library(ggplot2)

# Filter only converged agents with non-NA estimated threshold
plot_data <- convergence_results |>
  filter(!is.na(estimated_threshold))

# Scatter plot
ggplot(plot_data, aes(x = t, y = estimated_threshold, color = model)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(
    x = "True Threshold (t)",
    y = "Estimated Threshold (post-convergence)",
    title = "True vs Estimated Thresholds (Converged Agents Only)",
    color = "Model"
  ) +
  theme_minimal()


library(ggplot2)

# Combine everything into one summary table
summary_df <- convergence_results |>
  filter(!is.na(estimated_threshold)) |>
  group_by(model) |>
  summarise(
    mean_mse = mean(mse, na.rm = TRUE),
    correlation = cor(estimated_threshold, t, use = "complete.obs"),
    convergence_rate = mean(!is.na(convergence_trial)),
    .groups = "drop"
  )

summary_long <- summary_df |>
  pivot_longer(cols = c(mean_mse, correlation, convergence_rate),
               names_to = "Metric",
               values_to = "Value")

ggplot(summary_long |> filter(Metric == "mean_mse"), aes(x = reorder(model, -Value), y = Value, fill = model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.4, size = 3.5) +
  labs(
    x = "Model",
    y = "Mean Squared Error",
    title = "Model Comparison: MSE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )
