library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results", 
  full.names = TRUE
)

all_models_df = data.frame()

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results") && length(all_results) > 0) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    
    dataset_name = sub("_simulation_results", "", dataset_name)
    dataset_name = sub("_100_trials_results", "", dataset_name)  # <-- remove suffix
    dataset_name = gsub("_", " ", dataset_name)                  # <-- replace underscores with spaces
    
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

# Apply convergence calculation for each (model, t, beta) combination
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
    .groups = "drop"
  )

# View result
print(convergence_results)

model_summary_df <- convergence_results |>
  group_by(model) |>
  summarise(
    total_simulations = n(),
    non_converged = sum(is.na(convergence_trial)),
    percent_not_converged = 100 * non_converged / total_simulations,
    mean_convergence_trial = mean(convergence_trial, na.rm = TRUE),
    .groups = "drop"
  )

print(model_summary_df)

