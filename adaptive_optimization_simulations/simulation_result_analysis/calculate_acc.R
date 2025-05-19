library(dplyr)

file_paths = list.files(path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results",
                        full.names = TRUE)

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
      select(trial, t, beta, coherence, acc, model)
    
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

mean_acc_df <- all_models_df %>%
  group_by(model) %>%
  summarise(mean_acc = mean(acc, na.rm = TRUE)) %>%
  arrange(desc(mean_acc))


