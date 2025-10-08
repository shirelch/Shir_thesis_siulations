library(dplyr)

# Load all result files
file_paths <- list.files(
  path = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results", 
  full.names = TRUE
)

all_models_df <- data.frame()

# Read and combine all data
for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results") && length(all_results) > 0) {
    dataset_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Clean the dataset name
    dataset_name <- sub("_simulation_results", "", dataset_name)
    dataset_name <- sub("_100_trials_results", "", dataset_name)
    dataset_name <- gsub("_", " ", dataset_name)
    
    df <- all_results %>%
      mutate(model = dataset_name) %>%
      select(trial, t, beta, coherence, model)
    
    all_models_df <- bind_rows(all_models_df, df)
  } else {
    warning(paste("Results not found in", file_path))
  }
}

# ✅ Add simulation ID (assuming 100 trials per simulation)
all_models_df <- all_models_df %>%
  group_by(model, beta) %>%
  arrange(trial, .by_group = TRUE) %>%
  mutate(sim_id = rep(1:(n() / 100), each = 100)) %>%
  ungroup()

# ✅ Define convergence function
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

# ✅ Apply convergence calculation per simulation
convergence_results <- all_models_df %>%
  group_by(model, beta, sim_id) %>%
  arrange(trial) %>%
  summarise(
    convergence_trial = compute_convergence_trial(coherence, trial, t),
    final_threshold = tail(t, 1),
    mean_threshold_post_convergence = if (!is.na(convergence_trial)) {
      mean(t[trial >= convergence_trial])
    } else {
      NA
    },
    .groups = "drop"
  )

# ✅ Summarise convergence stats per model
model_summary_df <- convergence_results %>%
  group_by(model) %>%
  summarise(
    total_simulations = n(),
    non_converged = sum(is.na(convergence_trial)),
    percent_not_converged = 100 * non_converged / total_simulations,
    mean_convergence_trial = mean(convergence_trial, na.rm = TRUE),
    .groups = "drop"
  )

# ✅ Print results
print(model_summary_df)
