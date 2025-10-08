library(ggplot2)
library(dplyr)
library(patchwork)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results", 
  full.names = TRUE
)

all_models_df = data.frame()

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results") && length(all_results) > 0) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    
    # Filter files by name:
    if (!grepl("0\\.10_d_d_results$", dataset_name)) {
      next  # skip this file if it does not match
    }
    
    dataset_name = sub("_simulation_results", "", dataset_name)
    dataset_name = sub("_100_trials_results", "", dataset_name)
    dataset_name = gsub("_", " ", dataset_name)
    
    # Remove suffix like "0.10 d d results"
    dataset_name = sub("\\s*\\d+(\\.\\d+)? d d results$", "", dataset_name)
    
    df = all_results |>
      filter(id == 5) |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df = rbind(all_models_df, df)
    
  } else {
    warning(paste("Results not found in", file_path))
  }
}


p1 = ggplot(all_models_df, aes(x = trial)) +
  geom_line(aes(y = coherence, color = model), linewidth = 1, alpha = 0.9) +
  geom_line(aes(y = t, color = model), linetype = "dashed", alpha = 0.7) +  # dynamic threshold line
  labs(
    title = "Coherence & Threshold Per Trial (QUEST Methods)",
    x = "Trial",
    y = "Value",
    color = "Method"
  ) +
  theme_minimal()

p1
