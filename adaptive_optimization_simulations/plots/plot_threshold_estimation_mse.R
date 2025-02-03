library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results", 
  full.names = TRUE
)

all_models_df = data.frame()

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results")) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    dataset_name = sub("_simulation_results", "", dataset_name)
    dataset_name = sub("_", " ", dataset_name)
    
    df = all_results |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df = rbind(all_models_df, df)
    
  } else {
    warning(paste("Results not found in", file_path))
  }
}

mse_df = all_models_df |>
  group_by(model, t, beta) |>
  filter(trial %in% (max(trial) - 4):max(trial)) |>
  mutate(se = (mean(coherence)-t)^2) |>
  ungroup() |>
  group_by(model) |>
  mutate(MSE = mean(se)) |>
  ungroup()

# 4. Plot a bar chart of MSE for each model
ggplot(mse_df, aes(x = model, y = MSE, fill = model)) +
  geom_text(aes(label = round(MSE, 3)), 
            vjust = -0.3,
            size = 3) +
  geom_col(position = "identity") +
  labs(
    title = "MSE in Last 5 Trials",
    x = "Model",
    y = "Mean Squared Error"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
