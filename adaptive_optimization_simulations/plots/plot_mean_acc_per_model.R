library(ggplot2)
library(dplyr)
library(plotly)

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
      filter(trial >= max(trial) - 49) |>
      mutate(model = dataset_name) |>
      select(trial, t, beta, acc, model)
    
    all_models_df = rbind(all_models_df, df)
    
  } else {
    warning(paste("Results not found in", file_path))
  }
}

avg_acc_df = all_models_df |>
  group_by(model, t, beta) |>
  summarize(mean_acc = mean(acc), .groups = "drop") |>
  mutate(agent = paste0("t=", t, ", beta=", beta))

p = ggplot(avg_acc_df, aes(x = mean_acc, y = model, color = agent)) +
  geom_vline(xintercept = 0.75, linetype = "dashed", color = "red") +
  geom_point(size = 3, alpha = 0.3) +
  labs(
    title = "Mean Accuracy for Each Agent (Last 50 Trials)",
    x = "Mean Accuracy",
    y = "Model"
  ) +
  xlim(0.4,1) +
  theme_minimal() +
  theme(legend.position = "none")

print(p)