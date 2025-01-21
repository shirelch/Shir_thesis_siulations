library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/simulation_results", 
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
      filter(t == 60, beta == 3.5) |> #cherry-pick agent
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df = rbind(all_models_df, df)

  } else {
    warning(paste("Results not found in", file_path))
  }
}

ggplot(all_models_df, aes(x = trial, y = coherence, color = model)) +
  geom_line(size = 1, alpha = 0.9) +
  geom_hline(data = all_models_df, aes(yintercept = t), 
             linetype = "dashed", alpha = 0.7) +
  labs(
    title = "Cohrence Per Trial for Every Model",
    x = "Trial",
    y = "Coherence",
    color = "Model"
  ) +
  theme_minimal()