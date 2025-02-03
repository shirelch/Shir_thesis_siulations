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
    dataset_name = sub("_simulation_results", "", dataset_name)
    dataset_name = sub("_", " ", dataset_name)
    
    df = all_results |>
      filter(id %in% c(92, 3)) |> # Use tolerance for floating-point comparison
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model, id)
    
    all_models_df = rbind(all_models_df, df)
    
  } else {
    warning(paste("Results not found in", file_path))
  }
}

quest_df = all_models_df |> filter(grepl("^quest", model, ignore.case = TRUE))
staircase_df = all_models_df |> filter(!grepl("^quest", model, ignore.case = TRUE))

threshold_line = quest_df |> 
  group_by(id, trial) |> 
  summarise(beta = unique(round(beta, 2)), t = unique(t), .groups = "drop")  # Ensure t is unique for each trial

p1 = ggplot(quest_df, aes(x = trial, y = coherence, color = as.factor(round(beta, 2)))) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_line(data = threshold_line, aes(x = trial, y = t), 
            linetype = "dashed", alpha = 0.5)  +
  labs(
    title = "Coherence Per Trial (QUEST Methods)",
    x = "Trial",
    y = "Coherence",
    color = "Beta"
  ) +
  theme_minimal()

p2 = ggplot(staircase_df, aes(x = trial, y = coherence, color = as.factor(round(beta, 2)))) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_line(data = threshold_line, aes(x = trial, y = t), 
            linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Coherence Per Trial (Staircase Methods)",
    x = "Trial",
    y = "Coherence",
    color = "Beta"
  ) +
  theme_minimal()

p2+p1

