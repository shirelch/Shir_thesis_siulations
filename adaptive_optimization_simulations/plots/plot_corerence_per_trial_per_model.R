library(ggplot2)
library(dplyr)
library(patchwork)

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
      filter(id == 5) |> # Use tolerance for floating-point comparison
      mutate(model = dataset_name) |>
      select(trial, t, beta, coherence, model)
    
    all_models_df = rbind(all_models_df, df)

  } else {
    warning(paste("Results not found in", file_path))
  }
}

quest_df = all_models_df |> filter(grepl("^quest", model, ignore.case = TRUE))
staircase_df = all_models_df |> filter(!grepl("^quest", model, ignore.case = TRUE))

p1 = ggplot(quest_df, aes(x = trial, y = coherence, color = model)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_hline(data = quest_df, aes(yintercept = t), 
             linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Coherence Per Trial (QUEST Methods)",
    x = "Trial",
    y = "Coherence",
    color = "method"
  ) +
  theme_minimal()

p2 = ggplot(staircase_df, aes(x = trial, y = coherence, color = model)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_hline(data = staircase_df, aes(yintercept = t), 
             linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Coherence Per Trial (Staircase Methods)",
    x = "Trial",
    y = "Coherence",
    color = "method"
  ) +
  theme_minimal()

p2+p1
