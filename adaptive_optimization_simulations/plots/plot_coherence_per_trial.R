library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/simulation_results", 
  full.names = TRUE
)


plot_agent_results = function(results, title) {
  results = results |>
    mutate(Agent = paste0("t=", t, ", beta=", beta))
  
  dashed_lines = results |>
    group_by(Agent) |>
    summarize(t = unique(t))
  
  p = ggplot(results, aes(x = trial, y = coherence, color = Agent)) +
    geom_line() + # Main lines
    geom_hline(data = dashed_lines, aes(yintercept = t, color = Agent), 
               linetype = "dashed", alpha = 0.1) +
    labs(
      title = title,
      x = "Test Trial",
      y = "Coherence",
      color = "Agent"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results")) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    plot_title = paste("Coherence as a Function of Test Trial -", dataset_name)
    
    p = plot_agent_results(all_results, plot_title)
    print(p)

        rm(all_results)
  } else {
    warning(paste("Results not found in", file_path))
  }
}
