library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/simulation_results", 
  full.names = TRUE
)


plot_agent_results = function(results, title) {
  results = results |>
    mutate(Agent = paste0("t=", t, ", beta=", beta))
  
  # Prepare a separate data frame for dashed lines (constant t values)
  dashed_lines = results |>
    group_by(Agent) |>
    summarize(t = unique(t)) # Extract unique t values for each agent
  
  p = ggplot(results, aes(x = trial, y = coherence, color = Agent)) +
    geom_line() + # Main lines
    geom_hline(data = dashed_lines, aes(yintercept = t, color = Agent), 
               linetype = "dashed", alpha = 0.1) + # Dashed lines
    labs(
      title = title,
      x = "Test Trial",
      y = "Coherence",
      color = "Agent"
    ) +
    theme_minimal() +
    theme(legend.position = "none") # Remove the legend
}

for (file_path in file_paths) {
  load(file_path)
  
  # Ensure the results data is loaded into `all_results`
  if (exists("all_results")) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    plot_title = paste("Coherence as a Function of Test Trial -", dataset_name)
    
    # Create interactive plot for the dataset
    p = plot_agent_results(all_results, plot_title)
    print(p)
    # Clear all_results for the next file
    rm(all_results)
  } else {
    warning(paste("Results not found in", file_path))
  }
}
