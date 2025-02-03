library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results", 
  full.names = TRUE
)


plot_agent_results = function(results, title) {
  results = results |>
    mutate(Agent = paste0("t=", t, ", beta=", beta),
           agent_number = dense_rank(Agent),
           beta = beta)
  
  mean_acc_df = results |>
    group_by(agent_number) |>
    summarize(men_acc = mean(acc), t = mean(t), beta = mean(beta))
  
  p = ggplot(results, aes(factor = beta)) +
    geom_point(data = mean_acc_df, aes(x = t, y = men_acc, color = beta)) +
    labs(
      title = title,
      x = "Threshold",
      y = "Mean Accuracy",
      color = "Slope"
    ) +
    ylim(0.5, 1) + 
    theme_minimal()
}

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results")) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    plot_title = paste("Mean Accuracy Per Agent -", dataset_name)
    
    p = plot_agent_results(all_results, plot_title)
    print(p)
    
    rm(all_results)
  } else {
    warning(paste("Results not found in", file_path))
  }
}
