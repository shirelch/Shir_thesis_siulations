library(ggplot2)
library(dplyr)
library(plotly)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/static_threshold_simulation_results", 
  full.names = TRUE
)


plot_agent_results = function(results, title) {
  results = results |>
    filter(id > 20 & id < 24)|>
    mutate(Agent = paste0("t=", as.character(t), ", beta=", as.character(beta)))
  
  dashed_lines = results |>
    group_by(Agent) |>
    summarize(t = unique(t))
  
  p = ggplot(results, aes(x = trial, y = coherence, color = Agent)) +
    geom_line() + # Main lines
    geom_hline(data = dashed_lines, aes(yintercept = t, color = Agent), 
               linetype = "dashed", alpha = 0.8) +
    labs(
      title = title,
      x = "Test Trial",
      y = "Coherence",
      color = "Agent"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}

error_log <- c()  # Store names of files with errors

for (file_path in file_paths) {
  tryCatch({
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
  }, error = function(e) {
    message(paste("Error processing:", file_path))
    message("  Error message:", e$message)
    error_log <<- c(error_log, file_path)  # Save failed file name
  })
}

# Print out all files that had errors at the end
if (length(error_log) > 0) {
  message("The following files had errors and were skipped:")
  print(error_log)
}

