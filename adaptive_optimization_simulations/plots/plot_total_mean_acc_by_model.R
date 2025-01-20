library(ggplot2)
library(dplyr)

file_paths = list.files(
  path = "./adaptive_optimization_simulations/data/simulation_results", 
  full.names = TRUE
)

total_mean_acc = data.frame(Model = character(), MeanAccuracy = numeric(), stringsAsFactors = FALSE)

for (file_path in file_paths) {
  load(file_path)
  
  if (exists("all_results")) {
    dataset_name = tools::file_path_sans_ext(basename(file_path))
    
    mean_acc = mean(all_results$acc, na.rm = TRUE)
    dataset_name = sub("_simulation_results", "", dataset_name)
    
    total_mean_acc = rbind(
      total_mean_acc,
      data.frame(Model = dataset_name, MeanAccuracy = mean_acc)
    )
    
    rm(all_results)
  } else {
    warning(paste("Results not found in", file_path))
  }
}

p = ggplot(total_mean_acc, aes(x = Model, y = MeanAccuracy, fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(MeanAccuracy, 3)), 
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Total Mean Accuracy Per Model",
    x = "Model",
    y = "Total Mean Accuracy"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

print(p)
