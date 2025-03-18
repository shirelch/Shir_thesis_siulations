library(dplyr)
library(tidyr)
library(stringr)

results_folder = "./adaptive_optimization_simulations/data/static_threshold_simulation_results"

results_files = list.files(path = results_folder,
                           pattern = "_trial.*_results\\.RData$",
                           full.names = TRUE)

for (Ntrain_trials in c(10, 25, 50)) {
  stats_df = data.frame()
  
  for (f in results_files) {
    load(f)
    
    base_name = basename(f)
    base_name_no_ext = str_remove(base_name, "\\.RData$")
    
    trials = as.numeric(str_extract(base_name_no_ext, "\\d+(?=_trials)"))
    
    model = str_remove(base_name_no_ext,
                       "_\\d+_trials?(?:_simulation)?_results$")
    
    subject_means = all_results |>
      group_by(id) |>
      filter(trial > (Ntrain_trials)) |>
      summarize(mean_acc_subject = mean(acc, na.rm = TRUE))
    
    mean_acc = mean(subject_means$mean_acc_subject)
    sd_acc   = sd(subject_means$mean_acc_subject)
    range_acc = range(subject_means$mean_acc_subject)
    min_acc  = range_acc[1]
    max_acc  = range_acc[2]
    
    temp_df = data.frame(
      model = model,
      trials = as.numeric(trials),
      mean_acc = mean_acc,
      sd_acc   = sd_acc,
      min_acc  = min_acc,
      max_acc  = max_acc
    )
    
    stats_df = rbind(stats_df, temp_df)
  }
  
  # Create a single string per row with mean, SD, and range
  stats_df = stats_df |>
    mutate(
      coherence_stats = sprintf(
        "Mean=%.2f, SD=%.2f, Range=[%.2f, %.2f]",
        mean_acc,
        sd_acc,
        min_acc,
        max_acc
      )
    )
  
  final_table = stats_df |>
    select(model, trials, coherence_stats) |>
    pivot_wider(names_from = trials,
                values_from = coherence_stats) |>
    arrange(model)  # optional: sort by model name
  
  write.csv(
    final_table,
    file = paste0(
      "adaptive_optimization_simulations\\plot_outputs\\coherence_stats_table_",
      Ntrain_trials,
      ".csv"
    ),
    row.names = FALSE
  )
}
