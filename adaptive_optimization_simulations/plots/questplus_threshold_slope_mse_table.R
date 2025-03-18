library(ggplot2)
library(dplyr)
library(patchwork)

load("./adaptive_optimization_simulations/data/static_threshold_simulation_results/quest_plus_simulation_results_parallel.RData")

mse_df = all_results |>
  group_by(t, beta) |>
  filter(trial %in% (max(trial) - 4):max(trial)) |>
  mutate(th_se = (mean(coherence)-t)^2,
         sl_se = (mean(estimated_slope)-beta)^2) |>
  ungroup() |>
  mutate(threshold_MSE = mean(th_se),
         slope_MSE = mean(sl_se)) |>
  summarise(threshold_MSE = mean(threshold_MSE, na.rm = TRUE),
                                           slope_MSE = mean(slope_MSE, na.rm = TRUE))