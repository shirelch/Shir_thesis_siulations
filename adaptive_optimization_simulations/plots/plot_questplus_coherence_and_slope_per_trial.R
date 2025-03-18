library(ggplot2)
library(dplyr)
library(patchwork)

load(
  "C:\\lab-code\\Shir_thesis_siulations\\adaptive_optimization_simulations\\data\\static_threshold_simulation_results\\quest_plus_simulation_results_parallel.RData"
)

df = all_results |>
  filter(id == 79) |> # Use tolerance for floating-point comparison
  select(trial, t, beta, coherence, estimated_slope, estimated_threshold)

p1 = ggplot(df, aes(x = trial, y = estimated_slope)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_hline(
    data = df,
    aes(yintercept = beta),
    linetype = "dashed",
    alpha = 0.5
  ) +
  ylim(0.1, 8) +
  labs(title = "Slope estimation per trial Per Trial",
       x = "Trial",
       y = "S
lope estimation",) +
  theme_minimal()

p2 = ggplot(df, aes(x = trial, y = coherence)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_hline(
    data = df,
    aes(yintercept = t),
    linetype = "dashed",
    alpha = 0.5
  ) +
  labs(title = "Threshold estimation Per Trial",
       x = "Trial",
       y = "Threshold estimation") +
  theme_minimal()

p1 + p2
