load("./adaptive_optimization_simulations/data/config.RData")

# Set parameters
thresholds = seq(5, 75, by = 5)
slopes = seq(1.5, 5.5, by = 0.25)
lapse_rate = 0

agents = expand.grid(t = thresholds, beta = slopes, lambda = lapse_rate)
agents_decreasing_threshold_df = agents[rep(seq_len(nrow(agents)), each = config$Ntials),]
agents_decreasing_threshold_df$trial = rep(1:config$Ntials, times = nrow(agents))
agents_decreasing_threshold_df$t = agents_df$t - 0.005 * (agents_df$trial - 1)

save(df = agents_decreasing_threshold_df, file = "./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata")
