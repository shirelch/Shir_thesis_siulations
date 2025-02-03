load("./adaptive_optimization_simulations/data/config.RData")

# Set parameters
Nsubjects = 100
thresholds = runif(Nsubjects, 1, 30)
slopes = runif(Nsubjects, 0.5,8)
lapse_rate = 0

agents = data.frame(id = seq(1,Nsubjects), t = thresholds, beta = slopes, lambda = lapse_rate)
agents_decreasing_threshold_df = agents[rep(seq_len(nrow(agents)), each = config$Ntrials),]
agents_decreasing_threshold_df$trial = rep(1:config$Ntrials, times = nrow(agents))
agents_decreasing_threshold_df$t = agents_decreasing_threshold_df$t - 0.01 * (agents_decreasing_threshold_df$trial - 1)

save(df = agents_decreasing_threshold_df, file = "./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata")
