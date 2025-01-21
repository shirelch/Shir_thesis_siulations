load("./adaptive_optimization_simulations/data/config.RData")

# Set parameters
thresholds = seq(5, 75, by = 5)
slopes = seq(1.5, 5.5, by = 0.25)
lapse_rate = 0

agents = expand.grid(t = thresholds, beta = slopes, lambda = lapse_rate)
agents_df = agents[rep(seq_len(nrow(agents)), each = config$Ntrials), ]
agents_df$trial = rep(1:config$Ntrials, times = nrow(agents))

save(df = agents_df, file = "./adaptive_optimization_simulations/data/agents.Rdata")
