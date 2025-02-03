load("./adaptive_optimization_simulations/data/config.RData")

# Set parameters
Nsubjects = 100
thresholds = runif(Nsubjects, 1, 30)
slopes = runif(Nsubjects, 0.5,8)
lapse_rate = 0

agents = data.frame(id = seq(1,Nsubjects), t = thresholds, beta = slopes, lambda = lapse_rate)
agents_df = agents[rep(seq_len(nrow(agents)), each = config$Ntrials), ]
agents_df$trial = rep(1:config$Ntrials, times = nrow(agents))

save(df = agents_df, file = "./adaptive_optimization_simulations/data/agents.Rdata")
