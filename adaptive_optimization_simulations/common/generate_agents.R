# Set parameters
thresholds = seq(5, 75, by = 5)
slopes = seq(1.5, 5.5, by = 0.25)
lapse_rate = 0

agents = expand.grid(t = thresholds, beta = slopes, lambda = lapse_rate)

save(df = agents, file = "./adaptive_optimization_simulations/data/agents.Rdata")
