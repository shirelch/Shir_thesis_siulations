# Define task configuration
config = list(
  min_coherence = 0,
  max_coherence = 100,
  initial_coherence = 80,
  guess_rate = 0.5,
  Ntrials = c(50, 100, 200, 500, 1000),
  prob_correct = function(coherence, lapse_rate, guess_rate, thershold, beta) {
    guess_rate + ((1 - lapse_rate - guess_rate) / (1 + exp(-beta * (coherence - thershold))))
  })

save(config, file = "./adaptive_optimization_simulations/data/config.RData")
