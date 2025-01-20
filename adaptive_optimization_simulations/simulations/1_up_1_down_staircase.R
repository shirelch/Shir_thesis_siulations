load("./adaptive_optimization_simulations/data/agents.Rdata")
load("./adaptive_optimization_simulations/data/config.RData")

all_results = data.frame()
step_size = 5

for (i in seq_len(nrow(agents_df))) {
  agent = agents_df[i, ]
  t = agent$t
  beta = agent$beta
  lambda = agent$lambda
  
  trial = agent$trial
  
  if (trial == 1) {
    coherence = config$initial_coherence
    df             = data.frame()
  }
  
  # Present the stimulus and record the participant's response
  prob_correct = config$prob_correct(coherence, lambda, config$guess_rate, t, beta)
  acc          = rbinom(1, 1, prob_correct)
  
  # Change stim intensity
  if (acc == 1) {
    coherence = max(min(coherence - 1 * step_size, config$max_coherence),
                    config$min_coherence)
    direction      = "down"
  } else {
    coherence = max(min(coherence + 3 * step_size, config$max_coherence),
                    config$min_coherence)
    direction      = "up"
  }
  
  # Append data
  df = rbind(
    df,
    data.frame(
      trial,
      acc = acc,
      coherence = coherence,
      direction = direction,
      t = t,
      beta = beta
    )
  )
  
  if (trial == config$Ntrials) {
    # Store results for each parameter combination
    all_results = rbind(all_results, df)
  }
}

save(all_results, file = "./adaptive_optimization_simulations/data/simulation_results/1_down_1_up_simulation_results.RData")
