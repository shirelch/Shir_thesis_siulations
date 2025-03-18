agents_files = list.files(
  "./adaptive_optimization_simulations/data/agents",
  pattern = "\\.Rdata$",
  full.names = TRUE
)
load("./adaptive_optimization_simulations/data/config.RData")

step_size = 5

for (file in agents_files) {
  load(file)
  Ntrials = max(max(agents_df$trial))
  
  all_results = data.frame()
  
  for (i in seq_len(nrow(agents_df))) {
    agent = agents_df[i,]
    id = agent$id
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
        id = id,
        trial,
        acc = acc,
        coherence = coherence,
        direction = direction,
        t = t,
        beta = beta
      )
    )
    
    if (trial == Ntrials) {
      # Store results for each parameter combination
      all_results = rbind(all_results, df)
    }
  }
  
  save(
    all_results,
    file = sprintf(
      "./adaptive_optimization_simulations/data/static_threshold_simulation_results/1_down_1_up_simulation_%d_trials_results.RData",
      Ntrials
    )
  )
}