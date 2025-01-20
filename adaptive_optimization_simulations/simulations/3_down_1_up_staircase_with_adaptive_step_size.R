load("./adaptive_optimization_simulations/data/agents.Rdata")
load("./adaptive_optimization_simulations/data/config.RData")

all_results = data.frame()
step_size = 30

for (i in seq_len(nrow(agents))) {
  agent = agents[i, ]
  t = agent$t
  beta = agent$beta
  lambda = agent$lambda
  
  coherence = config$initial_coherence
  df             = data.frame()
  
  consecutive_correct = 0
  reversals = 0
  previous_direction = "no change"
  
  for (trial in 1:config$Ntrials) {
    if (reversals == 1) {
      step_size = step_size - step_size / 5
      reversals = 0
    }
    
    # Present the stimulus and record the participant's response
    prob_correct = config$prob_correct(coherence, lambda, config$guess_rate, t, beta)
    acc          = rbinom(1, 1, prob_correct)
    
    if (acc == 1) {
      consecutive_correct = consecutive_correct + 1
    } else {
      consecutive_correct = 0
    }
    
    # Change stim intensity
    if (acc == 1 && consecutive_correct == 3) {
      coherence = max(min(coherence - step_size, config$max_coherence), config$min_coherence)
      direction      = "down"
      consecutive_correct = 0
    } else if (acc == 0) {
      coherence = max(min(coherence + step_size, config$max_coherence), config$min_coherence)
      direction      = "up"
    } else {
       direction = "no change"
    }
    
    if (direction != "no change") {
      if (previous_direction != direction && previous_direction != "no change") {
        reversals = reversals + 1
      }
      else {
        reversals = 0
      }
      previous_direction = direction
    }
    
    # Append data
    df = rbind(df, data.frame(trial, acc = acc, coherence = coherence, prob_correct = prob_correct, direction = direction, 
                              t = t, beta = beta))
  trial = trial + 1
    }
  
  # Store results for each parameter combination
  all_results = rbind(all_results, df)
  }


save(all_results, file = "./adaptive_optimization_simulations/data/simulation_resultls/3_down_1_up_adaptive_step_size_simulation_results.RData")

