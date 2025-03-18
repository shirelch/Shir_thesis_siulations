agents_files = list.files(
  "./adaptive_optimization_simulations/data/agents",
  pattern = "\\.Rdata$",
  full.names = TRUE
)
load(
  "./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata"
)

load("./adaptive_optimization_simulations/data/config.RData")

simulate_3_down_1_up = function(agents_df, config, out_file) {
  all_results = data.frame()
  
  for (i in seq_len(nrow(agents_df))) {
    agent  = agents_df[i,]
    
    t      = agent$t
    beta   = agent$beta
    lambda = agent$lambda
    trial  = agent$trial
    id = agent$id
    Ntrials = max(agents_df$trial)
    
    if (trial == 1) {
      step_size           = 30
      coherence           = config$initial_coherence
      df                  = data.frame()
      
      consecutive_correct = 0
      reversals           = 0
      previous_direction  = "no change"
    }
    
    if (reversals == 1) {
      step_size = step_size / 2
      reversals = 0
    }
    
    prob_correct = config$prob_correct(coherence,
                                       lambda,
                                       config$guess_rate,
                                       t,
                                       beta)
    
    acc = rbinom(1, 1, prob_correct)
    
    if (acc == 1) {
      consecutive_correct = consecutive_correct + 1
    } else {
      consecutive_correct = 0
    }
    
    if (acc == 1 && consecutive_correct == 3) {
      coherence          =
        max(min(coherence - step_size, config$max_coherence),
            config$min_coherence)
      direction          = "down"
      consecutive_correct = 0
    } else if (acc == 0) {
      coherence =
        max(min(coherence + step_size, config$max_coherence),
            config$min_coherence)
      direction = "up"
    } else {
      direction = "no change"
    }
    
    if (direction != "no change") {
      if (previous_direction != direction &&
          previous_direction != "no change") {
        reversals = reversals + 1
      } else {
        reversals = 0
      }
      previous_direction = direction
    }
    
    df = rbind(
      df,
      data.frame(
        id           = id,
        trial        = trial,
        acc          = acc,
        coherence    = coherence,
        prob_correct = prob_correct,
        direction    = direction,
        t            = t,
        beta         = beta
      )
    )
    
    if (trial == Ntrials) {
      all_results = rbind(all_results, df)
    }
  }
  
  save(all_results, file = out_file)
  message("Simulation saved to ", out_file)
}

for (file in agents_files) {
  load(file)
  Ntrials = max(max(agents_df$trial))
  simulate_3_down_1_up(
    agents_df = agents_df,
    config    = config,
    out_file  = sprintf(
      "./adaptive_optimization_simulations/data/static_threshold_simulation_results/3_down_1_up_adaptive_step_size_decreasing_threshold_%d_trials_results.RData",
      Ntrials
    )
  )
}

simulate_3_down_1_up(agents_df = agents_decreasing_threshold_df,
                     config    = config,
                     out_file  = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results/3_down_1_up_adaptive_step_size_decreasing_threshold_simulation_results.RData")
