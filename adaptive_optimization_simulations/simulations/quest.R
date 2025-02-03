load("./adaptive_optimization_simulations/data/agents.Rdata")
load(
  "./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata"
)

load("./adaptive_optimization_simulations/data/config.RData")

all_results = data.frame()
possible_threshold_values = seq(config$min_coherence, config$max_coherence, length.out = 1000)
prior_density             = dunif(possible_threshold_values,
                                  config$min_coherence,
                                  config$max_coherence)
guessed_true_slope_values = c(0.1, 1, 5)

simulate_quest = function(agents_df,
                          config,
                          out_file,
                          guessed_true_slope) {
  for (i in seq_len(nrow(agents_df))) {
    agent = agents_df[i,]
    t = agent$t
    beta = agent$beta
    lambda = agent$lambda
    id = agent$id
    
    trial = agent$trial
    
    if (trial == 1) {
      coherence = config$initial_coherence
      posterior_density       = prior_density
      df             = data.frame()
      
    }
    
    # Present the stimulus and record the participant's response
    prob_correct = config$prob_correct(coherence, lambda, config$guess_rate, t, beta)
    acc          = rbinom(1, 1, prob_correct)
    
    likelihood = config$prob_correct(
      coherence,
      lambda,
      config$guess_rate,
      possible_threshold_values,
      guessed_true_slope
    )
    # Change stim intensity
    if (acc == 1) {
      posterior_density   = posterior_density * likelihood
    } else {
      posterior_density   = posterior_density * (1 - likelihood)
    }
    
    posterior_density     = posterior_density / sum(posterior_density) # Normalize
    
    # Append data
    df = rbind(df,
               data.frame(
                 id = id,
                 trial,
                 acc = acc,
                 coherence = coherence,
                 t = t,
                 beta = beta
               ))
    
    coherence = sum(posterior_density * possible_threshold_values)
    
    if (trial == config$Ntrials) {
      # Store results for each parameter combination
      all_results = rbind(all_results, df)
      
    }
  }
  
  save(all_results,
       file = out_file)
}

for (guessed_true_slope in guessed_true_slope_values) {
  simulate_quest(
    agents_df = agents_df,
    config    = config,
    out_file = paste0(
      "./adaptive_optimization_simulations/data/static_threshold_simulation_results/quest_beta_",
      guessed_true_slope,
      "_simulation_results.RData"
    ),
    guessed_true_slope = guessed_true_slope
  )
}

simulate_quest(
  agents_df = agents_decreasing_threshold_df,
  config    = config,
  out_file = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results/quest_beta_0.1_decreasing_threshold_simulation_results.RData",
  guessed_true_slope = guessed_true_slope
)
