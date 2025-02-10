library(future.apply)

load("./adaptive_optimization_simulations/data/agents.Rdata")
load(
  "./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata"
)

load("./adaptive_optimization_simulations/data/config.RData")

all_results = data.frame()

prior_min_slope         = 0.1
prior_max_slope         = 8

possible_threshold_values = seq(config$min_coherence, config$max_coherence, length.out = 200)
possible_slope_values     = seq(prior_min_slope, prior_max_slope, length.out = 200)

prior_density = outer(
  dunif(
    possible_threshold_values,
    min = min(possible_threshold_values),
    max = max(possible_threshold_values)
  ),
  dunif(
    possible_slope_values,
    min = min(possible_slope_values),
    max = max(possible_slope_values)
  )
)

prior_density = prior_density / sum(prior_density)  # Normalize

candidate_coherences      = seq(0, 50, length.out = 100)

simulate_quest_plus_parallel = function(agents_df, config, out_file) {
  plan(multisession)  # Enables parallel execution
  
  # Parallelize by agent, not by trial
  agents_list = split(agents_df, agents_df$id)  # Split by agent ID
  
  results_list = future_lapply(agents_list, function(agent_trials) {
    cat("Processing agent ID:", agent_trials$id[1], "\n")
    posterior_density = prior_density
    df = data.frame()
    
    coherence = config$initial_coherence  # Initialize coherence per agent
    
    for (trial in seq_len(nrow(agent_trials))) {
      agent = agent_trials[trial,]
      t = agent$t
      beta = agent$beta
      lambda = agent$lambda
      id = agent$id
      
      # Present the stimulus and record the participant's response
      prob_correct = config$prob_correct(coherence, lambda, config$guess_rate, t, beta)
      acc = rbinom(1, 1, prob_correct)
      
      likelihood = 0.5 + 0.5 * (1 / (1 + exp(
        -outer(
          possible_slope_values,
          (coherence - possible_threshold_values),
          "*"
        )
      )))
      
      # Update posterior
      if (acc == 1) {
        posterior_density = posterior_density * likelihood
      } else {
        posterior_density = posterior_density * (1 - likelihood)
      }
      posterior_density = posterior_density / sum(posterior_density)
      
      # Select next stimulus using minimum entropy rule
      expected_entropy = sapply(candidate_coherences, function(stim) {
        likelihood = 0.5 + 0.5 * (1 / (1 + exp(
          -outer(
            possible_slope_values,
            (stim - possible_threshold_values),
            "*"
          )
        )))
        posterior_updated = posterior_density * likelihood
        posterior_updated = posterior_updated / sum(posterior_updated)
        entropy = sum(posterior_updated * log(posterior_updated + 1e-10))
        return(entropy)
      })
      
      coherence = candidate_coherences[which.min(-expected_entropy)]
      
      df = rbind(
        df,
        data.frame(
          id = id,
          trial = trial,
          acc = acc,
          coherence = coherence,
          t = t,
          beta = beta
        )
      )
    }
    
    return(df)
  })
  
  all_results = do.call(rbind, results_list)  # Combine results from all parallel processes
  
  save(all_results, file = out_file)
}

simulate_quest_plus_parallel(
  agents_df = agents_df,
  config = config,
  out_file = paste0(
    "./adaptive_optimization_simulations/data/static_threshold_simulation_results/quest_plus",
    "_simulation_results_parallel.RData"
  )
)

# simulate_quest(
#   agents_df = agents_decreasing_threshold_df,
#   config    = config,
#   out_file = "./adaptive_optimization_simulations/data/decreasing_threshold_simulation_results/quest_beta_0.1_decreasing_threshold_simulation_results.RData",
#   guessed_true_slope = guessed_true_slope
# )
