library(future.apply)

load("./adaptive_optimization_simulations/data/agents.Rdata")
load("./adaptive_optimization_simulations/data/agents_with_decreasing_threshold.Rdata")
load("./adaptive_optimization_simulations/data/config.RData")

prior_min_slope         = 0.1
prior_max_slope         = 8

possible_threshold_values = seq(config$min_coherence, config$max_coherence, length.out = 200)
possible_slope_values     = seq(prior_min_slope, prior_max_slope, length.out = 200)

# Create a uniform prior over the parameter grid
prior_density = outer(
  rep(1/length(possible_threshold_values), length(possible_threshold_values)),
  rep(1/length(possible_slope_values), length(possible_slope_values))
)
prior_density = prior_density / sum(prior_density)

candidate_coherences = seq(0, 100, length.out = 100)

compute_likelihood <- function(stimulus, lambda, guess_rate, t_grid, beta_grid) {
  outer(
    t_grid, beta_grid,
    Vectorize(function(t_val, beta_val) {
      config$prob_correct(stimulus, lambda, config$guess_rate, t_val, beta_val)
    })
  )
}

simulate_quest_plus_parallel <- function(agents_df, config, out_file) {
  plan(multisession)  # Enables parallel execution
  
  # Parallelize by agent
  agents_list = split(agents_df, agents_df$id)
  
  results_list = future_lapply(agents_list, function(agent_trials) {
    cat("Processing agent ID:", agent_trials$id[1], "\n")
    posterior_density = prior_density
    df_results = data.frame()
    
    coherence = config$initial_coherence  # Initialize stimulus for agent
    
    param_grid_t = possible_threshold_values
    param_grid_beta = possible_slope_values
    
    for (trial in seq_len(nrow(agent_trials))) {
      agent = agent_trials[trial, ]
      true_t = agent$t
      true_beta = agent$beta
      lambda = agent$lambda
      id = agent$id
      
      # Present stimulus and record response:
      prob_correct_val = config$prob_correct(coherence, lambda, config$guess_rate, true_t, true_beta)
      acc = rbinom(1, 1, prob_correct_val)
      
      ## FIX: Compute likelihood only at the presented stimulus over the parameter grid
      likelihood_matrix = compute_likelihood(coherence, lambda, config$guess_rate, param_grid_t, param_grid_beta)
      
      # Update posterior density using the likelihood evaluated at the presented stimulus
      if (acc == 1) {
        posterior_density = posterior_density * likelihood_matrix
      } else {
        posterior_density = posterior_density * (1 - likelihood_matrix)
      }
      posterior_density = posterior_density / sum(posterior_density)
      
      ## FIX: Candidate selection: For each candidate stimulus, compute expected entropy
      expected_entropy = sapply(candidate_coherences, function(stim) {
        # Compute likelihood at candidate stimulus:
        likelihood_stim = compute_likelihood(stim, lambda, config$guess_rate, param_grid_t, param_grid_beta)
        # p_yes: probability of yes response integrated over the current posterior:
        p_yes = sum(posterior_density * likelihood_stim)
        
        # Posterior update if yes:
        posterior_yes = posterior_density * likelihood_stim
        posterior_yes = posterior_yes / sum(posterior_yes)
        # Posterior update if no:
        posterior_no = posterior_density * (1 - likelihood_stim)
        posterior_no = posterior_no / sum(posterior_no)
        
        # Shannon entropy: -sum(p*log(p))
        entropy <- function(posterior) {
          sum(-posterior * log(posterior + 1e-10))
        }
        entropy_yes = entropy(posterior_yes)
        entropy_no  = entropy(posterior_no)
        
        expected_ent = p_yes * entropy_yes + (1 - p_yes) * entropy_no
        return(expected_ent)
      })
      # Choose candidate stimulus that minimizes expected entropy
      next_coherence = candidate_coherences[which.min(expected_entropy)]
      
      # Compute point estimates for parameters (posterior mean)
      # For threshold:
      grid_t_matrix = matrix(rep(possible_threshold_values, each = length(possible_slope_values)),
                             nrow = length(possible_threshold_values))
      est_threshold = sum(posterior_density * grid_t_matrix)
      # For slope:
      grid_beta_matrix = matrix(rep(possible_slope_values, length(possible_threshold_values)),
                                nrow = length(possible_threshold_values), byrow = TRUE)
      est_slope = sum(posterior_density * grid_beta_matrix)
      
      df_results = rbind(
        df_results,
        data.frame(
          id = id,
          trial = trial,
          acc = acc,
          coherence = coherence,
          t = true_t,
          beta = true_beta,
          estimated_threshold = est_threshold,
          estimated_slope = est_slope
        )
      )
      
      # Update stimulus for next trial
      coherence = next_coherence
    }
    
    return(df_results)
  })
  
  all_results = do.call(rbind, results_list)
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