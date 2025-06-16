# Define task configuration
config = list(Nobs = c(25, 50, 100, 150, 200),
              Nsubj = 1000)

save(config, file = "./tau_estimation_simulations/data/config.RData")
