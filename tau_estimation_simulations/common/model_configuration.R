# Define task configuration
config = list(Nobs = c(25, 50, 100, 150, 200),
              Nsubj = 1000,
              mu_range = c(200, 2500),
              sigma_range = c(20, 800),
              tau_range = c(10, 1500)
)

save(config, file = "./tau_estimation_simulations/data/config.RData")
