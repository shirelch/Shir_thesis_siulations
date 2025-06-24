# Adaptive Optimization Simulations

## Project Overview

This R project contains simulations for a thesis focusing on adaptive optimization procedures.

### Key Components:

1. **Common**: 
   - Agent creation and task configuration.

2. **Simulations**: 
   - A separate file for each simulation procedure.

3. **Plots**: 
   - A separate file for each type of plot.

# Tau Estimation Simulations

## Project Overview

This R project contains simulations of tau estimation in a small number of distribution samples.

### Key Components:
1. **Common**:
   - Helper functions and task configuration.
2. **Models**:
   - generated models (lasso and random forest) for a requested number of samples.
---
## Usage
1. Simulate RT distributions: <br>
   run `simulate_rt_distibutions` to generate and save raw ex-gaussian distributions.
2. create RT components: <br>
   run `create_rt_components` to generate the components of the raw distributions, which will later be used to train ML models.
3. Train models: <br>
   run `train_lasso_rf_models` to train a specific model for each number of samples. 
---

