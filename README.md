# Shir Thesis Simulations (R)

This repository contains the simulation code used in my MSc thesis. The simulations focus on:
1) **Adaptive optimization of task difficulty** (psychophysics-style adaptive procedures), and  
2) **Reaction-time (RT) tail / tau estimation** from limited samples, including robustness under noise.
   
---

## Repository structure

- `adaptive_optimization_simulations/`  
  Simulations for adaptive difficulty/threshold estimation procedures (e.g., QUEST / related adaptive methods), including evaluation metrics and plotting scripts.

- `tau_estimation_simulations/`  
  Clean/controlled simulations for estimating tau (RT distribution tail) from small samples. Includes feature extraction and model training (e.g., LASSO / Random Forest).

- `noisy_tau_estimation_simulations/`  
  Robustness analyses under variable and noisy conditions (e.g., contamination/noise/lapses) and reliability-oriented evaluations.
  
---

## Quick start

### 1) Clone and open the project
```bash
git clone https://github.com/shirelch/Shir_thesis_siulations.git
Open Shir_thesis_simulations.Rproj in RStudio.
```

### 2) Install dependencies
This project assumes an R environment with common packages for simulation + modeling + plotting.

Recommended: use renv for reproducibility (if you add it):

```
install.packages("renv")
renv::restore()
```

### 3) Running the simulations
<b>Adaptive optimization simulations</b>
Go to adaptive_optimization_simulations/ and run the main driver script.

<b>Tau estimation simulations (controlled)</b>
Workflow:

- Simulate RT distributions
- Create RT features/components
- Train models for different sample sizes / trial counts
- simulate_rt_distibutions
- create_rt_components

<b>Noisy tau estimation simulations</b>
See noisy_tau_estimation_simulations/
