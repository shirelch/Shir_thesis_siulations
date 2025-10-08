create_predictors_for_rt = function(rt) {
  #### CREATE PREDICTORS MATRIX X ----
  
  mean_rt = mean(rt)
  sd_rt = sd(rt)
  sd_by_mean_rt = sd_rt / mean_rt
  
  sd_median = mean_diff_from_median(rt)
  tau_est_mean_median = mean(rt) - median(rt)
  
  # Compute derivative metrics
  rt_derv = diff(rt, differences = 2)
  rt_derv_quartiles = cut(rt_derv,
                          quantile(rt_derv, probs = seq(0, 1, 0.25), na.rm = TRUE),
                          include.lowest = TRUE)
  q1_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[1]], na.rm = TRUE)
  q2_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[2]], na.rm = TRUE)
  q3_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[3]], na.rm = TRUE)
  q4_mean = mean(rt_derv[rt_derv_quartiles == levels(rt_derv_quartiles)[4]], na.rm = TRUE)
  mean_derv = mean(abs(rt_derv))
  sd_derv = sd(abs(rt_derv))
  
  # Compute quantiles and statistics metrics
  q1 = quantile(rt, 0.1)
  q2 = quantile(rt, 0.2)
  q3 = quantile(rt, 0.3)
  q4 = quantile(rt, 0.4)
  q5 = quantile(rt, 0.5)
  q6 = quantile(rt, 0.6)
  q7 = quantile(rt, 0.7)
  q8 = quantile(rt, 0.8)
  q9 = quantile(rt, 0.9)
  
  X = data.frame(
    mean_rt, sd_rt, mean_derv, sd_derv, sd_by_mean_rt, tau_est_mean_median,
    sd_median, q1_mean, q2_mean, q3_mean, q4_mean,
    q1, q2, q3, q4, q5, q6, q7, q8, q9
  )
  
  
  #### exGaus Fit ----
  # Filter data for the current combination
  # Check if data_subset has sufficient observations and variance
  if (var(rt) > 0) {
    # Proceed to fit the model
    nu_coef = NA
    tryCatch({
      model = gamlss(rt ~ 1, family = exGAUS(), data = data.frame(rt), trace = FALSE)
      nu_coef = coef(model, parameter = 'nu')
    }, error = function(e) {
      # If there is an error fitting the model, store NA values
      message("Error message:", e$message)
    })
    
  } else {
    # Not enough data or zero variance, store NA values
    nu_coef = NA
  }
  
  
  #### PREDICT TAU ----
  rf_factor      = as.numeric(predict(rf_model_cv, newdata = X))
  exGaus_fit     = as.numeric(nu_coef)
  
  if (exists("lasso_model")) {
    lasso_factor  = as.numeric(predict(lasso_model, newx = as.numeric(X), s = "lambda.min"))
    X$lasso_factor = lasso_factor
  }
  X$rf_factor = rf_factor
  X$exGaus_fit = exGaus_fit

  
  return(X)
}