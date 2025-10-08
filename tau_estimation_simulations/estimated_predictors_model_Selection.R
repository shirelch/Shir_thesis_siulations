library(tidyverse)
library(glmnet)
library(randomForest)
library(caret)
library(openxlsx)

# Load configuration
load("tau_estimation_simulations/data/config.rdata")
Nobs_values = config$Nobs

# Create a comprehensive table of predictor selection across all Nobs values
predictor_selection_results <- data.frame()

for (current_Nobs in Nobs_values) {
  cat("Analyzing models for Nobs =", current_Nobs, "\n")
  
  # Load the trained models
  load(paste0("tau_estimation_simulations/models/lasso_model_Nobs_", current_Nobs, ".RData"))
  load(paste0("tau_estimation_simulations/models/rf_model_Nobs_", current_Nobs, ".RData"))
  
  # Get LASSO selected features (non-zero coefficients at lambda.min)
  lasso_coefs <- coef(lasso_model, s = "lambda.min")
  lasso_selected <- rownames(lasso_coefs)[which(lasso_coefs != 0)]
  lasso_selected <- lasso_selected[lasso_selected != "(Intercept)"]  # Remove intercept
  
  # Get Random Forest variable importance
  rf_importance <- varImp(rf_model_cv)$importance
  rf_importance$predictor <- rownames(rf_importance)
  rf_importance <- rf_importance[order(rf_importance$Overall, decreasing = TRUE), ]
  
  # Define all possible predictors (from the training data structure)
  all_predictors <- c("mean_rt", "sd_rt", "mean_derv", "sd_derv", "sd_by_mean_rt", 
                      "tau_est_mean_median", "sd_median", "q1_mean", "q2_mean", 
                      "q3_mean", "q4_mean", "q1", "q2", "q3", "q4", "q5", 
                      "q6", "q7", "q8", "q9")
  
  # Create results for this Nobs
  current_results <- data.frame(
    Nobs = current_Nobs,
    predictor = all_predictors,
    lasso_selected = all_predictors %in% lasso_selected,
    rf_importance = rf_importance$Overall[match(all_predictors, rf_importance$predictor)],
    stringsAsFactors = FALSE
  )
  
  # For RF, we'll consider top 10 most important features as "selected"
  current_results$rf_selected <- rank(-current_results$rf_importance, na.last = "keep") <= 10
  
  predictor_selection_results <- rbind(predictor_selection_results, current_results)
}

# Create summary table across all Nobs values
summary_table <- predictor_selection_results %>%
  group_by(predictor) %>%
  summarise(
    lasso_selection_rate = mean(lasso_selected, na.rm = TRUE),
    rf_selection_rate = mean(rf_selected, na.rm = TRUE),
    avg_rf_importance = mean(rf_importance, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(lasso_selection_rate))

# Create final table with binary selection (selected if chosen in majority of conditions)
final_table <- summary_table %>%
  mutate(
    chosen_by_lasso = lasso_selection_rate > 0.5,
    chosen_by_rf = rf_selection_rate > 0.5,
    predictor_description = case_when(
      predictor == "mean_rt" ~ "Mean reaction time across all trials",
      predictor == "sd_rt" ~ "Standard deviation of reaction times",
      predictor == "sd_by_mean_rt" ~ "Coefficient of variation (SD divided by mean RT)",
      predictor == "mean_derv" ~ "Mean of absolute values of second-order differences",
      predictor == "sd_derv" ~ "Standard deviation of absolute values of second-order differences", 
      predictor == "tau_est_mean_median" ~ "Difference between mean and median RT (skewness indicator)",
      predictor == "sd_median" ~ "Mean absolute deviation from the median RT",
      predictor == "q1_mean" ~ "Mean RT derivative value within the first quartile of derivatives",
      predictor == "q2_mean" ~ "Mean RT derivative value within the second quartile of derivatives",
      predictor == "q3_mean" ~ "Mean RT derivative value within the third quartile of derivatives",
      predictor == "q4_mean" ~ "Mean RT derivative value within the fourth quartile of derivatives",
      predictor == "q1" ~ "10th percentile of the RT distribution",
      predictor == "q2" ~ "20th percentile of the RT distribution",
      predictor == "q3" ~ "30th percentile of the RT distribution",
      predictor == "q4" ~ "40th percentile of the RT distribution",
      predictor == "q5" ~ "50th percentile of the RT distribution (median)",
      predictor == "q6" ~ "60th percentile of the RT distribution",
      predictor == "q7" ~ "70th percentile of the RT distribution",
      predictor == "q8" ~ "80th percentile of the RT distribution",
      predictor == "q9" ~ "90th percentile of the RT distribution",
      TRUE ~ predictor
    )
  ) %>%
  select(predictor, predictor_description, chosen_by_lasso, chosen_by_rf, 
         lasso_selection_rate, rf_selection_rate, avg_rf_importance)

print("=== PREDICTOR SELECTION SUMMARY ===")
print(final_table)

# Save results
write.csv(final_table, "tau_estimation_simulations/results/predictor_selection_table.csv", row.names = FALSE)

# Create properly formatted academic table with correct descriptions
academic_table <- data.frame(
  Category = c(
    "Basic RT Statistics", "", "",
    "Distribution Shape Measures", "",
    "Quantile-based Measures", "", "", "", "", "", "", "", "",
    "Second-order Difference Measures", "", "", "", "", ""
  ),
  Predictor = c(
    "Mean RT", "SD RT", "CV", 
    "Mean - Median", "MAD from Median",
    "Q1 (10th percentile)", "Q2 (20th percentile)", "Q3 (30th percentile)", 
    "Q4 (40th percentile)", "Q5 (50th percentile)", "Q6 (60th percentile)",
    "Q7 (70th percentile)", "Q8 (80th percentile)", "Q9 (90th percentile)",
    "Mean |2nd differences|", "SD |2nd differences|", "Q1 derivative mean", 
    "Q2 derivative mean", "Q3 derivative mean", "Q4 derivative mean"
  ),
  Description = c(
    "Mean reaction time across all trials", 
    "Standard deviation of reaction times", 
    "Coefficient of variation (SD divided by mean RT)",
    "Difference between mean and median RT (skewness indicator)", 
    "Mean absolute deviation from the median RT",
    "10th percentile of the RT distribution", 
    "20th percentile of the RT distribution", 
    "30th percentile of the RT distribution",
    "40th percentile of the RT distribution", 
    "50th percentile of the RT distribution (median)", 
    "60th percentile of the RT distribution",
    "70th percentile of the RT distribution", 
    "80th percentile of the RT distribution", 
    "90th percentile of the RT distribution",
    "Mean of absolute values of second-order differences", 
    "Standard deviation of absolute values of second-order differences", 
    "Mean RT derivative value within the first quartile of derivatives",
    "Mean RT derivative value within the second quartile of derivatives", 
    "Mean RT derivative value within the third quartile of derivatives", 
    "Mean RT derivative value within the fourth quartile of derivatives"
  ),
  predictor_code = c(
    "mean_rt", "sd_rt", "sd_by_mean_rt",
    "tau_est_mean_median", "sd_median", 
    "q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9",
    "mean_derv", "sd_derv", "q1_mean", "q2_mean", "q3_mean", "q4_mean"
  ),
  stringsAsFactors = FALSE
)

# Add selection information to academic table
academic_table <- academic_table %>%
  left_join(
    final_table %>% select(predictor, chosen_by_lasso, chosen_by_rf),
    by = c("predictor_code" = "predictor")
  ) %>%
  mutate(
    LASSO = ifelse(chosen_by_lasso, "✓", ""),
    `Random Forest` = ifelse(chosen_by_rf, "✓", "")
  ) %>%
  select(Category, Predictor, Description, LASSO, `Random Forest`)

# Print formatted table for manuscript
cat("\n=== TABLE FOR MANUSCRIPT ===\n")
print(academic_table)

# Create Excel workbook
wb <- createWorkbook()

# Add main results sheet
addWorksheet(wb, "Table_X_Predictors")
writeData(wb, "Table_X_Predictors", 
          "Table X. Predictors entered into the machine learning models for tau estimation", 
          startRow = 1, startCol = 1)
writeData(wb, "Table_X_Predictors", academic_table, startRow = 3)

# Add note at bottom
note_text <- paste(
  "Note: ✓ indicates that the predictor was selected by the respective model across",
  "the majority of sample sizes tested (25, 50, 100, 150, 200 trials).",
  "Second-order differences capture local variability patterns in the RT sequence by examining",
  "how consecutive RT changes themselves change. The quartile-based derivative measures",
  "(Q1-Q4 derivative means) characterize different aspects of this sequential variability,",
  "with each quartile representing different magnitudes of RT fluctuations."
)
writeData(wb, "Table_X_Predictors", note_text, 
          startRow = nrow(academic_table) + 5, startCol = 1)

# Add detailed results sheet
addWorksheet(wb, "Detailed_Results")
writeData(wb, "Detailed_Results", "Detailed Predictor Selection Results", startRow = 1)
writeData(wb, "Detailed_Results", final_table, startRow = 3)

# Add selection by Nobs sheet
addWorksheet(wb, "Selection_by_Nobs")
selection_by_nobs <- predictor_selection_results %>%
  select(Nobs, predictor, lasso_selected, rf_selected) %>%
  pivot_wider(
    names_from = Nobs,
    values_from = c(lasso_selected, rf_selected),
    names_sep = "_Nobs_"
  )
writeData(wb, "Selection_by_Nobs", "Predictor Selection by Sample Size", startRow = 1)
writeData(wb, "Selection_by_Nobs", selection_by_nobs, startRow = 3)

# Style the main table
# Bold headers
addStyle(wb, "Table_X_Predictors", 
         style = createStyle(textDecoration = "bold"),
         rows = 3, cols = 1:5)

# Bold category headers
category_rows <- which(academic_table$Category != "") + 3
addStyle(wb, "Table_X_Predictors",
         style = createStyle(textDecoration = "bold"),
         rows = category_rows, cols = 1)

# Add borders
addStyle(wb, "Table_X_Predictors",
         style = createStyle(border = "TopBottomLeftRight"),
         rows = 3:(nrow(academic_table) + 3), cols = 1:5, gridExpand = TRUE)

# Auto-size columns
setColWidths(wb, "Table_X_Predictors", cols = 1:5, widths = "auto")

# Save the workbook
saveWorkbook(wb, "tau_estimation_simulations/results/Table_X_Predictors.xlsx", overwrite = TRUE)

cat("\n=== EXCEL FILE CREATED ===\n")
cat("File saved as: tau_estimation_simulations/results/Table_X_Predictors.xlsx\n")
cat("The file contains 3 sheets:\n")
cat("1. Table_X_Predictors - Main academic table\n")
cat("2. Detailed_Results - Full analysis results\n")
cat("3. Selection_by_Nobs - Selection patterns by sample size\n")

# Additional analysis: Show selection by Nobs value
cat("\n=== SELECTION BY NOBS VALUE ===\n")
print(head(selection_by_nobs, 10))