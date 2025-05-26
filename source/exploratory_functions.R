perform_sem_analysis <- function(df) {
  # Define the SEM model
  model <- '
    TSX ~ AID + Age + OrgSize + EdLevel + 
    Frequency_Mean + 
    GitHub_Copilot + Codeium + Replit_Ghostwriter + Tabnine + Whispr_AI +
    Python + JavaScript + TypeScript + Java + C_ + C__ + PHP + C + SQL + Ruby + Go +
    AID:Python + AID:JavaScript + AID:TypeScript + AID:Java + AID:C_ + AID:C__ + AID:PHP + AID:C + AID:SQL + AID:Ruby + AID:Go
  '
  
  # Fit the SEM model
  fit <- sem(model, data = df, estimator = "MLR")
  
  # Print the summary with all desired options
  summary_output <- summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
  
  # Return the summary output
  return(summary_output)
}
