

# Descriptive functions

get_descriptive_stats_aid <- function(df) {
  df %>%
    group_by(AID) %>%
    summarise(
      TSX_Count = n(),
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      TSX_Med = median(TSX, na.rm = TRUE),
      TSX_Min = min(TSX, na.rm = TRUE),
      TSX_Max = max(TSX, na.rm = TRUE)
    )
}

print_descriptive_box_plot <- function(df) {
  ggplot(df, aes(x = as.factor(AID), y = TSX)) +
    geom_boxplot() +
    labs(
      title = "",
      x = "AI Code Generation Used (AID)",
      y = "Time Searching (TSX)"
    ) +
    theme_minimal()
}

get_exp_stats_by_aid <- function(df) {
  df %>%
    summarise(
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      AID_Mean = mean(AID, na.rm = TRUE),
      AID_SD = sd(AID, na.rm = TRUE),
      EXP_Mean = mean(EXP, na.rm = TRUE),
      EXP_SD = sd(EXP, na.rm = TRUE),
      PLC_Mean = mean(PLC, na.rm = TRUE),
      PLC_SD = sd(PLC, na.rm = TRUE),
      CTY_Mean = mean(CTY_INT, na.rm = TRUE),
      CTY_SD = sd(CTY_INT, na.rm = TRUE)
    )
}

get_descriptive_stats <- function(df, field) {
  df %>%
    summarise(
      Count = n(),
      Mean = mean(.data[[field]], na.rm = TRUE),
      SD = sd(.data[[field]], na.rm = TRUE),
      Median = median(.data[[field]], na.rm = TRUE),
      Min = min(.data[[field]], na.rm = TRUE),
      Max = max(.data[[field]], na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), round, 3))
}


#Remote Work
get_descriptive_stats_aid_remotework <- function(df) {
  df %>%
    group_by(AID, RemoteWork) %>%
    summarise(
      TSX_Count = n(),
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      TSX_Med = median(TSX, na.rm = TRUE),
      TSX_Min = min(TSX, na.rm = TRUE),
      TSX_Max = max(TSX, na.rm = TRUE)
    ) %>%
    arrange(AID, RemoteWork)  # Sorting for better readability
}

plot_boxplot_aid_remotework <- function(df, title = "TSX Distribution by AID and Remote Work") {
  ggplot(df, aes(x = as.factor(AID), y = TSX, fill = as.factor(RemoteWork))) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +  # Adjust transparency for better visibility
    geom_jitter(aes(color = as.factor(RemoteWork)), width = 0.2, alpha = 0.5, size = 1) +  # Add jittered points
    labs(
      title = title,
      x = "AI Code Generation Usage (AID)",
      y = "Time Searching (TSX)",
      fill = "Remote Work",
      color = "Remote Work"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +  # Use a color palette for better visualization
    scale_color_brewer(palette = "Set2") + 
    theme(legend.position = "right")  # Adjust legend position
}

plot_aid_remote_tsx <- function(df) {
  # Compute mean TSX for plotting
  summary_df <- df %>%
    group_by(AID, RemoteWork) %>%
    summarise(TSX_Mean = mean(TSX, na.rm = TRUE), .groups = "drop") %>%
    mutate(RemoteWork = factor(RemoteWork, levels = c("In-person", "Hybrid (some remote, some in-person)", "Remote")))  # Specify order
  
  # Generate Bar Chart
  ggplot(summary_df, aes(x = RemoteWork, y = TSX_Mean, fill = as.factor(AID))) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    coord_cartesian(ylim = c(60, max(summary_df$TSX_Mean) + 5)) +  # Y-axis starts at 60
    labs(title = "",
         x = "Work Location",
         y = "Mean Search Time",
         fill = "AI Tool Usage") +
    theme_minimal() +
    theme(legend.position = "top") +
    geom_text(aes(label = round(TSX_Mean, 1)), 
              position = position_dodge(width = 0.9), vjust = -0.5)  # Add Mean TSX as labels
}

#Org Size
get_descriptive_stats_aid_orgsize <- function(df) {
  df %>%
    group_by(AID, OrgSize) %>%
    summarise(
      TSX_Count = n(),
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      TSX_Med = median(TSX, na.rm = TRUE),
      TSX_Min = min(TSX, na.rm = TRUE),
      TSX_Max = max(TSX, na.rm = TRUE)
    ) %>%
    arrange(AID, OrgSize) %>%
    print()  # Print results to console
}

get_descriptive_stats_aid_age <- function(df) {
  df %>%
    group_by(AID, Age_Int) %>%
    summarise(
      TSX_Count = n(),
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      TSX_Med = median(TSX, na.rm = TRUE),
      TSX_Min = min(TSX, na.rm = TRUE),
      TSX_Max = max(TSX, na.rm = TRUE)
    ) %>%
    arrange(AID, Age_Int) %>%
    print()  # Print results to console
}

get_descriptive_stats_aid_exp <- function(df) {
  df %>%
    group_by(AID, EXP) %>%
    summarise(
      TSX_Count = n(),
      TSX_Mean = mean(TSX, na.rm = TRUE),
      TSX_SD = sd(TSX, na.rm = TRUE),
      TSX_Med = median(TSX, na.rm = TRUE),
      TSX_Min = min(TSX, na.rm = TRUE),
      TSX_Max = max(TSX, na.rm = TRUE)
    ) %>%
    arrange(AID, EXP) %>%
    print()  # Print results to console
}

plot_aid_orgsize_int_tsx <- function(df) {
  # Compute mean TSX for plotting
  summary_df <- df %>%
    group_by(AID, Orgsize_Int) %>%
    summarise(TSX_Mean = mean(TSX, na.rm = TRUE), .groups = "drop")
  
  # Generate Line Chart
  ggplot(summary_df, aes(x = Orgsize_Int, y = TSX_Mean, color = as.factor(AID))) +
    geom_line(size = 1) +  # Line plot
    geom_point(size = 2) +  # Add points for clarity
    labs(title = "Mean TSX by AI Usage and Organization Size",
         x = "Organization Size (Numeric)",
         y = "Mean TSX",
         color = "AI Tool Usage (AID)") +
    theme_minimal() +
    theme(legend.position = "top") +
    scale_color_manual(values = c("blue", "red"))  # Customize colors for AID series
}




# Correlation Matrix

# get_correlation_matrix_full <- function(df) {
#   # Filter rows with non-NA JobSat
#   # df <- df %>% filter(!is.na(JobSat))
#   
#   # Select relevant columns for correlation and remove rows with any NAs
#   df_corr <- df %>%
#     #dplyr::select(TSX, AID, PLC, EXP, CTY_INT) %>%
#     dplyr::select(TSX, AID, PLC, EXP, CTY_INT, Age_Int, pop_est, EdLevel_Int, Orgsize_Int, RemoteWork_Int, Frequency_Mean) %>%
#     #dplyr::select(TSX, AID, PLC, EXP, CTY_INT, Age_Int, pop_est, EdLevel_Int, Orgsize_Int, RemoteWork_Int, Frequency_1_Int, Frequency_2_Int, Frequency_3_Int, Knowledge_1_Int,Knowledge_2_Int,Knowledge_3_Int, Knowledge_4_Int,Knowledge_5_Int, Knowledge_6_Int,Knowledge_7_Int,Knowledge_8_Int) %>%
#     na.omit()  # Remove rows with NA values
#   
#   # Coerce all columns to numeric
#   df_corr <- df_corr %>%
#     mutate(across(everything(), as.numeric))
#   
#   # Check if there are any NA/NaN/Inf values introduced by coercion
#   if (any(is.na(as.matrix(df_corr)) | is.nan(as.matrix(df_corr)) | is.infinite(as.matrix(df_corr)))) {
#     stop("The dataset contains NA/NaN/Inf values after coercion. Please clean the data before running this function.")
#   }
#   
#   # Calculate the correlation matrix and p-values
#   correlation_matrix <- rcorr(as.matrix(df_corr), type = "pearson")
#   
#   # Return the results (r and p-values)
#   return(list(
#     r = correlation_matrix$r,
#     p = correlation_matrix$P
#   ))
# }

#Round to three deimals
get_correlation_matrix_full <- function(df) {
  # Select relevant columns for correlation and remove rows with any NAs
  df_corr <- df %>%
    #dplyr::select(TSX, AID, PLC, EXP, CTY_INT, Age_Int, pop_est, EdLevel_Int, Orgsize_Int, RemoteWork_Int, Frequency_Mean, economy_int) %>%
    dplyr::select(TSX, AID, PLC, EXP, CTY_INT, Age_Int, pop_est, EdLevel_Int, Orgsize_Int, RemoteWork_Int, Frequency_Mean, economy_int, Codeium, GitHub_Copilot, Replit_Ghostwriter, Tabnine, Whispr_AI, 
                  Frontend_JS_Langs, Systems_Langs, Rust_R_Group, Functional_Langs, Mobile_Langs, Scripting_Langs, Python, Java, PHP, C_, SQL, Ruby, Go) %>%
    na.omit()  # Remove rows with NA values
  
  # Coerce all columns to numeric
  df_corr <- df_corr %>%
    mutate(across(everything(), as.numeric))
  
  # Check for NA/NaN/Inf values introduced by coercion
  if (any(is.na(as.matrix(df_corr)) | is.nan(as.matrix(df_corr)) | is.infinite(as.matrix(df_corr)))) {
    stop("The dataset contains NA/NaN/Inf values after coercion. Please clean the data before running this function.")
  }
  
  # Compute correlation matrix
  correlation_matrix <- rcorr(as.matrix(df_corr), type = "pearson")
  
  # Round correlation and p-values to 3 decimals
  correlation_matrix$r <- round(correlation_matrix$r, 3)
  correlation_matrix$P <- round(correlation_matrix$P, 3)
  
  # Return the rounded results
  return(list(
    r = correlation_matrix$r,
    p = correlation_matrix$P
  ))
}

add_significance_stars <- function(r_matrix, p_matrix) {
  # Handle missing p-values (replace NA with 1)
  p_matrix[is.na(p_matrix)] <- 1
  
  # Assign significance stars based on p-values
  stars <- ifelse(p_matrix < 0.001, "***",
                  ifelse(p_matrix < 0.01, "**",
                         ifelse(p_matrix < 0.05, "*",
                                ifelse(p_matrix < 0.1, ".", ""))))
  
  # Convert r_matrix to character format
  r_matrix <- format(round(r_matrix, 3), nsmall = 3)
  
  # Ensure stars is character
  stars <- as.character(stars)
  
  # Append significance stars
  combined_matrix <- matrix(paste0(r_matrix, stars), 
                            nrow = nrow(r_matrix), 
                            ncol = ncol(r_matrix),
                            dimnames = dimnames(r_matrix))
  return(combined_matrix)
}


# Linear Regression

perform_linear_regression <- function(df) {
  model <- lm(TSX ~ AID + PLC + EXP + CTY_INT + AID:PLC + AID:EXP + AID:CTY_INT, data = df)
  
  #Print correlation matrix
  print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
  
  # Create predicted values
  df$predicted_TSX <- predict(model, newdata = df)
  
  # Calculate mean predicted TSX for each EXP and AID combination
  summary_df <- df %>%
    group_by(EXP, AID) %>%
    dplyr::summarize(mean_TSX = mean(predicted_TSX), .groups = "drop")
  
  # Generate the interaction plot
  ggplot(summary_df, aes(x = EXP, y = mean_TSX, color = factor(AID), linetype = factor(AID))) +
    geom_line(size = 1) +
    labs(
      title = "",
      x = "Experience (EXP) in Years",
      y = "Mean Predicted Time Searching (TSX)",
      color = "AI Tool Usage",
      linetype = "AI Tool Usage"
    ) +
    scale_color_manual(values = c("blue", "red"), labels = c("No AI Tools", "With AI Tools")) +
    scale_linetype_manual(values = c("dashed", "solid"), labels = c("No AI Tools", "With AI Tools")) +
    theme_minimal()
}



perform_linear_regression_moderators_w_interactions <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT + AID * EXP + AID * PLC + AID * CTY_INT, data = df)
  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  #New rounding to 3 deimals
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  print(coef_table)
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}

perform_linear_regression_main_demographics_interactions <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT + AID:EXP + AID:PLC + AID:CTY_INT, data = df)
  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  print(coef_table)
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}

plot_aid_vs_exp_effects <- function(model, df) {
  # Generate predicted values based on the model
  df$predicted_TSX <- predict(model, df)
  
  # Create the interaction plot
  ggplot(df, aes(x = EXP, y = predicted_TSX, color = factor(AID))) +
    geom_point(alpha = 0.3) +  # Scatterplot for distribution visibility
    geom_smooth(method = "loess", se = TRUE) +  # Use loess for smooth curve fitting
    scale_x_log10() +  # Log scale for better readability of experience distribution
    labs(title = "Impact of AI Code-Generation and Experience on Search Time",
         x = "Developer Experience (EXP)",
         y = "Predicted Search Time (TSX)",
         color = "AI Code Generation (AID)") +
    scale_color_manual(values = c("red", "blue"), labels = c("No AI", "AI Used")) +
    theme_minimal()
}





perform_linear_regression_demographics_langs <- function(df) {
  model <- lm(TSX ~ AID + Age_Int + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + economy_int + Python + JavaScript + TypeScript + Java + C_ + C__ + PHP + C + SQL + Ruby + Go, data = df)
  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  print(coef_table)
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}




perform_linear_regression_demographics_langs_interactions <- function(df) {
  #model <- lm(TSX ~ AID + Age_Int + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + economy_int + Python + JavaScript + TypeScript + Java + C_ + C__ + PHP + C + SQL + Ruby + Go + AID:Python + AID:JavaScript + AID:TypeScript + AID:Java + AID:C_ + AID:C__ + AID:PHP + AID:C + AID:SQL + AID:Ruby + AID:Go, data = df)
  #model <- lm(TSX ~ AID:Python + AID:JavaScript + AID:TypeScript + AID:Java + AID:C_ + AID:C__ + AID:PHP + AID:C + AID:SQL + AID:Ruby + AID:Go, data = df)#Print correlation matrix
  #model <- lm(TSX ~ Python + JavaScript + TypeScript + Java + C_ + C__ + PHP + C + SQL + Ruby + Go + AID:Python + AID:JavaScript + AID:TypeScript + AID:Java + AID:C_ + AID:C__ + AID:PHP + AID:C + AID:SQL + AID:Ruby + AID:Go, data = df)#Print correlation matrix
  
  print("Demographics + Tools")
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  print(coef_table)
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}



perform_linear_regression_all <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est +
                Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI + 
                Frontend_JS_Langs + Systems_Langs + Rust_R_Group + Functional_Langs + Mobile_Langs + Scripting_Langs + Python + Java + PHP + C_ + SQL + Ruby + Go +
                AID:EXP + AID:PLC + AID:CTY_INT + AID:Orgsize_Int + AID:RemoteWork_Int + AID:EdLevel_Int + AID:Frequency_Mean + AID:pop_est +
                AID:Codeium + AID:GitHub_Copilot + AID:Replit_Ghostwriter + AID:Tabnine + AID:Whispr_AI + 
                AID:Frontend_JS_Langs + AID:Systems_Langs + AID:Rust_R_Group + AID:Functional_Langs + AID:Mobile_Langs + AID:Scripting_Langs + AID:Python + AID:Java + AID:PHP + AID:C_ + AID:SQL + AID:Ruby + AID:Go +
                AID:Codeium:Frontend_JS_Langs + AID:Codeium:Systems_Langs + AID:Codeium:Rust_R_Group + AID:Codeium:Functional_Langs + AID:Codeium:Mobile_Langs + AID:Codeium:Scripting_Langs + AID:Codeium:Python + AID:Codeium:Java + AID:Codeium:PHP + AID:Codeium:C_ + AID:Codeium:SQL + AID:Codeium:Ruby + AID:Codeium:Go +
                AID:GitHub_Copilot:Frontend_JS_Langs + AID:GitHub_Copilot:Systems_Langs + AID:GitHub_Copilot:Rust_R_Group + AID:GitHub_Copilot:Functional_Langs + AID:GitHub_Copilot:Mobile_Langs + AID:GitHub_Copilot:Scripting_Langs + AID:GitHub_Copilot:Python + AID:GitHub_Copilot:Java + AID:GitHub_Copilot:PHP + AID:GitHub_Copilot:C_ + AID:GitHub_Copilot:SQL + AID:GitHub_Copilot:Ruby + AID:GitHub_Copilot:Go +
                AID:Replit_Ghostwriter:Frontend_JS_Langs + AID:Replit_Ghostwriter:Systems_Langs + AID:Replit_Ghostwriter:Rust_R_Group + AID:Replit_Ghostwriter:Functional_Langs + AID:Replit_Ghostwriter:Mobile_Langs + AID:Replit_Ghostwriter:Scripting_Langs + AID:Replit_Ghostwriter:Python + AID:Replit_Ghostwriter:Java + AID:Replit_Ghostwriter:PHP + AID:Replit_Ghostwriter:C_ + AID:Replit_Ghostwriter:SQL + AID:Replit_Ghostwriter:Ruby + AID:Replit_Ghostwriter:Go +
                AID:Tabnine:Frontend_JS_Langs + AID:Tabnine:Systems_Langs + AID:Tabnine:Rust_R_Group + AID:Tabnine:Functional_Langs + AID:Tabnine:Mobile_Langs + AID:Tabnine:Scripting_Langs + AID:Tabnine:Python + AID:Tabnine:Java + AID:Tabnine:PHP + AID:Tabnine:C_ + AID:Tabnine:SQL + AID:Tabnine:Ruby + AID:Tabnine:Go +
                AID:Whispr_AI:Frontend_JS_Langs + AID:Whispr_AI:Systems_Langs + AID:Whispr_AI:Rust_R_Group + AID:Whispr_AI:Functional_Langs + AID:Whispr_AI:Mobile_Langs + AID:Whispr_AI:Scripting_Langs + AID:Whispr_AI:Python + AID:Whispr_AI:Java + AID:Whispr_AI:PHP + AID:Whispr_AI:C_ + AID:Whispr_AI:SQL + AID:Whispr_AI:Ruby + AID:Whispr_AI:Go, data = df)
  
  format_regression_output(model)
  
  #print("Demographics + Tools")
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  # coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  # print(coef_table)
  
  #Print BP Test
  #print(bptest(model))
  
  #Print VIF Test
  #print(vif(model))
}

perform_linear_regression_demographics_aitools_langs_interactions <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est +
                Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI + 
                Frontend_JS_Langs + Systems_Langs + Rust_R_Group + Functional_Langs + Mobile_Langs + Scripting_Langs + Python + Java + PHP + C_ + SQL + Ruby + Go +
                AID:Codeium + AID:GitHub_Copilot + AID:Replit_Ghostwriter + AID:Tabnine + AID:Whispr_AI + 
                AID:Frontend_JS_Langs + AID:Systems_Langs + AID:Rust_R_Group + AID:Functional_Langs + AID:Mobile_Langs + AID:Scripting_Langs + AID:Python + AID:Java + AID:PHP + AID:C_ + AID:SQL + AID:Ruby + AID:Go, data = df)
  
  #format_regression_output(model)
  
  data_standardized <- data.frame(
    TSX = scale(data$TSX),
    AID = scale(data$AID),
    EXP = scale(data$EXP),
    PLC = scale(data$PLC),
    CTY_INT = scale(data$CTY_INT),
    Orgsize_Int = scale(data$Orgsize_Int),
    RemoteWork_Int = scale(data$RemoteWork_Int),
    EdLevel_Int = scale(data$EdLevel_Int),
    Frequency_Mean = scale(data$Frequency_Mean),
    pop_est = scale(data$pop_est),
    AID_EXP = scale(data$AID * data$EXP),
    AID_PLC = scale(data$AID * data$PLC),
    AID_CTY_INT = scale(data$AID * data$CTY_INT),
    AID_Orgsize_Int = scale(data$AID * data$Orgsize_Int),
    AID_RemoteWork_Int = scale(data$AID * data$RemoteWork_Int),
    AID_EdLevel_Int = scale(data$AID * data$EdLevel_Int),
    AID_Frequency_Mean = scale(data$AID * data$Frequency_Mean),
    AID_pop_est = scale(data$AID * data$pop_est),
    AID_Codeium = scale(data$AID * data$Codeium),
    AID_GitHub_Copilot = scale(data$AID * data$GitHub_Copilot),
    AID_Replit_Ghostwriter = scale(data$AID * data$Replit_Ghostwriter),
    AID_Tabnine = scale(data$AID * data$Tabnine),
    AID_Whispr_AI = scale(data$AID * data$Whispr_AI),
    Frontend_JS_Langs = scale(data$Frontend_JS_Langs),
    Systems_Langs = scale(data$Systems_Langs),
    Rust_R_Group = scale(data$Rust_R_Group),
    Functional_Langs = scale(data$Functional_Langs),
    Mobile_Langs = scale(data$Mobile_Langs),
    Scipting_Langs = scale(data$Scripting_Langs),
    Python = scale(data$Python),
    Java = scale(data$Java),
    PHP = scale(data$PHP),
    C_ = scale(data$C_),
    SQL = scale(data$SQL),
    Ruby = scale(data$Ruby),
    Go = scale(data$Go)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + AID_EXP + AID_PLC + AID_CTY_INT + AID_Orgsize_Int + AID_RemoteWork_Int + AID_EdLevel_Int + AID_Frequency_Mean + AID_pop_est +
                             AID_Codeium + AID_GitHub_Copilot + AID_Replit_Ghostwriter + AID_Tabnine + AID_Whispr_AI, data = data_standardized)
  
  format_beta_regression_output(model_standardized)  
  
}

perform_linear_regression_significant_factors <- function(df) {
  model <- lm(TSX ~ EXP + PLC + Orgsize_Int + RemoteWork_Int + Frequency_Mean + pop_est + 
                Frontend_Langs + Rust_R_Group + Functional_Langs + Python + PHP + Go +
                AID:EXP + AID:CTY_INT + AID:pop_est +
                AID:Tabnine + 
                AID:PHP, data = df)
  
  format_regression_output(model)
  
  #print("Significant Factors")
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  # coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  # print(coef_table)
  
  #Print BP Test
  #print(bptest(model))
  
  #Print VIF Test
  #print(vif(model))
}

perform_linear_regression_exploratory <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + Orgsize_Int + RemoteWork_Int + Frequency_Mean + pop_est + economy_int + 
                Tabnine + Python + TypeScript + Java + PHP + Go + 
                AID:EXP + AID:CTY_INT + AID:pop_est + AID:Tabnine + AID:PHP + AID:Codeium:C + AID:Codeium:SQL + AID:Python:GitHub_Copilot + AID:TypeScript:GitHub_Copilot, data = df)

  format_regression_output(model)  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  #New rounding to 3 deimals
  # coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  # coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  # print(coef_table)
  
  #Print BP Test
  #print(bptest(model))
  
  #Print VIF Test
  #print(vif(model))
}

plot_aid_exp_interaction <- function(model, df) {
  # Generate predicted values based on the model
  df$predicted_TSX <- predict(model, df)
  
  # Create interaction plot
  ggplot(df, aes(x = EXP, y = predicted_TSX, color = factor(AID))) +
    geom_point(alpha = 0.4) +  # Add points with slight transparency
    geom_smooth(method = "lm", se = TRUE) +  # Add linear trend lines with confidence bands
    labs(title = "Interaction Effect of AI Code-Generation and Experience on Search Time",
         x = "Developer Experience (EXP)",
         y = "Predicted Search Time (TSX)",
         color = "AI Code Generation (AID)") +
    scale_color_manual(values = c("red", "blue"), labels = c("No AI", "AI Used")) +
    theme_minimal()
}

perform_linear_regression_demographics_country <- function(df) {
  model <- lm(TSX ~ AID + Age_Int + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + gdp_md + economy_int, data = df)
  
  #Print correlation matrix
  print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}

perform_linear_regression_exp_age <- function(df) {
  model <- lm(TSX ~ AID + Age_Int + EXP + EXP * Age_Int, data = df)
  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  print(coef_table)
  
  #Print BP Test
  print(bptest(model))
  
  #Print VIF Test
  print(vif(model))
}

perform_linear_regression_exp_age_heatmap <- function() {
  # Hardcoded regression coefficients
  intercept <- 84.2298279
  age_coef <- -0.2317414
  exp_coef <- -2.2313519
  interaction_coef <- 0.0313441
  
  # Define Age and Experience Ranges
  age_range <- seq(20, 65, length.out = 100)  # Developer Age from 20 to 65
  exp_range <- seq(1, 40, length.out = 100)   # Experience from 1 to 40
  
  # Create a Grid of Age and Experience Values
  grid_data <- expand.grid(Age_Int = age_range, EXP = exp_range)
  
  # Calculate TSX using the Regression Equation
  grid_data$TSX <- with(grid_data, 
                        intercept + (age_coef * Age_Int) + (exp_coef * EXP) + (interaction_coef * Age_Int * EXP)
  )
  
  # Generate the Heatmap with Blue-to-Red Color Palette
  ggplot(grid_data, aes(x = Age_Int, y = EXP, fill = TSX)) +
    geom_raster(interpolate = TRUE) +  # Smooth color transitions
    scale_fill_gradientn(colors = c("blue", "deepskyblue", "steelblue", "lightblue", "yellow", "firebrick", "red")) + 
    labs(title = "",
         x = "Developer Age", 
         y = "Years of Experience",
         fill = "Predicted Search Time") +
    theme_minimal()
}

perform_linear_regression_exp_age_chart_new <- function(data) {
  # Perform Linear Regression Model
  model <- lm(TSX ~ AID + Age_Int + EXP + EXP * Age_Int, data = data)
  
  # Print Regression Summary
  print(summary(model))
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  age_coef <- coef(model)["Age_Int"]
  exp_coef <- coef(model)["EXP"]
  interaction_coef <- coef(model)["EXP:Age_Int"]
  
  # Define Experience Levels to Plot (e.g., 5, 10, 20, 30 years)
  exp_levels <- c(5, 10, 20, 30)
  
  # Generate Age Range
  age_range <- seq(20, 65, length.out = 100)
  
  # Create Data for Line Plot
  plot_data <- expand.grid(Age_Int = age_range, EXP = exp_levels) %>%
    mutate(TSX = intercept + (age_coef * Age_Int) + (exp_coef * EXP) + (interaction_coef * Age_Int * EXP))
  
  # Generate Line Chart
  plot <- ggplot(plot_data, aes(x = Age_Int, y = TSX, color = factor(EXP))) +
    geom_line(size = 1) +
    labs(title = "Effect of Age on Search Time (TSX) Across Experience Levels",
         x = "Developer Age",
         y = "Predicted Search Time (TSX)",
         color = "Years of Experience") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}


perform_linear_regression_exp_age_chart <- function(df) {
  # Perform Linear Regression Model
  model <- lm(TSX ~ AID + Age_Int + EXP + EXP * Age_Int, data = df)
  
  # Extract coefficients
  intercept <- coef(model)["(Intercept)"]
  age_coef <- coef(model)["Age_Int"]
  exp_coef <- coef(model)["EXP"]
  interaction_coef <- coef(model)["EXP:Age_Int"]
  
  # Generate Age & Experience Ranges
  age_range <- seq(20, 65, length.out = 100)
  exp_range <- seq(1, 40, length.out = 100)
  
  # Create Data Grid
  grid_data <- expand.grid(Age = age_range, EXP = exp_range)
  
  
  # start debugging
  summary(grid_data$TSX)
  summary(grid_data$Age)
  summary(grid_data$EXP)
  
  ggplot(grid_data, aes(x = Age, y = EXP, color = TSX)) +
    geom_point()
  # end debuggin
  
  # Calculate TSX using Regression Equation
  grid_data$TSX <- intercept + (age_coef * grid_data$Age) + 
    (exp_coef * grid_data$EXP) + 
    (interaction_coef * grid_data$Age * grid_data$EXP)
  
  # Handle missing or constant values
  grid_data$TSX[is.na(grid_data$TSX)] <- mean(grid_data$TSX, na.rm = TRUE)
  
  # Generate the visualization
  plot <- ggplot(grid_data, aes(x = Age, y = EXP, fill = TSX)) +
    geom_raster() +  # More robust than geom_tile()
    scale_fill_gradient(low = "blue", high = "red") + 
    labs(title = "Interaction Effect of Age and Experience on Search Time",
         x = "Developer Age", 
         y = "Years of Experience",
         fill = "Predicted TSX") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}


# create_interaction_plot <- function(df) {
#   # Create the interaction plot
#   ggplot(df, aes(x = pop_est, y = TSX, color = as.factor(AID))) +
#     geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.2) + # Regression lines
#     scale_x_log10() +  # Log scale for population
#     labs(title = "Interaction Effect of AID and Population on TSX",
#          x = "Estimated Population (pop_est)",
#          y = "Time Searching (TSX)",
#          color = "AI Tool Used (AID)") +
#     theme_minimal() +
#     theme(legend.position = "top")
# }

create_interaction_plot <- function(df) {
  ggplot(df, aes(x = pop_est, y = TSX, color = as.factor(AID))) +
    geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Loess smoothing for a curve
    scale_x_log10() +  # Log scale for population
    labs(title = "",
         x = "Estimated Population",
         y = "Time Searching",
         color = "AI Usage") +
    theme_minimal() +
    theme(legend.position = "right")
}

create_interaction_plot_2 <- function(df) {
  ggplot(df, aes(x = pop_est, y = TSX, color = as.factor(AID))) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, size = 1.2) +  # Polynomial regression
    scale_x_log10() +  # Log scale for population
    labs(title = "",
         x = "Estimated Population (pop_est)",
         y = "Time Searching (TSX)",
         color = "AI Tool Used (AID)") +
    theme_minimal() +
    theme(legend.position = "top")
}

library(ggplot2)

create_experience_interaction_plot <- function(df) {
  ggplot(df, aes(x = EXP, y = TSX, color = as.factor(AID))) +
    geom_smooth(method = "loess", se = FALSE, size = 1.2) +  # Smooth trend line for AI vs. Non-AI
    scale_x_log10() +  # Log scale to normalize experience spread
    labs(title = "",
         x = "Developer Experience (EXP)",
         y = "Time Searching (TSX)",
         color = "AI Usage") +
    theme_minimal() +
    theme(legend.position = "right")
}

group_programming_languages <- function(df, drop_original = FALSE) {
  
  # Create new combined variables based on factor analysis results
  #df$Frontend_Langs <- pmax(df$JavaScript, df$TypeScript, df$Java, na.rm = TRUE) # JavaScript, TypeScript, HTML/CSS
  df$Frontend_JS_Langs <- pmax(df$JavaScript, df$TypeScript, na.rm = TRUE)
  #df$Systems_Langs <- pmax(df$C, df$C_, df$C__, na.rm = TRUE) # C, C_, C__
  df$Systems_Langs <- pmax(df$C, df$C__, na.rm = TRUE) # C, CC__
  df$Rust_R_Group <- pmax(df$Rust, df$R, na.rm = TRUE) # Rust and R (confirmed correlation: 0.73)
  df$Functional_Langs <- pmax(df$Elixir, df$Erlang, na.rm = TRUE) # Elixir and Erlang
  df$Mobile_Langs <- pmax(df$Swift, df$Kotlin, na.rm = TRUE) # Swift and Kotlin
  df$Scripting_Langs <- pmax(df$PowerShell, df$Assembly, na.rm = TRUE) # PowerShell and Assembly
  
  # Drop original language columns if specified
  if (drop_original) {
    df <- df %>% select(-c(JavaScript, TypeScript, HTML_CSS, C, C_, C__, Rust, R, Elixir, Erlang, Swift, Kotlin, PowerShell, Assembly))
  }
  
  return(df)
}

perform_linear_regression_moderators <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT, data = df)
  
  format_regression_output(model)
  
  #Print correlation matrix
  #print(coeftest(model, vcov = vcovHC(model, type = "HC1")))
  
  #New rounding to 3 deimals
  
  #coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  #coef_table[, 1:4] <- round(coef_table[, 1:4], 3)
  #print(coef_table)
  
  #Print BP Test
  #print(bptest(model))
  
  #Print VIF Test
  #print(vif(model))
}

perform_linear_regression_ai_langs_interactions <- function(df) {
  # Define AI tools and programming languages
  ai_tools <- c("Codeium", "GitHub_Copilot", "Replit_Ghostwriter", "Tabnine", "Whispr_AI")
  programming_languages <- c("Frontend_JS_Langs", "Systems_Langs", "Rust_R_Group", "Functional_Langs", "Mobile_Langs", "Python", "Java", "PHP", "SQL", "Ruby", "Go")
  
  # Generate interaction terms dynamically
  interaction_terms <- c()
  for (tool in ai_tools) {
    for (lang in programming_languages) {
      interaction_terms <- c(interaction_terms, paste0("AID:", tool, ":", lang))
    }
  }
  
  # Construct the regression formula
  formula <- as.formula(paste("TSX ~", paste(interaction_terms, collapse = " + ")))
  
  # Run the linear regression model
  model <- lm(formula, data = df)
  
  format_regression_output(model)
}

format_regression_output <- function(model) {
  # Get coefficient table with robust standard errors
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  # Round coefficients and standard errors to 3 decimals
  b_values <- round(coef_table[, 1], 3)
  se_values <- round(coef_table[, 2], 3)
  p_values <- coef_table[, 4]  # Extract p-values
  
  # Assign significance indicators
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*",
                                       ifelse(p_values < 0.1, ".", ""))))
  
  # Format b values with significance markers
  b_values <- paste0(b_values, significance)
  
  # Create a formatted data frame
  result_df <- data.frame(
    Predictor = rownames(coef_table),
    b = b_values,
    SE = se_values
  )

  # Print formatted table
  print(result_df, row.names = FALSE)
}

format_beta_regression_output <- function(model) {
  # Get coefficient table with robust standard errors
  coef_table <- coeftest(model, vcov = vcovHC(model, type = "HC1"))
  
  # Extract needed columns
  beta_values <- round(coef_table[, 1], 3)  # These are now your standardized beta coefficients!
  se_values <- round(coef_table[, 2], 3)
  p_values <- coef_table[, 4]  # Extract p-values
  
  # Assign significance indicators based on p-values
  significance <- ifelse(p_values < 0.001, "***",
                         ifelse(p_values < 0.01, "**",
                                ifelse(p_values < 0.05, "*",
                                       ifelse(p_values < 0.1, ".", ""))))
  
  # Format beta values with significance markers
  beta_values_formatted <- paste0(beta_values, significance)
  
  # Create a formatted data frame
  result_df <- data.frame(
    Predictor = rownames(coef_table),
    Beta = beta_values_formatted,
    SE = se_values,
    p = round(p_values, 3)
  )
  
  # Print formatted table
  print(result_df, row.names = FALSE)
}


################################################################################
 #1 Confirmatory Model
perform_linear_regression_main <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(data$TSX),
    AID = scale(data$AID),
    EXP = scale(data$EXP),
    PLC = scale(data$PLC),
    CTY_INT = scale(data$CTY_INT)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT, data = data_standardized)
  
  #summary(model_standardized)
  format_beta_regression_output(model_standardized)
  #format_regression_output(model_standardized)
}

#2 Demographics
perform_linear_regression_demographics <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est, data = data_standardized)
  
  format_beta_regression_output(model_standardized)
 
}

# 3. Demographics + AI Tools
perform_linear_regression_demographics_tools <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    Codeium = scale(df$Codeium),
    GitHub_Copilot = scale(df$GitHub_Copilot),
    Replit_Ghostwriter = scale(df$Replit_Ghostwriter),
    Tabnine = scale(df$Tabnine),
    Whispr_AI = scale(df$Whispr_AI)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI , data = data_standardized)
  
  format_beta_regression_output(model_standardized)
  #format_regression_output(model)
}

# 4. Demographics + AI Tools + Programming Languages
perform_linear_regression_demographics_tools_langs <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    Codeium = scale(df$Codeium),
    GitHub_Copilot = scale(df$GitHub_Copilot),
    Replit_Ghostwriter = scale(df$Replit_Ghostwriter),
    Tabnine = scale(df$Tabnine),
    Whispr_AI = scale(df$Whispr_AI),
    Frontend_JS_Langs = scale(df$Frontend_JS_Langs),
    Systems_Langs = scale(df$Systems_Langs),
    Rust_R_Group = scale(df$Rust_R_Group),
    Functional_Langs = scale(df$Functional_Langs),
    Mobile_Langs = scale(df$Mobile_Langs),
    Scripting_Langs = scale(df$Scripting_Langs),
    Python = scale(df$Python),
    Java = scale(df$Java),
    PHP = scale(df$PHP),
    C_ = scale(df$C_),
    SQL = scale(df$SQL),
    Ruby = scale(df$Ruby),
    Go = scale(df$Go)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI +
                             Frontend_JS_Langs + Systems_Langs + Rust_R_Group + Functional_Langs + Mobile_Langs + Scripting_Langs + Python + Java + PHP + C_ + SQL + Ruby + Go, data = data_standardized)
  
  format_beta_regression_output(model_standardized)
}

# 5. Demographics + Interactions
perform_linear_regression_demographics_interactions <- function(df) {
  model <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + AID:EXP + AID:PLC + AID:CTY_INT + AID:Orgsize_Int + AID:RemoteWork_Int + AID:EdLevel_Int + AID:Frequency_Mean + AID:pop_est, data = df)
  
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    AID_EXP = scale(df$AID * df$EXP),
    AID_PLC = scale(df$AID * df$PLC),
    AID_CTY_INT = scale(df$AID * df$CTY_INT),
    AID_Orgsize_Int = scale(df$AID * df$Orgsize_Int),
    AID_RemoteWork_Int = scale(df$AID * df$RemoteWork_Int),
    AID_EdLevel_Int = scale(df$AID * df$EdLevel_Int),
    AID_Frequency_Mean = scale(df$AID * df$Frequency_Mean),
    AID_pop_est = scale(df$AID * df$pop_est)
  )
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + AID_EXP + AID_PLC + AID_CTY_INT + AID_Orgsize_Int + AID_RemoteWork_Int + AID_EdLevel_Int + AID_Frequency_Mean + AID_pop_est, data = data_standardized)
  
  format_beta_regression_output(model_standardized)  
}

# 6. Demographics + AI Tools + Interactions
perform_linear_regression_demographics_tools_interactions <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    Codeium = scale(df$Codeium),
    GitHub_Copilot = scale(df$GitHub_Copilot),
    Replit_Ghostwriter = scale(df$Replit_Ghostwriter),
    Tabnine = scale(df$Tabnine),
    Whispr_AI = scale(df$Whispr_AI),
    
    # Scaled interaction terms
    AID_EXP = scale(df$AID * df$EXP),
    AID_PLC = scale(df$AID * df$PLC),
    AID_CTY_INT = scale(df$AID * df$CTY_INT),
    AID_Orgsize_Int = scale(df$AID * df$Orgsize_Int),
    AID_RemoteWork_Int = scale(df$AID * df$RemoteWork_Int),
    AID_EdLevel_Int = scale(df$AID * df$EdLevel_Int),
    AID_Frequency_Mean = scale(df$AID * df$Frequency_Mean),
    AID_pop_est = scale(df$AID * df$pop_est),
    AID_Codeium = scale(df$AID * df$Codeium),
    AID_GitHub_Copilot = scale(df$AID * df$GitHub_Copilot),
    AID_Replit_Ghostwriter = scale(df$AID * df$Replit_Ghostwriter),
    AID_Tabnine = scale(df$AID * df$Tabnine),
    AID_Whispr_AI = scale(df$AID * df$Whispr_AI)
  )
  
  # 2. Run linear regression on standardized variables
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + 
                             Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI +
                             AID_EXP + AID_PLC + AID_CTY_INT + AID_Orgsize_Int + AID_RemoteWork_Int + 
                             AID_EdLevel_Int + AID_Frequency_Mean + AID_pop_est +
                             AID_Codeium + AID_GitHub_Copilot + AID_Replit_Ghostwriter + AID_Tabnine + AID_Whispr_AI,
                           data = data_standardized)
  
  # 3. Format the standardized output
  format_beta_regression_output(model_standardized)  
}

# 7. Demographics + AI Tools + Programming Languages + Interactions
perform_linear_regression_demographics_tools_langs_interactions_beta <- function(df) {
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    Codeium = scale(df$Codeium),
    GitHub_Copilot = scale(df$GitHub_Copilot),
    Replit_Ghostwriter = scale(df$Replit_Ghostwriter),
    Tabnine = scale(df$Tabnine),
    Whispr_AI = scale(df$Whispr_AI),
    
    # Scaled interaction terms
    AID_EXP = scale(df$AID * df$EXP),
    AID_PLC = scale(df$AID * df$PLC),
    AID_CTY_INT = scale(df$AID * df$CTY_INT),
    AID_Orgsize_Int = scale(df$AID * df$Orgsize_Int),
    AID_RemoteWork_Int = scale(df$AID * df$RemoteWork_Int),
    AID_EdLevel_Int = scale(df$AID * df$EdLevel_Int),
    AID_Frequency_Mean = scale(df$AID * df$Frequency_Mean),
    AID_pop_est = scale(df$AID * df$pop_est),
    AID_Frontend_JS_Langs = scale(df$AID * df$Frontend_JS_Langs),
    AID_Systems_Langs = scale(df$AID * df$Systems_Langs),
    AID_Rust_R_Group = scale(df$AID * df$Rust_R_Group),
    AID_Functional_Langs = scale(df$AID * df$Functional_Langs),
    AID_Mobile_Langs = scale(df$AID * df$Mobile_Langs),
    AID_Scripting_Langs = scale(df$AID * df$Scripting_Langs),
    AID_Python = scale(df$AID * df$Python),
    AID_Java = scale(df$AID * df$Java),
    AID_PHP = scale(df$AID * df$PHP),
    AID_C_ = scale(df$AID * df$C_),
    AID_SQL = scale(df$AID * df$SQL),
    AID_Ruby = scale(df$AID * df$Ruby),
    AID_Go = scale(df$AID * df$Go)
  )
  
  # 2. Run linear regression on standardized variables
  model_standardized <- lm(TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est + 
                             Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI +
                             AID_EXP + AID_PLC + AID_CTY_INT + AID_Orgsize_Int + AID_RemoteWork_Int + 
                             AID_EdLevel_Int + AID_Frequency_Mean + AID_pop_est +
                             AID_Frontend_JS_Langs + AID_Systems_Langs + AID_Rust_R_Group + AID_Functional_Langs +
                             AID_Mobile_Langs + AID_Scripting_Langs + AID_Python + AID_Java + AID_PHP +
                             AID_C_ + AID_SQL + AID_Ruby + AID_Go,
                           data = data_standardized)
  
  # 3. Format the standardized output
  format_beta_regression_output(model_standardized)  
}

# 8. Demographics + AI Tools + Programming Languages + Interactions + AID Interactions (All three)
perform_linear_regression_all_standardized <- function(df) {
  # Step 1: Standardize main variables
  data_standardized <- data.frame(
    TSX = scale(df$TSX),
    AID = scale(df$AID),
    EXP = scale(df$EXP),
    PLC = scale(df$PLC),
    CTY_INT = scale(df$CTY_INT),
    Orgsize_Int = scale(df$Orgsize_Int),
    RemoteWork_Int = scale(df$RemoteWork_Int),
    EdLevel_Int = scale(df$EdLevel_Int),
    Frequency_Mean = scale(df$Frequency_Mean),
    pop_est = scale(df$pop_est),
    
    Codeium = scale(df$Codeium),
    GitHub_Copilot = scale(df$GitHub_Copilot),
    Replit_Ghostwriter = scale(df$Replit_Ghostwriter),
    Tabnine = scale(df$Tabnine),
    Whispr_AI = scale(df$Whispr_AI),

    Frontend_JS_Langs = scale(df$Frontend_JS_Langs),
    Systems_Langs = scale(df$Systems_Langs),
    Rust_R_Group = scale(df$Rust_R_Group),
    Functional_Langs = scale(df$Functional_Langs),
    Mobile_Langs = scale(df$Mobile_Langs),
    Scripting_Langs = scale(df$Scripting_Langs),
    Python = scale(df$Python),
    Java = scale(df$Java),
    PHP = scale(df$PHP),
    C_ = scale(df$C_),
    SQL = scale(df$SQL),
    Ruby = scale(df$Ruby),
    Go = scale(df$Go),
    
    # Step 2: Standardize interaction terms
    AID_EXP = scale(df$AID * df$EXP),
    AID_PLC = scale(df$AID * df$PLC),
    AID_CTY_INT = scale(df$AID * df$CTY_INT),
    AID_Orgsize_Int = scale(df$AID * df$Orgsize_Int),
    AID_RemoteWork_Int = scale(df$AID * df$RemoteWork_Int),
    AID_EdLevel_Int = scale(df$AID * df$EdLevel_Int),
    AID_Frequency_Mean = scale(df$AID * df$Frequency_Mean),
    AID_pop_est = scale(df$AID * df$pop_est),
    
    AID_Codeium = scale(df$AID * df$Codeium),
    AID_GitHub_Copilot = scale(df$AID * df$GitHub_Copilot),
    AID_Replit_Ghostwriter = scale(df$AID * df$Replit_Ghostwriter),
    AID_Tabnine = scale(df$AID * df$Tabnine),
    AID_Whispr_AI = scale(df$AID * df$Whispr_AI),
    
    AID_Frontend_JS_Langs = scale(df$AID * df$Frontend_JS_Langs),
    AID_Systems_Langs = scale(df$AID * df$Systems_Langs),
    AID_Rust_R_Group = scale(df$AID * df$Rust_R_Group),
    AID_Functional_Langs = scale(df$AID * df$Functional_Langs),
    AID_Mobile_Langs = scale(df$AID * df$Mobile_Langs),
    AID_Scripting_Langs = scale(df$AID * df$Scripting_Langs),
    AID_Python = scale(df$AID * df$Python),
    AID_Java = scale(df$AID * df$Java),
    AID_PHP = scale(df$AID * df$PHP),
    AID_C_ = scale(df$AID * df$C_),
    AID_SQL = scale(df$AID * df$SQL),
    AID_Ruby = scale(df$AID * df$Ruby),
    AID_Go = scale(df$AID * df$Go),
    
    # 2-way interactions with each AI tool
    AID_Codeium_Frontend_JS_Langs = scale(df$AID * df$Codeium * df$Frontend_JS_Langs),
    AID_Codeium_Systems_Langs = scale(df$AID * df$Codeium * df$Systems_Langs),
    AID_Codeium_Rust_R_Group = scale(df$AID * df$Codeium * df$Rust_R_Group),
    AID_Codeium_Functional_Langs = scale(df$AID * df$Codeium * df$Functional_Langs),
    AID_Codeium_Mobile_Langs = scale(df$AID * df$Codeium * df$Mobile_Langs),
    AID_Codeium_Scripting_Langs = scale(df$AID * df$Codeium * df$Scripting_Langs),
    AID_Codeium_Python = scale(df$AID * df$Codeium * df$Python),
    AID_Codeium_Java = scale(df$AID * df$Codeium * df$Java),
    AID_Codeium_PHP = scale(df$AID * df$Codeium * df$PHP),
    AID_Codeium_C_ = scale(df$AID * df$Codeium * df$C_),
    AID_Codeium_SQL = scale(df$AID * df$Codeium * df$SQL),
    AID_Codeium_Ruby = scale(df$AID * df$Codeium * df$Ruby),
    AID_Codeium_Go = scale(df$AID * df$Codeium * df$Go),
    
    AID_GitHub_Copilot_Frontend_JS_Langs = scale(df$AID * df$GitHub_Copilot * df$Frontend_JS_Langs),
    AID_GitHub_Copilot_Systems_Langs = scale(df$AID * df$GitHub_Copilot * df$Systems_Langs),
    AID_GitHub_Copilot_Rust_R_Group = scale(df$AID * df$GitHub_Copilot * df$Rust_R_Group),
    AID_GitHub_Copilot_Functional_Langs = scale(df$AID * df$GitHub_Copilot * df$Functional_Langs),
    AID_GitHub_Copilot_Mobile_Langs = scale(df$AID * df$GitHub_Copilot * df$Mobile_Langs),
    AID_GitHub_Copilot_Scripting_Langs = scale(df$AID * df$GitHub_Copilot * df$Scripting_Langs),
    AID_GitHub_Copilot_Python = scale(df$AID * df$GitHub_Copilot * df$Python),
    AID_GitHub_Copilot_Java = scale(df$AID * df$GitHub_Copilot * df$Java),
    AID_GitHub_Copilot_PHP = scale(df$AID * df$GitHub_Copilot * df$PHP),
    AID_GitHub_Copilot_C_ = scale(df$AID * df$GitHub_Copilot * df$C_),
    AID_GitHub_Copilot_SQL = scale(df$AID * df$GitHub_Copilot * df$SQL),
    AID_GitHub_Copilot_Ruby = scale(df$AID * df$GitHub_Copilot * df$Ruby),
    AID_GitHub_Copilot_Go = scale(df$AID * df$GitHub_Copilot * df$Go),
    
    AID_Replit_Ghostwriter_Frontend_JS_Langs = scale(df$AID * df$Replit_Ghostwriter * df$Frontend_JS_Langs),
    AID_Replit_Ghostwriter_Systems_Langs = scale(df$AID * df$Replit_Ghostwriter * df$Systems_Langs),
    AID_Replit_Ghostwriter_Rust_R_Group = scale(df$AID * df$Replit_Ghostwriter * df$Rust_R_Group),
    AID_Replit_Ghostwriter_Functional_Langs = scale(df$AID * df$Replit_Ghostwriter * df$Functional_Langs),
    AID_Replit_Ghostwriter_Mobile_Langs = scale(df$AID * df$Replit_Ghostwriter * df$Mobile_Langs),
    AID_Replit_Ghostwriter_Scripting_Langs = scale(df$AID * df$Replit_Ghostwriter * df$Scripting_Langs),
    AID_Replit_Ghostwriter_Python = scale(df$AID * df$Replit_Ghostwriter * df$Python),
    AID_Replit_Ghostwriter_Java = scale(df$AID * df$Replit_Ghostwriter * df$Java),
    AID_Replit_Ghostwriter_PHP = scale(df$AID * df$Replit_Ghostwriter * df$PHP),
    AID_Replit_Ghostwriter_C_ = scale(df$AID * df$Replit_Ghostwriter * df$C_),
    AID_Replit_Ghostwriter_SQL = scale(df$AID * df$Replit_Ghostwriter * df$SQL),
    AID_Replit_Ghostwriter_Ruby = scale(df$AID * df$Replit_Ghostwriter * df$Ruby),
    AID_Replit_Ghostwriter_Go = scale(df$AID * df$Replit_Ghostwriter * df$Go),
    
    AID_Tabnine_Frontend_JS_Langs = scale(df$AID * df$Tabnine * df$Frontend_JS_Langs),
    AID_Tabnine_Systems_Langs = scale(df$AID * df$Tabnine * df$Systems_Langs),
    AID_Tabnine_Rust_R_Group = scale(df$AID * df$Tabnine * df$Rust_R_Group),
    AID_Tabnine_Functional_Langs = scale(df$AID * df$Tabnine * df$Functional_Langs),
    AID_Tabnine_Mobile_Langs = scale(df$AID * df$Tabnine * df$Mobile_Langs),
    AID_Tabnine_Scripting_Langs = scale(df$AID * df$Tabnine * df$Scripting_Langs),
    AID_Tabnine_Python = scale(df$AID * df$Tabnine * df$Python),
    AID_Tabnine_Java = scale(df$AID * df$Tabnine * df$Java),
    AID_Tabnine_PHP = scale(df$AID * df$Tabnine * df$PHP),
    AID_Tabnine_C_ = scale(df$AID * df$Tabnine * df$C_),
    AID_Tabnine_SQL = scale(df$AID * df$Tabnine * df$SQL),
    AID_Tabnine_Ruby = scale(df$AID * df$Tabnine * df$Ruby),
    AID_Tabnine_Go = scale(df$AID * df$Tabnine * df$Go),
    
    AID_Whispr_AI_Frontend_JS_Langs = scale(df$AID * df$Whispr_AI * df$Frontend_JS_Langs),
    AID_Whispr_AI_Systems_Langs = scale(df$AID * df$Whispr_AI * df$Systems_Langs),
    AID_Whispr_AI_Rust_R_Group = scale(df$AID * df$Whispr_AI * df$Rust_R_Group),
    AID_Whispr_AI_Functional_Langs = scale(df$AID * df$Whispr_AI * df$Functional_Langs),
    AID_Whispr_AI_Mobile_Langs = scale(df$AID * df$Whispr_AI * df$Mobile_Langs),
    AID_Whispr_AI_Scripting_Langs = scale(df$AID * df$Whispr_AI * df$Scripting_Langs),
    AID_Whispr_AI_Python = scale(df$AID * df$Whispr_AI * df$Python),
    AID_Whispr_AI_Java = scale(df$AID * df$Whispr_AI * df$Java),
    AID_Whispr_AI_PHP = scale(df$AID * df$Whispr_AI * df$PHP),
    AID_Whispr_AI_C_ = scale(df$AID * df$Whispr_AI * df$C_),
    AID_Whispr_AI_SQL = scale(df$AID * df$Whispr_AI * df$SQL),
    AID_Whispr_AI_Ruby = scale(df$AID * df$Whispr_AI * df$Ruby),
    AID_Whispr_AI_Go = scale(df$AID * df$Whispr_AI * df$Go)
  )
  
  # Step 3: Build the model
  model_standardized <- lm(
    TSX ~ AID + EXP + PLC + CTY_INT + Orgsize_Int + RemoteWork_Int + EdLevel_Int + Frequency_Mean + pop_est +
      Codeium + GitHub_Copilot + Replit_Ghostwriter + Tabnine + Whispr_AI +
      Frontend_JS_Langs + Systems_Langs + Rust_R_Group + Functional_Langs + Mobile_Langs + Scripting_Langs +
      Python + Java + PHP + C_ + SQL + Ruby + Go +
      AID_EXP + AID_PLC + AID_CTY_INT + AID_Orgsize_Int + AID_RemoteWork_Int + AID_EdLevel_Int + AID_Frequency_Mean + AID_pop_est +
      AID_Codeium + AID_GitHub_Copilot + AID_Replit_Ghostwriter + AID_Tabnine + AID_Whispr_AI +
      AID_Frontend_JS_Langs + AID_Systems_Langs + AID_Rust_R_Group + AID_Functional_Langs + AID_Mobile_Langs +
      AID_Scripting_Langs + AID_Python + AID_Java + AID_PHP + AID_C_ + AID_SQL + AID_Ruby + AID_Go +
      AID_Codeium_Frontend_JS_Langs + AID_Codeium_Systems_Langs + AID_Codeium_Rust_R_Group +
      AID_Codeium_Functional_Langs + AID_Codeium_Mobile_Langs + AID_Codeium_Scripting_Langs +
      AID_Codeium_Python + AID_Codeium_Java + AID_Codeium_PHP + AID_Codeium_C_ + AID_Codeium_SQL +
      AID_Codeium_Ruby + AID_Codeium_Go +
      AID_GitHub_Copilot_Frontend_JS_Langs + AID_GitHub_Copilot_Systems_Langs + AID_GitHub_Copilot_Rust_R_Group +
      AID_GitHub_Copilot_Functional_Langs + AID_GitHub_Copilot_Mobile_Langs + AID_GitHub_Copilot_Scripting_Langs +
      AID_GitHub_Copilot_Python + AID_GitHub_Copilot_Java + AID_GitHub_Copilot_PHP + AID_GitHub_Copilot_C_ +
      AID_GitHub_Copilot_SQL + AID_GitHub_Copilot_Ruby + AID_GitHub_Copilot_Go +
      AID_Replit_Ghostwriter_Frontend_JS_Langs + AID_Replit_Ghostwriter_Systems_Langs + AID_Replit_Ghostwriter_Rust_R_Group +
      AID_Replit_Ghostwriter_Functional_Langs + AID_Replit_Ghostwriter_Mobile_Langs + AID_Replit_Ghostwriter_Scripting_Langs +
      AID_Replit_Ghostwriter_Python + AID_Replit_Ghostwriter_Java + AID_Replit_Ghostwriter_PHP + AID_Replit_Ghostwriter_C_ +
      AID_Replit_Ghostwriter_SQL + AID_Replit_Ghostwriter_Ruby + AID_Replit_Ghostwriter_Go +
      AID_Tabnine_Frontend_JS_Langs + AID_Tabnine_Systems_Langs + AID_Tabnine_Rust_R_Group +
      AID_Tabnine_Functional_Langs + AID_Tabnine_Mobile_Langs + AID_Tabnine_Scripting_Langs +
      AID_Tabnine_Python + AID_Tabnine_Java + AID_Tabnine_PHP + AID_Tabnine_C_ + AID_Tabnine_SQL +
      AID_Tabnine_Ruby + AID_Tabnine_Go +
      AID_Whispr_AI_Frontend_JS_Langs + AID_Whispr_AI_Systems_Langs + AID_Whispr_AI_Rust_R_Group +
      AID_Whispr_AI_Functional_Langs + AID_Whispr_AI_Mobile_Langs + AID_Whispr_AI_Scripting_Langs +
      AID_Whispr_AI_Python + AID_Whispr_AI_Java + AID_Whispr_AI_PHP + AID_Whispr_AI_C_ +
      AID_Whispr_AI_SQL + AID_Whispr_AI_Ruby + AID_Whispr_AI_Go,
    
    data = data_standardized
  )
  
  # Step 4: Output formatted standardized regression
  format_beta_regression_output(model_standardized)
}
