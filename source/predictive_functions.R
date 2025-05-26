train_random_forest <- function(data, response, predictors, train_ratio = 0.7, num_trees = 1000, seed = 123) {
  # Set the seed for reproducibility
  set.seed(seed)
  
  # Split the data into training and testing sets
  train_indices <- sample(1:nrow(data), train_ratio * nrow(data))
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Construct the formula for the model
  formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  
  # Train the random forest model
  rf_model <- ranger(formula, data = train_data, num.trees = num_trees, importance = "impurity")
  
  # Return the results as a list
  return(list(
    model = rf_model,
    train_data = train_data,
    test_data = test_data
  ))
}

plot_variable_importance <- function(importance_values, title = "") {
  # Define custom labels
  custom_labels <- c(
    pop_est = "Country Population",
    WorkExp = "Experience",
    Frequency_Mean = "Interaction Frequency",
    PLC = "Number of Programming Languages Used",
    OrgSize = "Organization Size",
    EdLevel = "Education Level",
    RemoteWork = "Work Location",
    AID = "Use of AI Code-Generation",
    Age_Int = "Respondent Age",
    EXP = "Years of Professional Experience",
    SQL = "SQL",
    GitHub_Copilot = "GitHub Copilot",
    System_Langs = "System Languages",
    Rust_R_Group = "Rust/R Group",
    Go = "Go",
    PHP = "PHP",
    Frontend_JS_Langs = "Frontend JS Languages",
    Systems_Langs = "System Languages",
    Scripting_Langs = "Scripting Languages",
    Mobile_Langs = "Mobile Languages",
    Java = "Java",
    Python = "Python",
    Tabnine = "Tabnine",
    Functional_Langs = "Functional Languages",
    Codeium = "Codeium",
    Whispr_AI = "Whispr AI",
    Replit_Ghostwriter = "Replit Ghostwriter"
  )
  
  # Create a data frame for importance values
  importance_df <- data.frame(
    Variable = names(importance_values),
    Importance = importance_values
  ) %>%
    mutate(Variable = ifelse(Variable %in% names(custom_labels), 
                             custom_labels[Variable], 
                             Variable)) # Replace with custom labels if available
  
  # Create the plot
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = format(Importance, big.mark = ",", scientific = FALSE)), 
              hjust = -0.2, size = 3) +
    coord_flip() +
    labs(title = title, x = "Variables", y = "Importance") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma, limits = c(0, max(importance_df$Importance) * 1.2))
}

