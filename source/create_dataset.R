create_dataset <- function() {
  # Read in the column names from columns.csv to create the empty dataframe
  columns <- read.csv("data//columns.csv", stringsAsFactors = FALSE, header = TRUE)
  col_names <- columns[[1]]
  
  # Create an empty dataframe with the desired columns
  data <- data.frame(matrix(ncol = length(col_names), nrow = 0))  
  colnames(data) <- col_names
  
  # Read in the datasets for 2022, 2023, and 2024 with SurveyYear
  #data2 <- read_and_select("data//data2.csv", col_names, 2022)
  data3 <- read_and_select("data//data3.csv", col_names, 2023)
  data4 <- read_and_select("data//data4.csv", col_names, 2024)
  
  # Combine all datasets into one
  #data <- bind_rows(data2, data3, data4)
  data <- bind_rows(data3, data4)
  data <- data[ , !names(data) %in% "Country.1"]
  
  return(data)
}

read_and_select <- function(file_path, col_names, survey_year) {
  # Read the dataset
  df <- read.csv(file_path, stringsAsFactors = FALSE, header = TRUE)
  
  # Select only the columns that exist in the current dataset and in col_names
  available_cols <- intersect(colnames(df), col_names)
  df <- df[ , available_cols]
  
  # Add missing columns that are in col_names but not in the dataset as NA
  missing_cols <- setdiff(col_names, colnames(df))
  df[missing_cols] <- NA
  
  # Reorder columns to match col_names
  df <- df[col_names]
  
  # Add SurveyYear column
  df$SurveyYear <- survey_year
  
  return(df)
}