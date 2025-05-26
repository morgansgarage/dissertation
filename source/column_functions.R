update_regions <- function(df) {
  # Read in the country-region lookup from a CSV file
  country_region_lookup <- read.csv("data//distinct_countries_with_regions.csv", stringsAsFactors = FALSE)
  country_region_lookup$Country <- trimws(country_region_lookup$Country)
  
  df <- df %>%
    mutate(Country = trimws(Country)) %>%  # Clean country names in the main dataframe
    left_join(country_region_lookup, by = "Country") %>%
    mutate(Region = coalesce(Region.y, Region.x)) %>%  # Use the updated region if availablef
    dplyr::select(-Region.x, -Region.y) 
  
  return(df)
}

setup_common_fields <- function(df) {
  df <- df %>%
    mutate(TSX = case_when(
      TimeSearching == "Less than 15 minutes a day" ~ 15,
      TimeSearching == "15-30 minutes a day" ~ 30,
      TimeSearching == "30-60 minutes a day" ~ 60,
      TimeSearching == "60-120 minutes a day" ~ 120,
      TimeSearching == "Over 120 minutes a day" ~ 180,
      TRUE ~ NA_real_  # Default case if no conditions are met
    ))
  
  df$TSC <- df$TSX
  df$EXP <- df$WorkExp
  df$PLC <- sapply(strsplit(df$LanguageHaveWorkedWith, ";"), length)
  
  return(df)
}

setup_other_fields_2023 <- function(df) {
  df$AID1 <- ifelse(is.na(df$AIDevHaveWorkedWith), 0, 1)    
  ai_common_tools <- c("AWS CodeWhisperer", "Codeium", "GitHub Copilot", "Replit Ghostwriter", "Tabnine", "Whispr AI")
  df$AIC2023 <- ifelse(grepl(paste(ai_common_tools, collapse = "|"), df$AIDevHaveWorkedWith), 1, 0)
  
  for (tool in ai_common_tools) {
    # Create a sanitized column name by replacing spaces and special characters with underscores
    tool_column <- gsub("[^[:alnum:]]", "_", tool)
    
    # Populate the new column with 1 if the tool is found in AIDevHaveWorkedWith, otherwise 0
    df[[tool_column]] <- ifelse(grepl(tool, df$AIDevHaveWorkedWith, fixed = TRUE), 1, 0) 
  }
  
  return(df) 
}

setup_other_fields_2024 <- function(df) {
  # List of AI tools
  ai_tools <- c("AskCodi", "Amazon Q", "Codeium", "Cody", "GitHub Copilot", "Lightning AI", "Replit Ghostwriter", "Tabnine", "Whispr AI")
  
  # Create AID2 based on whether any of the ai_tools are found in AISearchDevHaveWorkedWith
  df$AID2 <- ifelse(grepl(paste(ai_tools, collapse = "|"), df$AISearchDevHaveWorkedWith), 1, 0)
  
  # Common AI tools for 2024
  ai_common_tools <- c("Amazon Q", "Codeium", "GitHub Copilot", "Replit Ghostwriter", "Tabnine", "Whispr AI")
  
  # Create AIC2024
  df$AIC2024 <- ifelse(grepl(paste(ai_common_tools, collapse = "|"), df$AISearchDevHaveWorkedWith), 1, 0)
  
  # Update columns for each tool without overwriting the 2023 data
  for (tool in ai_common_tools) {
    tool_column <- gsub("[^[:alnum:]]", "_", tool)
    
    # Create a temporary variable to store matches for the 2024 data
    df_temp <- ifelse(grepl(tool, df$AISearchDevHaveWorkedWith), 1, 0)
    
    # Ensure the column exists in the dataframe and handle cases where it's missing (for first time creation)
    if (!tool_column %in% names(df)) {
      df[[tool_column]] <- 0  # Initialize the column with 0s if it doesn't exist
    }
    
    # Preserve 2023 data: if the 2023 value is 1, keep it; otherwise, use the 2024 data
    df[[tool_column]] <- ifelse(df[[tool_column]] == 1, 1, df_temp)
  }
  
  return(df)
}

combine_AID_columns <- function(df) {
  df$AID <- ifelse(df$AID1 == 1 | df$AID2 == 1, 1, 0)
  
  df <- df %>%
    dplyr::select(-AID1, -AID2)
  
  return(df)
}

combine_AIC_columns <- function(df) {
  df$AIC <- ifelse(df$AIC2023 == 1 | df$AIC2024 == 1, 1, 0)
  
  df <- df %>%
    dplyr::select(-AIC2023, -AIC2024)
  
  return(df)
}

setup_aws <-function(df) {
  df$AWS <- ifelse(df$AWS_CodeWhisperer == 1 | df$Amazon_Q == 1, 1, 0)
  df <- df %>%
    dplyr::select(-AWS_CodeWhisperer, -Amazon_Q)
  
  return(df)
}

setup_cty_fields <- function(df) {
  df$CTY <- df$Country
  df$CTY_INT <- as.integer(factor(df$CTY))
  df$CTY_ISO3A <- countrycode(df$CTY, origin = 'country.name', destination = 'iso3c')
  df$CTY_ISO3A[df$CTY == "Kosovo"] <- "XKX"
  
  return(df)
}

add_world_data <- function(df) {
  # Load the world data from rnaturalearth
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Select relevant columns from the world dataset
  world_data <- world %>%
    st_drop_geometry() %>%  
    dplyr::select(iso_a3_eh, pop_est, gdp_md, economy, income_grp)
  
  # Merge your dataset (df) with the world data, matching CTY_ISO3A with iso_a3
  df <- df %>%
    left_join(world_data, by = c("CTY_ISO3A" = "iso_a3_eh"))
  
  # Return the updated dataframe
  return(df)
}

setup_age_int <- function(df) {
  df <- df %>%
    mutate(Age_Int = case_when(
      Age == "Under 18 years old" ~ 18,
      Age == "18-24 years old" ~ 24,
      Age == "25-34 years old" ~ 34,
      Age == "35-44 years old" ~ 44,
      Age == "45-54 years old" ~ 54,
      Age == "55-64 years old" ~ 64,
      Age == "65 or older" ~ 68,
      Age == "Prefer not to say" ~ NA,
      TRUE ~ NA_real_  # For any other values, assign NA
    ))
  
  return(df)
}

setup_orgsize_int <- function(df) {
  df <- df %>%
    mutate(Orgsize_Int = case_when(
      OrgSize == "2 to 9 employees" ~ 9,
      OrgSize == "10 to 19 employees" ~ 19,
      OrgSize == "20 to 99 employees" ~ 99,
      OrgSize == "100 to 499 employees" ~ 499,
      OrgSize == "500 to 999 employees" ~ 999,
      OrgSize == "1,000 to 4,999 employees" ~ 4999,
      OrgSize == "5,000 to 9,999 employees" ~ 9999,
      OrgSize == "10,000 or more employees" ~ 20000,
      TRUE ~ NA_real_  # For any other values, assign NA
    ))
  
  return(df)
}

convert_edlevel_to_int <- function(df) {
  df <- df %>%
    mutate(EdLevel_Int = case_when(
      EdLevel == "Primary/elementary school" ~ 5,
      EdLevel == "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)" ~ 12,
      EdLevel == "Some college/university study without earning a degree" ~ 13,
      EdLevel == "Associate degree (A.A., A.S., etc.)" ~ 14,
      EdLevel == "Bachelor’s degree (B.A., B.S., B.Eng., etc.)" ~ 16,
      EdLevel == "Master’s degree (M.A., M.S., M.Eng., MBA, etc.)" ~ 18,
      EdLevel == "Professional degree (JD, MD, Ph.D, Ed.D, etc.)" ~ 21,
      EdLevel == "Something else" ~ 8,
      TRUE ~ NA_real_  # Assign NA for any unhandled values
    ))
  
  return(df)
}

convert_remotework_to_int <- function(df) {
  df <- df %>%
    mutate(RemoteWork_Int = case_when(
      RemoteWork == "Remote" ~ 3,
      RemoteWork == "Hybrid (some remote, some in-person)" ~ 2,
      RemoteWork == "In-person" ~ 1,
      TRUE ~ NA_real_  # Assign NA for any unhandled values
    ))
  
  return(df)
}

convert_economy_to_int <- function(df) {
  df <- df %>%
    mutate(economy_int = case_when(
      economy == "1. Developed region: G7" ~ 110,
      economy == "2. Developed region: nonG7" ~ 100,
      economy == "3. Emerging region: BRIC" ~ 75,
      economy == "4. Emerging region: MIKT" ~ 70,
      economy == "5. Emerging region: G20" ~ 65,
      economy == "6. Developing region" ~ 50,
      economy == "7. Least developed region" ~ 25,
      TRUE ~ NA_real_  # Assign NA for any unhandled values
    ))
  
  return(df)
}

setup_frequency_int <- function(df, fromColName, toColName) {
  df <- df %>%
    mutate(!!toColName := case_when(
      .data[[fromColName]] == "Never" ~ 0,
      .data[[fromColName]] == "1-2 times a week" ~ 2,
      .data[[fromColName]] == "3-5 times a week" ~ 5,
      .data[[fromColName]] == "6-10 times a week" ~ 10,
      .data[[fromColName]] == "10+ times a week" ~ 15,
      TRUE ~ NA_real_  # For any other values, assign NA
    ))
  
  return(df)
}

convert_to_int_down <- function(df, column_name) {
  df[[paste0(column_name, "_Int")]] <- dplyr::case_when(
    df[[column_name]] == "Strongly Agree" ~ 5,
    df[[column_name]] == "Agree" ~ 4,
    df[[column_name]] == "Neither Agree or Disagree" ~ 3,
    df[[column_name]] == "Disagree" ~ 2,
    df[[column_name]] == "Strongly Disagree" ~ 1,
    TRUE ~ NA_integer_  # Handle unexpected or missing values
  )
  return(df)
}

update_regions <- function(df) {
  # Read in the country-region lookup from a CSV file
  country_region_lookup <- read.csv("data//distinct_countries_with_regions.csv", stringsAsFactors = FALSE)
  country_region_lookup$Country <- trimws(country_region_lookup$Country)
  
  df <- df %>%
    mutate(Country = trimws(Country)) %>%  # Clean country names in the main dataframe
    left_join(country_region_lookup, by = "Country") %>%
    mutate(Region = coalesce(Region.y, Region.x)) %>%  # Use the updated region if availablef
    dplyr::select(-Region.x, -Region.y) 
  
  return(df)
}

get_unique_values_for_column <- function(df, column_name) {
  unique_values <- df %>%
    separate_rows(!!sym(column_name), sep = ";") %>%
    mutate(!!sym(column_name) := trimws(!!sym(column_name))) %>%
    distinct(!!sym(column_name)) %>%
    pull(!!sym(column_name))
  
  return(unique_values)
}

sanitize_column_name <- function(colname) {
  # Replace non-alphanumeric characters with underscores
  sanitized_name <- gsub("[^[:alnum:]]", "_", colname)
  return(sanitized_name)
}

create_bit_columns <- function(df, column_name) {
  # Ensure the column is character type to handle NA properly
  df[[column_name]] <- as.character(df[[column_name]])
  
  # Filter out rows where the column is NA
  df <- df %>% dplyr::filter(!is.na(df[[column_name]]))
  
  # Get a unique list of languages
  unique_languages <- get_unique_values_for_column(df, column_name)
  
  # Iterate over each language and create a bit column
  for (language in unique_languages) {
    sanitized_name <- sanitize_column_name(language)
    
    # Create the bit column with the sanitized name using fixed = TRUE
    df[[sanitized_name]] <- ifelse(
      grepl(language, df[[column_name]], fixed = TRUE), 
      1, 
      0
    )
  }
  return(df)
}

calculate_row_mean <- function(df, columns, new_column_name) {
  # Check if all specified columns are in the data frame
  if (!all(columns %in% colnames(df))) {
    stop("Data frame does not contain all specified columns.")
  }
  
  # Calculate the row-wise mean for the specified columns
  df[[new_column_name]] <- rowMeans(df[, columns], na.rm = TRUE)
  
  return(df)
}