cty_mean_data <- function(df) {
  library(dplyr)
  
  df <- df %>%
    group_by(CTY) %>%
    dplyr::filter(n() >= 30) %>%
    ungroup()
  
  mean_tsearch_by_cty <- df %>%
    group_by(CTY, AID) %>%
    summarise(Mean_TSX = mean(TSX, na.rm = TRUE), CountTSX = n()) %>%
    ungroup()
  
  mean_tsearch_wide_count <- mean_tsearch_by_cty %>%
    dplyr::select(CTY, AID, CountTSX) %>%
    pivot_wider(names_from = AID, values_from = CountTSX, names_prefix = "Count_")
  
  mean_tsearch_wide_mean <- mean_tsearch_by_cty %>%
    dplyr::select(CTY, AID, Mean_TSX) %>%
    pivot_wider(names_from = AID, values_from = Mean_TSX, names_prefix = "TimeSearching_")
  
  mean_tsearch_wide <- left_join(mean_tsearch_wide_mean, mean_tsearch_wide_count, by = "CTY")
  
  mean_tsearch_wide <- mean_tsearch_wide %>%
    mutate(Difference = TimeSearching_1 - TimeSearching_0)
  
  top_10_diff <- mean_tsearch_wide %>%
    arrange(Difference) %>%
    dplyr::slice(1:25)
  
  return(top_10_diff)
}

plot_cty_comparison <- function (cty_list, df, title, color) {
  top_10_cty <- head(cty_list, 10)
  
  usa_mean_tsx <- data %>%
    dplyr::filter(CTY == "United States of America") %>%
    summarise(mean_TSX = mean(TSX, na.rm = TRUE)) %>%
    pull(mean_TSX)
  
  df_long <- top_10_cty %>%
    pivot_longer(cols = c("TimeSearching_0", "TimeSearching_1"),
                 names_to = "TimeSearching", values_to = "Value")
  
  ggplot(df_long, aes(x = CTY, y = Value, fill = TimeSearching)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_hline(aes(yintercept = usa_mean_tsx), color = "black", linetype = "dashed", size = 1) +
    annotate("text", x = 1, y = usa_mean_tsx + 2, label = "USA Mean", color = "black", size = 4, hjust = 0) + 
    scale_fill_manual(values = c("TimeSearching_0" = "lightseagreen", "TimeSearching_1" = "lightcoral"),
                      labels = c("TimeSearching_0" = "No AI", "TimeSearching_1" = "AI")) +
    labs(title = title, x = "Country", y = "Time Searching", fill = "Time Searching") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

plot_top_cty_v2 <- function (df, title, color) {
  df$PositiveDifference <- abs(df$Difference)
  
  ggplot(df, aes(x = reorder(CTY, PositiveDifference), y = PositiveDifference)) +
    geom_bar(stat = "identity", fill = color) +
    geom_text(aes(label = round(PositiveDifference, 2)), 
              hjust = -0.2, # Adjust horizontal position to ensure labels are outside the bars
              size = 3) +  # Adjust size as needed
    coord_flip() + # Flip coordinates for horizontal bars
    labs(title = title,
         x = "Country",
         y = "Time Savings") +
    theme_minimal() +
    theme(
      plot.margin = margin(10, 50, 10, 10) # Increase right margin
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) # Add spacing on the right side
}

plot_avg_tsx_world_map <- function(df) {
  # Step 1: Filter countries with a minimum count of 50
  avg_tsx_by_country <- df %>%
    group_by(CTY_ISO3A) %>%
    dplyr::filter(n() >= 30) %>%  # Filter for countries with at least 50 records
    group_by(CTY_ISO3A, AID) %>%
    summarise(avg_TSX = mean(TSX, na.rm = TRUE)) %>%
    ungroup()
  
  # Step 2: Pivot the data to have separate columns for AID=0 and AID=1
  avg_tsx_by_country <- avg_tsx_by_country %>%
    pivot_wider(names_from = AID, values_from = avg_TSX, names_prefix = "AID_")
  
  # Step 3: Calculate time savings
  avg_tsx_by_country <- avg_tsx_by_country %>%
    mutate(time_savings = AID_0 - AID_1)
  
  # Step 4: Load the world map data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Step 5: Merge world map with the average TSX by country
  world <- left_join(world, avg_tsx_by_country, by = c("iso_a3" = "CTY_ISO3A"))
  #return(avg_tsx_by_country)
  # Step 6: Plot the world map with time savings using a more granular color scale
  # ggplot(data = world) +
  #   geom_sf(aes(fill = time_savings), color = "black") +  # Fill based on time savings
  #   scale_fill_viridis_c(
  #     option = "virdis",         # Use the virdis color scale
  #     na.value = "grey",         # Color for missing data
  #     name = "Time Savings",     # Label for the color scale
  #     limits = c(-32, 15),       # Set the limits for the color scale
  #   ) +
   ggplot(data = world) +
   geom_sf(aes(fill = time_savings), color = "black") +  # Fill based on time savings
   scale_fill_gradientn(
     colors = c("darkred", "firebrick", "darkorange" , "goldenrod", "gold", "darkgreen", "mediumseagreen", "deepskyblue"),  # Custom color gradient
    na.value = "grey",        # Color for missing data
     name = "Time Savings",    # Label for the color scale
     limits = c(-32, 15)       # Set the limits for the color scale
   ) +
    theme_minimal() +
    labs(title = "") +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
}

calculate_bit_stats_by_year <- function(df, bit_columns) {
  # Loop through each bit column and calculate count and mean of TSX grouped by SurveyYear
  results <- lapply(bit_columns, function(col) {
    df %>%
      dplyr::filter(!!sym(col) == 1) %>%  # Use dplyr::filter to avoid conflicts
      group_by(SurveyYear) %>%  # Group by SurveyYear
      summarise(
        Count = n(),
        Mean_TSX = mean(TSX, na.rm = TRUE)
      ) %>%
      mutate(Feature = col)
  })
  
  # Combine results into a single dataframe
  results_df <- bind_rows(results)
  
  return(results_df)
}

calculate_bit_stats <- function(df, bit_columns) {
  # Loop through each bit column and calculate count and mean of TSX
  results <- lapply(bit_columns, function(col) {
    df %>%
      dplyr::filter(!!sym(col) == 1) %>%  # Use dplyr::filter to avoid conflicts
      summarise(
        Count = n(),
        Mean_TSX = mean(TSX, na.rm = TRUE)
      ) %>%
      mutate(Feature = col)
  })
  
  # Combine results into a single dataframe
  results_df <- bind_rows(results)
  
  return(results_df)
}

create_aic_bit_column_barchart_mean_with_count_labels <- function(df, bit_columns) {
  # Calculate the mean TSX and count for each tool (without SurveyYear)
  results <- lapply(bit_columns, function(col) {
    df %>%
      dplyr::filter(!!sym(col) == 1) %>%  # Only consider rows where the bit column is 1
      summarise(
        Mean_TSX = mean(TSX, na.rm = TRUE),
        Count = n()  # Calculate the count of rows for the labels
      ) %>%
      mutate(Feature = col)
  })
  
  # Combine the results into a single dataframe
  results_df <- bind_rows(results)
  
  # Create the label text combining count and mean TSX
  results_df <- results_df %>%
    mutate(Label = paste0(Count, " (", round(Mean_TSX, 1), ")"))  # Round Mean TSX to 1 decimal
  
  # Create the bar chart using ggplot
  ggplot(results_df, aes(x = Feature, y = Mean_TSX)) +  # Removed fill argument
    geom_bar(stat = "identity", color = "black", fill = "steelblue") +  # Standard blue bars
    
    # Add text labels with both Count and Mean TSX
    geom_text(aes(label = Label), vjust = -0.5) +
    
    # Set y-axis limits using coord_cartesian
    coord_cartesian(ylim = c(55, 85)) +
    
    # Set labels and theme
    labs(title = "",
         x = "AI Tools",
         y = "Mean TSX") +
    
    # Center x-axis labels and improve appearance
    theme_minimal() +
    theme(axis.text.x = element_text(hjust = 0.5, vjust = 1, face = "bold"))
}

