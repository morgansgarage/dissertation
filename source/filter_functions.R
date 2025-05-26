initialize_counts <- function() {
  dfcounts <- data.frame(FilterName = character(), Count = numeric(), stringsAsFactors = FALSE)
  return(dfcounts)
}

filter_ICorPM <- function(df) {
  df_filtered <- df %>% dplyr::filter(ICorPM == "Individual contributor")
  return(df_filtered)
}

filter_MainBranch <- function(df) {
  df_filtered <- df %>% dplyr::filter(MainBranch == "I am a developer by profession")
  return(df_filtered)
}

filter_Employment <- function(df) {
  df_filtered <- df %>% dplyr::filter(Employment == "Employed, full-time")
  return(df_filtered)
}

filter_DevType <- function(df) {
  df_filtered <- df %>%
    dplyr::filter(str_detect(DevType, "Developer, front-end") |
             str_detect(DevType, "Developer, back-end") |
             str_detect(DevType, "Developer, full-stack") |
             str_detect(DevType, "Developer, desktop or enterprise applications"))
  return(df_filtered)
}

filter_OrgSize <- function(df) {
  df_filtered <- subset(df, OrgSize != "NA" & OrgSize != "I don’t know" & OrgSize != "Just me - I am a freelancer, sole proprietor, etc.")
  return(df_filtered)
}

filter_TimeSearching <- function(df) {
  df_filtered <- subset(df, TimeSearching != "NA") 
  return(df_filtered)
}

filter_WorkExp <- function(df) {
  df_filtered <- subset(df, !is.na(WorkExp))
  return(df_filtered)
}

filter_Cty <- function(df) {
  df_filtered <- subset(df, Country != "Nomadic")
  return(df_filtered)
}

filter_RemoteWork <- function(df) {
  df_filtered <- subset(df, RemoteWork != "NA")
  return(df_filtered)
}

filter_Population <- function(df) {
  df_filtered <- subset(df, !is.na(pop_est))
  return(df_filtered)
}

filter_Age <- function(df) {
  df_filtered <- subset(df, !is.na(Age_Int))
  return(df_filtered)
}

filter_Frequency_1 <- function(df) {
  df_filtered <- subset(df, !is.na(Frequency_1_Int))
  return(df_filtered)
}

filter_Frequency_2 <- function(df) {
  df_filtered <- subset(df, !is.na(Frequency_2_Int))
  return(df_filtered)
}

filter_Frequency_3 <- function(df) {
  df_filtered <- subset(df, !is.na(Frequency_3_Int))
  return(df_filtered)
}

filter_Knowledge <- function(df) {
  df_filtered <- subset(df, !is.na(Knowledge_1_Int))
  df_filtered <- subset(df, !is.na(Knowledge_2_Int))
  df_filtered <- subset(df, !is.na(Knowledge_3_Int))
  df_filtered <- subset(df, !is.na(Knowledge_4_Int))
  df_filtered <- subset(df, !is.na(Knowledge_5_Int))
  df_filtered <- subset(df, !is.na(Knowledge_6_Int))
  df_filtered <- subset(df, !is.na(Knowledge_7_Int))
  df_filtered <- subset(df, !is.na(Knowledge_8_Int))
  return(df_filtered)
}

filter_plot <- function(df) {
  ggplot(df, aes(x = reorder(FilterName, +Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = scales::comma(Count)), hjust = -0.2, color = "black", size = 3) +
    coord_flip() +
    labs(title = "", x = "Filter Step", y = "Count") +
    theme_minimal() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Add space for labels
}
