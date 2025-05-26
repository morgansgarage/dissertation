#Clear the Environment
#rm(data)
#rm(list = ls())

#Install Packages
install.packages("countrycode")
install.packages("rworldmap") 
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("sf")
install.packages("MatchIt")
install.packages("lmtest")
install.packages("lm.beta")
install.packages("car")
install.packages("sandwich")
install.packages("tidyverse")
install.packages("conflicted")
install.packages("ranger")
install.packages("scales")
install.packages("Hmisc")
install.packages("psych")


#Libraries
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(stringr)
library(ggplot2)
library(MatchIt)
library(lmtest)
library(lm.beta)
library(car)
library(sandwich)
library(lavaan)
library(tidyverse)
library(conflicted)
library(ranger)
library(scales)
library(Hmisc)
library(psych)

#setwd("/Users/scottmorgan/Documents/Marymount/Dissertation/Project/aicodedevprod/")

#Source Files
source("create_dataset.R")
source("column_functions.R")
source("filter_functions.R")
source("confirmatory_functions.R")
source("exploratory_functions.R")
source("predictive_functions.R")
source("impact_functions.R")

################################################################################
#Create the dataset
data <- create_dataset()

################################################################################
# Set up Computed and Transformed Columns

data <- update_regions(data)
data <- setup_common_fields(data)
data <- setup_other_fields_2023(data)
data <- setup_other_fields_2024(data)
data <- combine_AID_columns(data)
data <- combine_AIC_columns(data)
data <- setup_aws(data)
data <- setup_cty_fields(data)
data <- add_world_data(data)
data <- setup_age_int(data)
data <- setup_orgsize_int(data)
data <- convert_edlevel_to_int(data)
data <- setup_frequency_int(data, "Frequency_1", "Frequency_1_Int")
data <- setup_frequency_int(data, "Frequency_2", "Frequency_2_Int")
data <- setup_frequency_int(data, "Frequency_3", "Frequency_3_Int")
data <- convert_to_int_down(data, "Knowledge_1")
data <- convert_to_int_down(data, "Knowledge_2")
data <- convert_to_int_down(data, "Knowledge_3")
data <- convert_to_int_down(data, "Knowledge_4")
data <- convert_to_int_down(data, "Knowledge_5")
data <- convert_to_int_down(data, "Knowledge_6")
data <- convert_to_int_down(data, "Knowledge_7")
data <- convert_to_int_down(data, "Knowledge_8")

################################################################################
# Filter the data

dfcounts <- initialize_counts()

data <- filter_ICorPM(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Ind Contributor", Count = nrow(data)))

data <- filter_MainBranch(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Develper Type", Count = nrow(data)))

data <- filter_Employment(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Employment", Count = nrow(data)))

data <- filter_DevType(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Developer Role", Count = nrow(data)))

data <- filter_OrgSize(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Org Size", Count = nrow(data)))

data <- filter_TimeSearching(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "TimeSearching", Count = nrow(data)))

data <- filter_WorkExp(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "WorkExp", Count = nrow(data)))

data <- filter_Cty(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Country", Count = nrow(data)))

data <- filter_RemoteWork(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Work Location", Count = nrow(data)))

data <- filter_Population(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Population", Count = nrow(data)))

data <- filter_Frequency_1(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Frequency 1", Count = nrow(data)))

data <- filter_Frequency_2(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Frequency 2", Count = nrow(data)))

data <- filter_Frequency_3(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Frequency 3", Count = nrow(data)))

data <- filter_Age(data)
dfcounts <- rbind(dfcounts, data.frame(FilterName = "Age", Count = nrow(data)))

# Add columns
data <- create_bit_columns(data, "LanguageHaveWorkedWith")
data <- calculate_row_mean(data, columns = c("Frequency_1_Int", "Frequency_2_Int", "Frequency_3_Int"), new_column_name = "Frequency_Mean")
data <- convert_remotework_to_int(data)
data <- convert_economy_to_int(data)

dfcounts <- rbind(dfcounts, data.frame(FilterName = "Programming Language", Count = nrow(data)))

#Plot the filtering process
filter_plot(dfcounts)

rm(dfcounts)

################################################################################
# Factor Analysis
# Demographic Factor Analysis
data_subset <- data[, c("TSX", "AID", "EXP", "PLC", "CTY_INT", "Age_Int", "RemoteWork_Int", "pop_est", "Orgsize_Int", "economy_int", "Frequency_Mean", "EdLevel_Int")]
KMO(data_subset)
cortest.bartlett(cor(data_subset), n = nrow(data_subset))
efa_results <- fa(data_subset, nfactors = 6, rotate = "varimax", fm = "ml")
print(efa_results$loadings)
#fa.diagram(efa_results)

rm(efa_results, data_subset)
#End Demographic Factor Analysis

#Factor Analysis Pogramming Languages
data_subset_p <- data[, c("Python", "JavaScript", "TypeScript", "Java", "C", "C_", "C__", "SQL", "Ruby", "PHP", "Go", "HTML_CSS", "Bash_Shell__all_shells_", "Ada", "Clojure", "Elixir", 
                          "Lisp", "OCaml", "Raku", "Scala", "Swift", "Zig", "Solidity", "Perl", "Dart", "Haskell", "Kotlin", "Lua", "PowerShell", "Assembly","Rust", "VBA","R","F_",
                          "MATLAB", "Groovy", "Crystal", "Delphi", "Fortran", "Prolog", "SAS", "Apex", "Julia", "GDScript", "Erlang", "Cobol","Flow", "Nim", "APL", "MicroPython", "Zephyr", "Objective_C", "Visual_Basic___Net_")]
KMO(data_subset_p)
cortest.bartlett(cor(data_subset_p), n = nrow(data_subset_p))
efa_results_p <- fa(data_subset_p, nfactors = 10, rotate = "varimax", fm = "ml")
print(efa_results_p$loadings)

rm(efa_results_p, data_subset_p)

#Needed to explain the Rust and R grouping
cor(data$Rust, data$R, use = "complete.obs")

#Group languagse
data <- group_programming_languages(data, drop_original = FALSE) 
#End Factor Analysis Programming Languages

################################################################################
# Descriptives and Correlation Matrixn

descriptive_stats_aid <- get_descriptive_stats_aid(data)
print(descriptive_stats_aid)
rm(descriptive_stats_aid)

print_descriptive_box_plot(data)
get_exp_stats_by_aid(data)

# Correlation Matrix and Descriptives
correlation_matrix <- get_correlation_matrix_full(data)
print(correlation_matrix)

cor_matrix_with_significance <- add_significance_stars(correlation_matrix$r, correlation_matrix$p)
print(cor_matrix_with_significance)

file_path <- "cor_matrix_with_significance.csv"
write.csv(cor_matrix_with_significance, file = file_path, row.names = TRUE)
cat("Correlation matrix saved to:", file_path)
#print(correlation_matrix)
rm(correlation_matrix, cor_matrix_with_significance)

descriptive_stats_tsx <- get_descriptive_stats(data, "TSX")
descriptive_stats_aid <- get_descriptive_stats(data, "AID")
descriptive_stats_plc <- get_descriptive_stats(data, "PLC")
descriptive_stats_exp <- get_descriptive_stats(data, "EXP")
descriptive_stats_cty <- get_descriptive_stats(data, "CTY_INT")
descriptive_stats_eco <- get_descriptive_stats(data, "economy_int")
descriptive_stats_remotework <- get_descriptive_stats(data, "RemoteWork_Int")
descriptive_stats_pop <- get_descriptive_stats(data, "pop_est")
descriptive_stats_edlevel <- get_descriptive_stats(data, "EdLevel_Int")
descriptive_stats_orgsize <- get_descriptive_stats(data, "Orgsize_Int")
descriptive_stats_age <- get_descriptive_stats(data, "Age_Int")
descriptive_stats_frq <- get_descriptive_stats(data, "Frequency_Mean")

descriptive_stats_cod <- get_descriptive_stats(data, "Codeium")
descriptive_stats_github <- get_descriptive_stats(data, "GitHub_Copilot")
descriptive_stats_replit <- get_descriptive_stats(data, "Replit_Ghostwriter")
descriptive_stats_tabnine <- get_descriptive_stats(data, "Tabnine")
descriptive_stats_whispr <- get_descriptive_stats(data, "Whispr_AI")

descriptive_stats_frontend <- get_descriptive_stats(data, "Frontend_JS_Langs")
descriptive_stats_systems <- get_descriptive_stats(data, "Systems_Langs")
descriptive_stats_rust <- get_descriptive_stats(data, "Rust_R_Group")
descriptive_stats_functional <- get_descriptive_stats(data, "Functional_Langs")
descriptive_stats_mobile <- get_descriptive_stats(data, "Mobile_Langs")
descriptive_stats_scripting <- get_descriptive_stats(data, "Scripting_Langs")
descriptive_stats_python <- get_descriptive_stats(data, "Python")
descriptive_stats_java <- get_descriptive_stats(data, "Java")
descriptive_stats_php <- get_descriptive_stats(data, "PHP")
descriptive_stats_c_ <- get_descriptive_stats(data, "C_")
descriptive_stats_sql <- get_descriptive_stats(data, "SQL")
descriptive_stats_ruby <- get_descriptive_stats(data, "Ruby")
descriptive_stats_go <- get_descriptive_stats(data, "Go")

print(descriptive_stats_tsx)
print(descriptive_stats_aid)
print(descriptive_stats_age)
print(descriptive_stats_exp)
print(descriptive_stats_edlevel)
print(descriptive_stats_plc)
print(descriptive_stats_cty)
print(descriptive_stats_eco)
print(descriptive_stats_pop)
print(descriptive_stats_remotework)
print(descriptive_stats_orgsize)
print(descriptive_stats_frq)
print(descriptive_stats_cod)
print(descriptive_stats_github)
print(descriptive_stats_replit)
print(descriptive_stats_tabnine)
print(descriptive_stats_whispr)
print(descriptive_stats_frontend)
print(descriptive_stats_systems)
print(descriptive_stats_rust)
print(descriptive_stats_functional)
print(descriptive_stats_mobile)
print(descriptive_stats_scripting)
print(descriptive_stats_python)
print(descriptive_stats_java)
print(descriptive_stats_php)
print(descriptive_stats_c_)
print(descriptive_stats_sql)
print(descriptive_stats_ruby)
print(descriptive_stats_go)


rm(descriptive_stats_aid, descriptive_stats_tsx, descriptive_stats_plc, descriptive_stats_exp, descriptive_stats_cty, descriptive_stats_remotework, descriptive_stats_pop, descriptive_stats_edlevel, descriptive_stats_orgsize, descriptive_stats_age, descriptive_stats_frq, descriptive_stats_eco, 
   descriptive_stats_cod, descriptive_stats_github, descriptive_stats_replit, descriptive_stats_tabnine, descriptive_stats_whispr,
   descriptive_stats_frontend, descriptive_stats_systems, descriptive_stats_rust, descriptive_stats_functional, descriptive_stats_mobile,
   descriptive_stats_scripting, descriptive_stats_python, descriptive_stats_java, descriptive_stats_php, descriptive_stats_c_, 
   descriptive_stats_sql, descriptive_stats_ruby,descriptive_stats_go)


################################################################################
# Linear Regression Models

# 1. Confirmatory Model
perform_linear_regression_main(data)

# 2. Demographics
perform_linear_regression_demographics(data)

# 3. Demographics + AI Tools
perform_linear_regression_demographics_tools(data)

# 4. Demographics + AI Tools + Programming Languages
perform_linear_regression_demographics_tools_langs(data)

# 5. Demographics + Interactions
perform_linear_regression_demographics_interactions(data)

# 6. Demographics + AI Tools + Interactions
perform_linear_regression_demographics_tools_interactions(data)

# 7. Demographics + AI Tools + Programming Languages + Interactions
perform_linear_regression_demographics_tools_langs_interactions_beta(data)

# 8. Demographics + AI Tools + Programming Languages + Interactions + AID Interactions (All three)
#perform_linear_regression_ai_langs_interactions(data)
source("confirmatory_functions.R")
perform_linear_regression_all_standardized(data)

# Plot the experience interaction
create_experience_interaction_plot(data)

# Curvilinear Test
# 1. Create the squared term
data$AID_Squared <- data$AID^2

# 2. Run the simple curvilinear regression
curvilinear_model <- lm(TSX ~ AID + AID_Squared, data = data)

# 3. Summarize the results
summary(curvilinear_model)

# Optional: Check the plot to visually assess curvature
# plot(data$AID, data$TSX, main="Scatterplot with Curvilinear Fit", xlab="AID", ylab="TSX")
rm(curvilinear_model)



################################################################################
# Random Forest 

response <- "TSX"
#predictors <- c("AID", "EdLevel", "RemoteWork", "OrgSize", "PLC", "WorkExp", "Frequency_Mean", "pop_est", "Age_Int")
predictors <- c("EXP", "PLC", "RemoteWork", "OrgSize", "EdLevel", "Frequency_Mean", "pop_est",
                "Codeium", "GitHub_Copilot", "Replit_Ghostwriter", "Tabnine", "Whispr_AI",
                #"AID",
                "Frontend_JS_Langs", "Systems_Langs", "Rust_R_Group", "Functional_Langs", "Mobile_Langs", "Scripting_Langs", "Python", "Java", "PHP", "SQL", "Go"
                )
result <- train_random_forest(data = data, response = response, predictors = predictors)
rf_model <- result$model
print(rf_model)

# Variable Importance
importance_values <- rf_model$variable.importance
print(importance_values)

#Plot
plot_variable_importance(importance_values, title = "")

rm(result, rf_model)
# End Random Forest ############

################################################################################
# AI Tools Analyses 
bit_columns <- c("Codeium", "GitHub_Copilot", "Replit_Ghostwriter", "Tabnine", "Whispr_AI")
results_df <- calculate_bit_stats(data, bit_columns)
print(results_df)

create_aic_bit_column_barchart_mean_with_count_labels(data, bit_columns)
rm(bit_columns, results_df)
# End AI Tool Analyses #########


################################################################################
# Country Analyses

cty_mean_result <- cty_mean_data(data)
print(cty_mean_result,n=25)

plot_top_cty_v2(cty_mean_result, "", "steelblue")
plot_avg_tsx_world_map(data)
create_interaction_plot(data)

rm(cty_mean_result)
# End CTY ##########