# Heat, Air Pollution, and COVID-19 Health Outcomes Analysis
# Using ChAdOx, GCRO, and Earth Engine data with hybrid approach
# Focus: Heat Center Project - Environmental Justice Implications

# Research Question:
# What is the relationship between heat exposure, air pollution (PM2.5), and COVID-19 health outcomes 
# in Soweto during the pandemic period, with particular focus on respiratory symptoms, 
# fever incidence, and hospitalization rates?
# Secondary questions:
# 1. Are there seasonal patterns in these relationships?
# 2. Do temperature and PM2.5 thresholds exist beyond which health impacts increase significantly?
# 3. Are there lag effects between environmental exposures and health outcomes?

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)
library(reticulate)  # For Python integration with Earth Engine

# Progress tracking function
report_progress <- function(step, message) {
  timestamp <- format(Sys.time(), "%H:%M:%S")
  cat(paste0("[", timestamp, "] Step ", step, ": ", message, "\n"))
  flush.console()  # Force output to console immediately
}

# Source the hybrid data function
report_progress(1, "Loading hybrid data function")
source("heat_health_hybrid_approach.R")

#----- 1. LOAD AND PREPARE DATA -----#

# Load GCRO datasets
report_progress(2, "Loading GCRO datasets")
qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)
report_progress(2, "Successfully loaded GCRO datasets")

# Load ChAdOx data
report_progress(3, "Loading ChAdOx datasets")
load("ChAdOx data 2024-07-19.Rdata")
load("ChAdOx supp visit data 2024-07-19.Rdata")
report_progress(3, "Successfully loaded ChAdOx datasets")

# Extract and prepare COVID cases data
report_progress(4, "Processing COVID cases data")
covid_data <- covid_cases %>%
  mutate(
    record_id = as.character(record_id),
    visit_date = as.Date(fu_dat),
    covid_test_date = as.Date(fu_np_datetime1),
    covid_result = fu_res1,
    covid_hospitalized = ifelse(grepl("hosp|admit", fu_res1, ignore.case = TRUE), "Yes", "No")
  ) %>%
  select(record_id, visit_date, covid_test_date, covid_result, covid_hospitalized) %>%
  filter(!is.na(visit_date))

# Extract and prepare illness visit data
report_progress(5, "Processing illness visit data")
illness_data <- illness_visit %>%
  mutate(
    record_id = as.character(record_id),
    visit_date = as.Date(ev_dat),
    temperature = as.numeric(gsub("[^0-9.]", "", ev_temp)),  # Clean temperature data
    oxygen_saturation = as.numeric(gsub("[^0-9.]", "", ev_oxy)),
    respiratory_rate = as.numeric(gsub("[^0-9.]", "", ev_resp)),
    pulse = as.numeric(gsub("[^0-9.]", "", ev_pulse)),
    fever = ifelse(temperature > 38, "Yes", "No"),
    respiratory_symptoms = case_when(
      ev_resp_pe == "1" ~ "None",
      ev_resp_pe == "2" ~ "Mild",
      ev_resp_pe == "3" ~ "Moderate",
      ev_resp_pe == "4" ~ "Severe",
      TRUE ~ "Unknown"
    )
  ) %>%
  select(record_id, visit_date, temperature, oxygen_saturation, respiratory_rate, 
         pulse, fever, respiratory_symptoms) %>%
  filter(!is.na(visit_date))

# Extract and prepare symptom data
report_progress(6, "Processing symptom data")
if (exists("diary_card_illness_v2_3")) {
  # Function to interpret symptom codes - vectorized version
  interpret_symptom <- function(codes) {
    result <- rep(NA, length(codes))
    result[codes == "0"] <- "None"
    result[codes == "1"] <- "Mild"
    result[codes == "2"] <- "Moderate"
    result[codes == "3"] <- "Severe"
    result[!codes %in% c("0", "1", "2", "3") & !is.na(codes)] <- "Unknown"
    return(result)
  }
  
  symptom_data <- diary_card_illness_v2_3 %>%
    mutate(
      record_id = as.character(record_id),
      symptom_date = as.Date(dc_ill_date_v2_3),
      cough = interpret_symptom(dc_ill_cough_1_v2_3),
      smell_loss = interpret_symptom(dc_ill_smell_1_v2_3),
      taste_loss = interpret_symptom(dc_ill_taste_1_v2_3)
    ) %>%
    select(record_id, symptom_date, cough, smell_loss, taste_loss) %>%
    filter(!is.na(symptom_date))
}

# Extract demographic data
report_progress(7, "Processing demographic data")
if (exists("demographic_and_contact_information_v2_3")) {
  demo_data <- demographic_and_contact_information_v2_3 %>%
    mutate(
      record_id = as.character(record_id),
      age = as.numeric(age_calc),
      sex = case_when(
        sc_gender_v2_3 == "1" ~ "Male",
        sc_gender_v2_3 == "2" ~ "Female",
        TRUE ~ "Unknown"
      ),
      race = case_when(
        sc_race_v2_3 == "1" ~ "Black",
        sc_race_v2_3 == "2" ~ "White",
        sc_race_v2_3 == "3" ~ "Coloured",
        sc_race_v2_3 == "4" ~ "Indian",
        sc_race_v2_3 == "5" ~ "Other",
        TRUE ~ "Unknown"
      )
    ) %>%
    select(record_id, age, sex, race)
}

# Combine all ChAdOx datasets
report_progress(8, "Combining ChAdOx datasets")
chadox_combined <- illness_data %>%
  left_join(covid_data, by = c("record_id", "visit_date"))

if (exists("symptom_data")) {
  # Join symptom data by record_id and closest date
  chadox_combined <- chadox_combined %>%
    left_join(symptom_data, by = "record_id") %>%
    mutate(
      days_diff = abs(as.numeric(difftime(visit_date, symptom_date, units = "days")))
    ) %>%
    group_by(record_id, visit_date) %>%
    filter(days_diff == min(days_diff, na.rm = TRUE) | is.na(days_diff)) %>%
    ungroup() %>%
    select(-days_diff, -symptom_date)
}

if (exists("demo_data")) {
  chadox_combined <- chadox_combined %>%
    left_join(demo_data, by = "record_id")
}

# Clean and prepare the combined dataset
chadox_combined <- chadox_combined %>%
  mutate(
    # Ensure all variables are properly formatted
    fever = ifelse(is.na(fever), "No", fever),
    respiratory_symptoms = ifelse(is.na(respiratory_symptoms), "None", respiratory_symptoms),
    covid_hospitalized = ifelse(is.na(covid_hospitalized), "No", covid_hospitalized),
    cough = ifelse(is.na(cough), "None", cough),
    smell_loss = ifelse(is.na(smell_loss), "None", smell_loss),
    taste_loss = ifelse(is.na(taste_loss), "None", taste_loss),
    # Create binary indicators for analysis
    fever_binary = ifelse(fever == "Yes" | temperature > 38, 1, 0),
    resp_binary = ifelse(respiratory_symptoms %in% c("Moderate", "Severe"), 1, 0),
    hosp_binary = ifelse(covid_hospitalized == "Yes", 1, 0),
    cough_binary = ifelse(cough %in% c("Moderate", "Severe"), 1, 0),
    smell_binary = ifelse(smell_loss %in% c("Moderate", "Severe"), 1, 0),
    taste_binary = ifelse(taste_loss %in% c("Moderate", "Severe"), 1, 0)
  )

report_progress(8, "ChAdOx data preparation complete")
report_progress(8, paste("Combined dataset dimensions:", nrow(chadox_combined), "rows,", ncol(chadox_combined), "columns"))

#----- 2. CLIMATE DATA RETRIEVAL -----#

# Determine date range from ChAdOx data
date_range <- range(chadox_combined$visit_date, na.rm = TRUE)
start_date <- date_range[1]
end_date <- date_range[2]

report_progress(9, paste("ChAdOx data date range:", start_date, "to", end_date))

# Fetch climate data using Python and Earth Engine
report_progress(10, "Starting climate data retrieval using Python and Earth Engine (this may take several minutes)")

# Check if climate data already exists
climate_data_file <- "climate_data_ee.csv"
if (file.exists(climate_data_file)) {
  # Check if the file is recent (less than 1 day old)
  file_info <- file.info(climate_data_file)
  file_age <- difftime(Sys.time(), file_info$mtime, units = "hours")
  
  if (file_age < 24) {
    report_progress(10, "Using existing climate data file (less than 24 hours old)")
    use_existing <- TRUE
  } else {
    report_progress(10, "Existing climate data file is older than 24 hours, fetching new data")
    use_existing <- FALSE
  }
} else {
  use_existing <- FALSE
}

if (!use_existing) {
  # Use Python script to fetch climate data
  python_cmd <- paste0(
    "python fetch_climate_data.py ",
    format(start_date, "%Y-%m-%d"), " ",
    format(end_date, "%Y-%m-%d"), " ",
    climate_data_file
  )
  
  report_progress(10, paste("Running Python command:", python_cmd))
  system(python_cmd)
}

# Load the climate data from CSV
report_progress(11, "Loading climate data from CSV")
climate_data <- read.csv(climate_data_file, stringsAsFactors = FALSE)

# Convert date column to Date type
climate_data$date <- as.Date(climate_data$date)

# Display data source information
report_progress(11, paste("Climate data source:", unique(climate_data$data_source)))

# Check data completeness
missing_temp <- sum(is.na(climate_data$temp_mean))
missing_pm25 <- sum(is.na(climate_data$pm25))

report_progress(11, "Data completeness:")
report_progress(11, paste("  Temperature data:", round((1 - missing_temp / nrow(climate_data)) * 100, 1), "% complete"))
report_progress(11, paste("  PM2.5 data:", round((1 - missing_pm25 / nrow(climate_data)) * 100, 1), "% complete"))

#----- 3. MERGE HEALTH AND CLIMATE DATA -----#

# Convert ChAdOx data to daily format
report_progress(12, "Converting ChAdOx data to daily format")
chadox_daily <- chadox_combined %>%
  group_by(visit_date) %>%
  summarize(
    avg_temperature = mean(temperature, na.rm = TRUE),
    avg_oxygen = mean(oxygen_saturation, na.rm = TRUE),
    avg_respiratory = mean(respiratory_rate, na.rm = TRUE),
    fever_cases = sum(fever_binary, na.rm = TRUE),
    resp_cases = sum(resp_binary, na.rm = TRUE),
    hosp_cases = sum(hosp_binary, na.rm = TRUE),
    cough_cases = sum(cough_binary, na.rm = TRUE),
    smell_loss_cases = sum(smell_binary, na.rm = TRUE),
    taste_loss_cases = sum(taste_binary, na.rm = TRUE),
    total_visits = n()
  )

# Merge with climate data
report_progress(13, "Merging health and climate data")
analysis_data <- chadox_daily %>%
  left_join(climate_data, by = c("visit_date" = "date"))

# Add month and season variables
analysis_data <- analysis_data %>%
  mutate(
    month = month(visit_date),
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Autumn",
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    )
  )

# Save the merged dataset for future use
save(analysis_data, file = "heat_health_merged_data.RData")
report_progress(13, "Merged dataset saved to 'heat_health_merged_data.RData'")

#----- 4. ANALYSIS -----#

# 1. Correlation analysis
report_progress(14, "Performing correlation analysis")
health_vars <- c("avg_temperature", "avg_oxygen", "avg_respiratory", 
                "fever_cases", "resp_cases", "hosp_cases", 
                "cough_cases", "smell_loss_cases", "taste_loss_cases")

climate_vars <- c("temp_mean", "temp_max", "temp_min", "heat_index", "pm25")

# Create correlation matrix
correlation_data <- analysis_data %>%
  select(all_of(c(health_vars, climate_vars)))

# Ensure we handle NA values properly
correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

# Print correlation between climate and health variables
report_progress(14, "Correlation between climate variables and health outcomes:")
health_climate_corr <- correlation_matrix[health_vars, climate_vars]
print(health_climate_corr)

# 2. Seasonal analysis
report_progress(15, "Analyzing seasonal patterns")
seasonal_summary <- analysis_data %>%
  group_by(season) %>%
  summarize(
    avg_temp = mean(temp_mean, na.rm = TRUE),
    avg_max_temp = mean(temp_max, na.rm = TRUE),
    avg_heat_index = mean(heat_index, na.rm = TRUE),
    avg_pm25 = mean(pm25, na.rm = TRUE),
    fever_rate = sum(fever_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    respiratory_rate = sum(resp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    hospitalization_rate = sum(hosp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    total_cases = sum(total_visits, na.rm = TRUE)
  )

report_progress(15, "Seasonal patterns in climate variables and health outcomes:")
print(seasonal_summary)

# 3. Heat threshold analysis
report_progress(16, "Analyzing heat thresholds and health impacts")
heat_threshold_analysis <- analysis_data %>%
  filter(!is.na(temp_max)) %>%  # Filter out NA values
  mutate(
    heat_category = case_when(
      temp_max < 25 ~ "Cool (<25°C)",
      temp_max >= 25 & temp_max < 30 ~ "Moderate (25-30°C)",
      temp_max >= 30 & temp_max < 35 ~ "Hot (30-35°C)",
      temp_max >= 35 ~ "Extreme (≥35°C)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(heat_category) %>%
  summarize(
    avg_fever_rate = sum(fever_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    avg_resp_rate = sum(resp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    avg_hosp_rate = sum(hosp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    days_count = n(),
    total_visits = sum(total_visits, na.rm = TRUE)
  )

report_progress(16, "Health impacts by temperature threshold:")
print(heat_threshold_analysis)

# 4. Air pollution threshold analysis
report_progress(17, "Analyzing air pollution thresholds and health impacts")
pm25_threshold_analysis <- analysis_data %>%
  filter(!is.na(pm25)) %>%  # Filter out NA values
  mutate(
    pm25_category = case_when(
      pm25 < 10 ~ "Good (<10 μg/m³)",
      pm25 >= 10 & pm25 < 25 ~ "Moderate (10-25 μg/m³)",
      pm25 >= 25 & pm25 < 50 ~ "Unhealthy (25-50 μg/m³)",
      pm25 >= 50 ~ "Very Unhealthy (≥50 μg/m³)",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(pm25_category) %>%
  summarize(
    avg_fever_rate = sum(fever_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    avg_resp_rate = sum(resp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    avg_hosp_rate = sum(hosp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    days_count = n(),
    total_visits = sum(total_visits, na.rm = TRUE)
  )

report_progress(17, "Health impacts by PM2.5 threshold:")
print(pm25_threshold_analysis)

# 5. Lag effect analysis
report_progress(18, "Analyzing lag effects between climate exposure and health outcomes")
# Create lagged variables
analysis_data_lag <- analysis_data
for (lag_days in 1:7) {
  lag_suffix <- paste0("_lag", lag_days)
  analysis_data_lag[[paste0("temp_max", lag_suffix)]] <- lag(analysis_data_lag$temp_max, lag_days)
  analysis_data_lag[[paste0("pm25", lag_suffix)]] <- lag(analysis_data_lag$pm25, lag_days)
}

# Correlation with lagged variables
lag_vars <- c(paste0("temp_max_lag", 1:7), paste0("pm25_lag", 1:7))
health_outcome_vars <- c("fever_cases", "resp_cases", "hosp_cases")

# Handle NA values properly
lag_correlation <- cor(
  analysis_data_lag[, c(health_outcome_vars, lag_vars)], 
  use = "pairwise.complete.obs"
)

report_progress(18, "Lag effect correlation (temperature):")
print(lag_correlation[health_outcome_vars, paste0("temp_max_lag", 1:7)])

report_progress(18, "Lag effect correlation (PM2.5):")
print(lag_correlation[health_outcome_vars, paste0("pm25_lag", 1:7)])

#----- 5. VISUALIZATIONS -----#

# 1. Time series of temperature and health outcomes
report_progress(19, "Creating time series visualization")
# Filter out NA values for visualization
time_series_data <- analysis_data %>% 
  filter(!is.na(temp_max) & !is.na(heat_index))

# Check if we have data to plot
if(nrow(time_series_data) > 0) {
  p1 <- ggplot(time_series_data, aes(x = visit_date)) +
    geom_line(aes(y = temp_max, color = "Max Temperature")) +
    geom_line(aes(y = heat_index, color = "Heat Index")) +
    geom_bar(aes(y = fever_cases, fill = "Fever Cases"), stat = "identity", alpha = 0.5) +
    scale_y_continuous(name = "Temperature (°C)", 
                      sec.axis = sec_axis(~., name = "Number of Cases")) +
    labs(title = paste0("Temperature and Fever Cases Over Time (", unique(climate_data$data_source), ")"),
         x = "Date",
         color = "Climate Variable",
         fill = "Health Outcome") +
    theme_minimal()
  
  ggsave("temperature_fever_timeseries.png", plot = p1, width = 10, height = 6)
  report_progress(19, "Time series visualization saved")
} else {
  report_progress(19, "WARNING: Not enough data for time series visualization")
}

# 2. Seasonal comparison
report_progress(20, "Creating seasonal comparison visualization")
# Make sure we have valid seasonal data
seasonal_long <- seasonal_summary %>%
  filter(!is.na(avg_temp) & !is.na(avg_max_temp) & !is.na(avg_heat_index) & !is.na(avg_pm25)) %>%
  pivot_longer(cols = c(avg_temp, avg_max_temp, avg_heat_index, avg_pm25),
              names_to = "climate_variable", values_to = "value")

if(nrow(seasonal_long) > 0) {
  p2 <- ggplot(seasonal_long, aes(x = season, y = value, fill = climate_variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~climate_variable, scales = "free_y") +
    labs(title = paste0("Seasonal Patterns in Climate Variables (", unique(climate_data$data_source), ")"),
         x = "Season",
         y = "Value",
         fill = "Climate Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("seasonal_climate_patterns.png", plot = p2, width = 10, height = 8)
  report_progress(20, "Seasonal comparison visualization saved")
} else {
  report_progress(20, "WARNING: Not enough data for seasonal comparison visualization")
}

# 3. Heat threshold impact visualization
report_progress(21, "Creating heat threshold visualization")
# Check if we have valid threshold data
if(nrow(heat_threshold_analysis) > 0 && !all(heat_threshold_analysis$heat_category == "Unknown")) {
  heat_threshold_long <- heat_threshold_analysis %>%
    pivot_longer(cols = c(avg_fever_rate, avg_resp_rate, avg_hosp_rate),
                names_to = "health_outcome", values_to = "rate")
  
  p3 <- ggplot(heat_threshold_long, aes(x = heat_category, y = rate, fill = health_outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Health Impacts by Temperature Threshold (", unique(climate_data$data_source), ")"),
         x = "Temperature Category",
         y = "Rate (%)",
         fill = "Health Outcome") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("heat_threshold_impacts.png", plot = p3, width = 10, height = 6)
  report_progress(21, "Heat threshold visualization saved")
} else {
  report_progress(21, "WARNING: Not enough data for heat threshold visualization")
}

# 4. PM2.5 threshold impact visualization
report_progress(22, "Creating PM2.5 threshold visualization")
# Check if we have valid PM2.5 threshold data
if(nrow(pm25_threshold_analysis) > 0 && !all(pm25_threshold_analysis$pm25_category == "Unknown")) {
  pm25_threshold_long <- pm25_threshold_analysis %>%
    pivot_longer(cols = c(avg_fever_rate, avg_resp_rate, avg_hosp_rate),
                names_to = "health_outcome", values_to = "rate")
  
  p4 <- ggplot(pm25_threshold_long, aes(x = pm25_category, y = rate, fill = health_outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Health Impacts by PM2.5 Threshold (", unique(climate_data$data_source), ")"),
         x = "PM2.5 Category",
         y = "Rate (%)",
         fill = "Health Outcome") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("pm25_threshold_impacts.png", plot = p4, width = 10, height = 6)
  report_progress(22, "PM2.5 threshold visualization saved")
} else {
  report_progress(22, "WARNING: Not enough data for PM2.5 threshold visualization")
}

# 5. Lag effect visualization
report_progress(23, "Creating lag effect visualization")
# Check if we have valid lag correlation data
valid_lag_data <- !is.na(lag_correlation["fever_cases", "temp_max_lag1"])

if(valid_lag_data) {
  # Prepare lag effect data
  lag_effect_temp <- data.frame(
    lag_days = 0:7,
    fever_correlation = c(cor(analysis_data$fever_cases, analysis_data$temp_max, use = "pairwise.complete.obs"),
                         lag_correlation["fever_cases", paste0("temp_max_lag", 1:7)]),
    resp_correlation = c(cor(analysis_data$resp_cases, analysis_data$temp_max, use = "pairwise.complete.obs"),
                        lag_correlation["resp_cases", paste0("temp_max_lag", 1:7)]),
    hosp_correlation = c(cor(analysis_data$hosp_cases, analysis_data$temp_max, use = "pairwise.complete.obs"),
                        lag_correlation["hosp_cases", paste0("temp_max_lag", 1:7)])
  )
  
  lag_effect_temp_long <- lag_effect_temp %>%
    pivot_longer(cols = c(fever_correlation, resp_correlation, hosp_correlation),
                names_to = "health_outcome", values_to = "correlation")
  
  p5 <- ggplot(lag_effect_temp_long, aes(x = lag_days, y = correlation, color = health_outcome, group = health_outcome)) +
    geom_line() +
    geom_point() +
    labs(title = paste0("Lag Effect of Temperature on Health Outcomes (", unique(climate_data$data_source), ")"),
         x = "Lag (days)",
         y = "Correlation",
         color = "Health Outcome") +
    theme_minimal()
  
  ggsave("temperature_lag_effects.png", plot = p5, width = 10, height = 6)
  report_progress(23, "Lag effect visualization saved")
} else {
  report_progress(23, "WARNING: Not enough data for lag effect visualization")
}

# Save results
report_progress(24, "Saving analysis results")
save(correlation_matrix, seasonal_summary, heat_threshold_analysis, pm25_threshold_analysis,
    lag_correlation, analysis_data, climate_data, file = "heat_health_analysis_results.RData")

report_progress(25, "Analysis complete. Results saved to 'heat_health_analysis_results.RData'")
report_progress(25, "Visualizations saved as PNG files")
report_progress(25, paste("IMPORTANT: Climate data source is", unique(climate_data$data_source)))
