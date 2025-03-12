# Heat, Air Pollution, and COVID-19 Health Outcomes Analysis
# Using real ChAdOx, GCRO, and Earth Engine data
# Focus: Heat Center Project - Environmental Justice Implications

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)
library(reticulate)  # For Python integration with Earth Engine

# Configure reticulate to use conda environment
use_condaenv("base", required = TRUE)

#----- 1. LOAD AND PREPARE DATA -----#

# Load GCRO datasets
cat("Loading GCRO datasets...\n")
qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)
cat("Successfully loaded GCRO datasets\n")

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
load("ChAdOx data 2024-07-19.Rdata")
load("ChAdOx supp visit data 2024-07-19.Rdata")
cat("Successfully loaded ChAdOx datasets\n")

# Extract and prepare COVID cases data
cat("Processing COVID cases data...\n")
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
cat("Processing illness visit data...\n")
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
cat("Processing symptom data...\n")
if (exists("diary_card_illness_v2_3")) {
  # Function to interpret symptom codes
  interpret_symptom <- function(code) {
    if (is.na(code)) return(NA)
    code <- as.character(code)
    if (code == "0") return("None")
    if (code == "1") return("Mild")
    if (code == "2") return("Moderate")
    if (code == "3") return("Severe")
    return("Unknown")
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
cat("Processing demographic data...\n")
if (exists("demographic_and_contact_information_v2_3")) {
  demo_data <- demographic_and_contact_information_v2_3 %>%
    mutate(
      record_id = as.character(record_id),
      age = as.numeric(age_calc),
      sex = case_when(
        sc_sex_v2_3 == "1" ~ "Male",
        sc_sex_v2_3 == "2" ~ "Female",
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
cat("Combining ChAdOx datasets...\n")
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

cat("ChAdOx data preparation complete\n")
cat("Combined dataset dimensions:", nrow(chadox_combined), "rows,", ncol(chadox_combined), "columns\n")

#----- 2. EARTH ENGINE CLIMATE DATA -----#

# Function to fetch climate data from Earth Engine
fetch_climate_data_ee <- function(start_date, end_date, location = "Soweto") {
  cat("Fetching climate data from Earth Engine for", location, "from", start_date, "to", end_date, "\n")
  
  # Initialize Earth Engine
  ee <- reticulate::import("ee")
  
  # Authenticate - use the service account method
  ee$Initialize()
  cat("Earth Engine initialized successfully\n")
  
  # Define location (Soweto coordinates)
  soweto_point <- ee$Geometry$Point(c(27.8579, -26.2485))  # longitude, latitude
  
  # Convert dates to EE format
  start_date_ee <- ee$Date(as.character(as.Date(start_date)))
  end_date_ee <- ee$Date(as.character(as.Date(end_date)))
  
  # Get ERA5 temperature data
  cat("Retrieving ERA5 temperature data...\n")
  era5_dataset <- ee$ImageCollection('ECMWF/ERA5/DAILY')$
    filterDate(start_date_ee, end_date_ee)
  
  # Get temperature data
  temp_data <- era5_dataset$select(c('mean_2m_air_temperature', 'minimum_2m_air_temperature', 'maximum_2m_air_temperature'))
  
  # Get CAMS air quality data
  cat("Retrieving CAMS air quality data...\n")
  cams_dataset <- ee$ImageCollection('ECMWF/CAMS/NRT')$
    filterDate(start_date_ee, end_date_ee)
  
  # Get PM2.5 and NO2 data
  air_quality_data <- cams_dataset$select(c('particulate_matter_d_less_than_2_5_um_surface', 'nitrogen_dioxide_surface'))
  
  # Function to extract data at a point
  extract_data <- function(image) {
    data <- image$reduceRegion(
      reducer = ee$Reducer$mean(),
      geometry = soweto_point,
      scale = 10000  # Scale in meters
    )
    return(ee$Feature(NULL, data))
  }
  
  # Apply extraction function to collections
  temp_features <- temp_data$map(extract_data)
  air_quality_features <- air_quality_data$map(extract_data)
  
  # Convert to lists
  temp_list <- temp_features$toList(temp_data$size())
  air_quality_list <- air_quality_features$toList(air_quality_data$size())
  
  # Create a sequence of dates
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Create an empty dataframe
  climate_data <- data.frame(
    date = dates,
    temp_mean = NA,
    temp_min = NA,
    temp_max = NA,
    pm25 = NA,
    no2 = NA
  )
  
  # Process temperature data
  cat("Processing temperature data...\n")
  temp_size <- as.integer(temp_data$size()$getInfo())
  for (i in 1:min(temp_size, length(dates))) {
    feature <- ee$Feature(temp_list$get(i - 1))
    props <- feature$getInfo()$properties
    
    if (!is.null(props)) {
      # Find the matching date index
      date_str <- names(props)[grep("system:time_start", names(props))]
      if (length(date_str) > 0) {
        timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
        date_index <- which(abs(as.numeric(dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
        
        if (length(date_index) > 0) {
          climate_data$temp_mean[date_index] <- props[["mean_2m_air_temperature"]] - 273.15  # Convert from K to C
          climate_data$temp_min[date_index] <- props[["minimum_2m_air_temperature"]] - 273.15
          climate_data$temp_max[date_index] <- props[["maximum_2m_air_temperature"]] - 273.15
        }
      }
    }
  }
  
  # Process air quality data
  cat("Processing air quality data...\n")
  aq_size <- as.integer(air_quality_data$size()$getInfo())
  for (i in 1:min(aq_size, length(dates))) {
    feature <- ee$Feature(air_quality_list$get(i - 1))
    props <- feature$getInfo()$properties
    
    if (!is.null(props)) {
      # Find the matching date index
      date_str <- names(props)[grep("system:time_start", names(props))]
      if (length(date_str) > 0) {
        timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
        date_index <- which(abs(as.numeric(dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
        
        if (length(date_index) > 0) {
          climate_data$pm25[date_index] <- props[["particulate_matter_d_less_than_2_5_um_surface"]]
          climate_data$no2[date_index] <- props[["nitrogen_dioxide_surface"]]
        }
      }
    }
  }
  
  # Calculate heat index
  cat("Calculating heat index...\n")
  climate_data$heat_index <- with(climate_data, {
    # Simplified heat index calculation
    # HI = 0.5 * (T + 61.0 + ((T-68.0)*1.2) + (RH*0.094))
    # Using a simplified version since we don't have humidity
    temp_mean + (temp_max - temp_mean) * 0.2
  })
  
  cat("Climate data retrieval complete\n")
  return(climate_data)
}

# Determine date range from ChAdOx data
date_range <- range(chadox_combined$visit_date, na.rm = TRUE)
start_date <- date_range[1]
end_date <- date_range[2]

cat("ChAdOx data date range:", start_date, "to", end_date, "\n")

# Fetch climate data for the same period
climate_data <- fetch_climate_data_ee(start_date, end_date, "Soweto")

#----- 3. MERGE HEALTH AND CLIMATE DATA -----#

# Convert ChAdOx data to daily format
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

#----- 4. ANALYSIS -----#

# 1. Correlation analysis
cat("\nPerforming correlation analysis...\n")
health_vars <- c("avg_temperature", "avg_oxygen", "avg_respiratory", 
                "fever_cases", "resp_cases", "hosp_cases", 
                "cough_cases", "smell_loss_cases", "taste_loss_cases")

climate_vars <- c("temp_mean", "temp_max", "temp_min", "heat_index", "pm25", "no2")

# Create correlation matrix
correlation_data <- analysis_data %>%
  select(all_of(c(health_vars, climate_vars)))

correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

# Print correlation between climate and health variables
cat("\nCorrelation between climate variables and health outcomes:\n")
health_climate_corr <- correlation_matrix[health_vars, climate_vars]
print(health_climate_corr)

# 2. Seasonal analysis
cat("\nAnalyzing seasonal patterns...\n")
seasonal_summary <- analysis_data %>%
  group_by(season) %>%
  summarize(
    avg_temp = mean(temp_mean, na.rm = TRUE),
    avg_max_temp = mean(temp_max, na.rm = TRUE),
    avg_heat_index = mean(heat_index, na.rm = TRUE),
    avg_pm25 = mean(pm25, na.rm = TRUE),
    avg_no2 = mean(no2, na.rm = TRUE),
    fever_rate = sum(fever_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    respiratory_rate = sum(resp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    hospitalization_rate = sum(hosp_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    total_cases = sum(total_visits, na.rm = TRUE)
  )

cat("\nSeasonal patterns in climate variables and health outcomes:\n")
print(seasonal_summary)

# 3. Heat threshold analysis
cat("\nAnalyzing heat thresholds and health impacts...\n")
heat_threshold_analysis <- analysis_data %>%
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

cat("\nHealth impacts by temperature threshold:\n")
print(heat_threshold_analysis)

# 4. Air pollution threshold analysis
cat("\nAnalyzing air pollution thresholds and health impacts...\n")
pm25_threshold_analysis <- analysis_data %>%
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

cat("\nHealth impacts by PM2.5 threshold:\n")
print(pm25_threshold_analysis)

# 5. Lag effect analysis
cat("\nAnalyzing lag effects between climate exposure and health outcomes...\n")
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

lag_correlation <- cor(
  analysis_data_lag[, c(health_outcome_vars, lag_vars)], 
  use = "pairwise.complete.obs"
)

cat("\nLag effect correlation (temperature):\n")
print(lag_correlation[health_outcome_vars, paste0("temp_max_lag", 1:7)])

cat("\nLag effect correlation (PM2.5):\n")
print(lag_correlation[health_outcome_vars, paste0("pm25_lag", 1:7)])

#----- 5. VISUALIZATIONS -----#

# 1. Time series of temperature and health outcomes
cat("\nCreating time series visualization...\n")
ggplot(analysis_data, aes(x = visit_date)) +
  geom_line(aes(y = temp_max, color = "Max Temperature")) +
  geom_line(aes(y = heat_index, color = "Heat Index")) +
  geom_bar(aes(y = fever_cases, fill = "Fever Cases"), stat = "identity", alpha = 0.5) +
  scale_y_continuous(name = "Temperature (°C)", 
                    sec.axis = sec_axis(~., name = "Number of Cases")) +
  labs(title = "Temperature and Fever Cases Over Time",
       x = "Date",
       color = "Climate Variable",
       fill = "Health Outcome") +
  theme_minimal()

ggsave("temperature_fever_timeseries.png", width = 10, height = 6)

# 2. Seasonal comparison
cat("\nCreating seasonal comparison visualization...\n")
seasonal_long <- seasonal_summary %>%
  pivot_longer(cols = c(avg_temp, avg_max_temp, avg_heat_index, avg_pm25, avg_no2),
              names_to = "climate_variable", values_to = "value")

ggplot(seasonal_long, aes(x = season, y = value, fill = climate_variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~climate_variable, scales = "free_y") +
  labs(title = "Seasonal Patterns in Climate Variables",
       x = "Season",
       y = "Value",
       fill = "Climate Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("seasonal_climate_patterns.png", width = 10, height = 8)

# 3. Heat threshold impact visualization
cat("\nCreating heat threshold visualization...\n")
heat_threshold_long <- heat_threshold_analysis %>%
  pivot_longer(cols = c(avg_fever_rate, avg_resp_rate, avg_hosp_rate),
              names_to = "health_outcome", values_to = "rate")

ggplot(heat_threshold_long, aes(x = heat_category, y = rate, fill = health_outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Health Impacts by Temperature Threshold",
       x = "Temperature Category",
       y = "Rate (%)",
       fill = "Health Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("heat_threshold_impacts.png", width = 10, height = 6)

# 4. PM2.5 threshold impact visualization
cat("\nCreating PM2.5 threshold visualization...\n")
pm25_threshold_long <- pm25_threshold_analysis %>%
  pivot_longer(cols = c(avg_fever_rate, avg_resp_rate, avg_hosp_rate),
              names_to = "health_outcome", values_to = "rate")

ggplot(pm25_threshold_long, aes(x = pm25_category, y = rate, fill = health_outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Health Impacts by PM2.5 Threshold",
       x = "PM2.5 Category",
       y = "Rate (%)",
       fill = "Health Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("pm25_threshold_impacts.png", width = 10, height = 6)

# 5. Lag effect visualization
cat("\nCreating lag effect visualization...\n")
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

ggplot(lag_effect_temp_long, aes(x = lag_days, y = correlation, color = health_outcome, group = health_outcome)) +
  geom_line() +
  geom_point() +
  labs(title = "Lag Effect of Temperature on Health Outcomes",
       x = "Lag (days)",
       y = "Correlation",
       color = "Health Outcome") +
  theme_minimal()

ggsave("temperature_lag_effects.png", width = 10, height = 6)

# Save results
cat("\nSaving analysis results...\n")
save(correlation_matrix, seasonal_summary, heat_threshold_analysis, pm25_threshold_analysis,
    lag_correlation, analysis_data, file = "heat_health_real_data_results.RData")

cat("\nAnalysis complete. Results saved to 'heat_health_real_data_results.RData'\n")
cat("Visualizations saved as PNG files\n")
