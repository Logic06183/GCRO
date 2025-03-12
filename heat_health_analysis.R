# Heat, Air Pollution, and COVID-19 Health Outcomes Analysis
# Integrating ChAdOx, GCRO, and Earth Engine Climate Data
# Focus: Heat Center Project - Environmental Justice Implications

# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(stringr)
library(lubridate)
library(corrplot)
library(reticulate)  # For Python integration with Earth Engine

# Configure reticulate to use the correct Python
tryCatch({
  use_condaenv("base", required = TRUE)
  cat("Using conda 'base' environment for Python\n")
}, error = function(e) {
  tryCatch({
    py_config <- py_discover_config()
    use_python(py_config$python, required = TRUE)
    cat("Using system Python:", py_config$python, "\n")
  }, error = function(e) {
    cat("Could not configure Python environment:", e$message, "\n")
  })
})

#----- 1. DATA LOADING -----#

# Load GCRO datasets
cat("Loading GCRO datasets...\n")
tryCatch({
  # Load 2020-2021 GCRO dataset
  qol_2020 <- read.csv("qols-2020-2021-new-weights-v1.csv", stringsAsFactors = FALSE)
  cat("Successfully loaded 2020-2021 GCRO dataset\n")
  
  # Load 2017-2018 GCRO dataset
  qol_2017 <- read.csv("qols-v-2017-2018-v1.1.csv", stringsAsFactors = FALSE)
  cat("Successfully loaded 2017-2018 GCRO dataset\n")
}, error = function(e) {
  cat("Error loading GCRO datasets:", e$message, "\n")
  stop("Cannot proceed without GCRO datasets")
})

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
tryCatch({
  load("ChAdOx data 2024-07-19.Rdata")
  load("ChAdOx supp visit data 2024-07-19.Rdata")
  cat("Successfully loaded ChAdOx datasets\n")
  
  # List all loaded datasets
  cat("Available ChAdOx datasets:\n")
  chadox_datasets <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]
  print(chadox_datasets)
  
  # Extract key datasets
  cat("\nExtracting key ChAdOx datasets for analysis...\n")
  
  # 1. COVID cases data
  if (exists("covid_cases")) {
    cat("Processing COVID cases data...\n")
    # Check structure and column names
    cat("COVID cases columns:", paste(names(covid_cases)[1:min(5, ncol(covid_cases))], collapse=", "), "...\n")
    cat("COVID cases dimensions:", nrow(covid_cases), "rows,", ncol(covid_cases), "columns\n")
    
    # Extract key variables
    covid_data <- covid_cases %>%
      select(record_id,
             covid_test_date = covid_test_date_v2_3,
             covid_result = covid_result_v2_3,
             covid_hospitalized = covid_hosp_v2_3,
             covid_severity = covid_severity_v2_3) %>%
      mutate(covid_test_date = as.Date(covid_test_date))
  } else {
    cat("COVID cases dataset not found\n")
  }
  
  # 2. Illness visit data
  if (exists("illness_visit")) {
    cat("Processing illness visit data...\n")
    # Check structure and column names
    cat("Illness visit columns:", paste(names(illness_visit)[1:min(5, ncol(illness_visit))], collapse=", "), "...\n")
    cat("Illness visit dimensions:", nrow(illness_visit), "rows,", ncol(illness_visit), "columns\n")
    
    # Extract key variables
    illness_data <- illness_visit %>%
      select(record_id, 
             visit_date = ill_visit_date_v2_3,
             temperature = ill_temp_v2_3,
             oxygen_saturation = ill_oxy_v2_3,
             respiratory_rate = ill_resp_v2_3,
             pulse = ill_pulse_v2_3) %>%
      mutate(visit_date = as.Date(visit_date))
  } else {
    cat("Illness visit dataset not found\n")
  }
  
  # 3. Symptom data
  if (exists("diary_card_illness_v2_3")) {
    cat("Processing symptom data...\n")
    # Check structure and column names
    cat("Symptom data columns:", paste(names(diary_card_illness_v2_3)[1:min(5, ncol(diary_card_illness_v2_3))], collapse=", "), "...\n")
    cat("Symptom data dimensions:", nrow(diary_card_illness_v2_3), "rows,", ncol(diary_card_illness_v2_3), "columns\n")
    
    # Extract key variables
    symptom_data <- diary_card_illness_v2_3 %>%
      select(record_id,
             symptom_date = diary_ill_date_v2_3,
             fever = diary_ill_fever_v2_3,
             cough = diary_ill_cough_v2_3,
             shortness_of_breath = diary_ill_sob_v2_3,
             loss_of_smell = diary_ill_smell_v2_3,
             loss_of_taste = diary_ill_taste_v2_3,
             fatigue = diary_ill_fatigue_v2_3) %>%
      mutate(symptom_date = as.Date(symptom_date))
  } else {
    cat("Symptom data not found\n")
  }
  
  # 4. Demographic data
  if (exists("demographic_and_contact_information_v2_3")) {
    cat("Processing demographic data...\n")
    # Check structure and column names
    cat("Demographic data columns:", paste(names(demographic_and_contact_information_v2_3)[1:min(5, ncol(demographic_and_contact_information_v2_3))], collapse=", "), "...\n")
    cat("Demographic data dimensions:", nrow(demographic_and_contact_information_v2_3), "rows,", ncol(demographic_and_contact_information_v2_3), "columns\n")
    
    # Extract key variables
    demo_data <- demographic_and_contact_information_v2_3 %>%
      select(record_id,
             age = demo_age_v2_3,
             sex = demo_sex_v2_3,
             race = demo_race_v2_3,
             location = demo_res_area_v2_3)
  } else {
    cat("Demographic data not found\n")
  }
  
  # Combine datasets
  cat("\nCombining ChAdOx datasets...\n")
  if (exists("illness_data") && exists("covid_data")) {
    chadox_combined <- illness_data %>%
      left_join(covid_data, by = "record_id")
    
    if (exists("symptom_data")) {
      chadox_combined <- chadox_combined %>%
        left_join(symptom_data, by = "record_id")
    }
    
    if (exists("demo_data")) {
      chadox_combined <- chadox_combined %>%
        left_join(demo_data, by = "record_id")
    }
    
    cat("Successfully created combined ChAdOx dataset\n")
    cat("Combined dataset dimensions:", nrow(chadox_combined), "rows,", ncol(chadox_combined), "columns\n")
  } else {
    cat("Could not create combined dataset - missing required components\n")
    stop("Cannot proceed without ChAdOx data")
  }
}, error = function(e) {
  cat("Error processing ChAdOx data:", e$message, "\n")
  stop("Cannot proceed without ChAdOx data")
})

#----- 2. EARTH ENGINE CLIMATE DATA -----#

# Function to fetch climate data from Earth Engine
fetch_climate_data_ee <- function(start_date, end_date, location = "Soweto", auth_code = NULL) {
  cat("Fetching climate data from Earth Engine for", location, "from", start_date, "to", end_date, "\n")
  
  tryCatch({
    # Initialize Earth Engine
    ee <- reticulate::import("ee")
    
    # Authenticate if auth_code is provided
    if (!is.null(auth_code)) {
      cat("Using provided authentication code\n")
      ee$Initialize()
    } else {
      cat("Using default authentication\n")
      ee$Initialize()
    }
    
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
  }, error = function(e) {
    cat("Error fetching climate data from Earth Engine:", e$message, "\n")
    stop("Cannot proceed without climate data")
  })
}

# Determine date range from ChAdOx data
if (exists("chadox_combined")) {
  date_cols <- names(chadox_combined)[sapply(chadox_combined, is.Date)]
  
  if (length(date_cols) > 0) {
    all_dates <- unlist(lapply(date_cols, function(col) chadox_combined[[col]]))
    all_dates <- all_dates[!is.na(all_dates)]
    
    if (length(all_dates) > 0) {
      start_date <- min(all_dates, na.rm = TRUE)
      end_date <- max(all_dates, na.rm = TRUE)
      
      cat("ChAdOx data date range:", start_date, "to", end_date, "\n")
      
      # Earth Engine authentication code (from previous session)
      auth_code <- "4/1AfJohXlbUXrCJKDhzHm6Oe9BVEgFMT2Ld1ixK-_uCTfLSEDQMVw9aqQQELQ"
      
      # Fetch climate data for the same period
      climate_data <- fetch_climate_data_ee(start_date, end_date, "Soweto", auth_code)
    } else {
      cat("No valid dates found in ChAdOx data\n")
      stop("Cannot proceed without date range")
    }
  } else {
    cat("No date columns found in ChAdOx data\n")
    stop("Cannot proceed without date range")
  }
} else {
  cat("ChAdOx combined dataset not available\n")
  stop("Cannot proceed without ChAdOx data")
}

#----- 3. DATA ANALYSIS -----#

# 1. Prepare data for analysis
cat("\nPreparing data for analysis...\n")

# Convert ChAdOx data to daily format for merging with climate data
chadox_daily <- chadox_combined %>%
  group_by(visit_date) %>%
  summarize(
    avg_temperature = mean(as.numeric(temperature), na.rm = TRUE),
    avg_oxygen = mean(as.numeric(oxygen_saturation), na.rm = TRUE),
    avg_respiratory = mean(as.numeric(respiratory_rate), na.rm = TRUE),
    fever_cases = sum(fever == "Yes" | as.numeric(temperature) > 38, na.rm = TRUE),
    cough_cases = sum(cough == "Yes", na.rm = TRUE),
    sob_cases = sum(shortness_of_breath == "Yes", na.rm = TRUE),
    smell_loss_cases = sum(loss_of_smell == "Yes", na.rm = TRUE),
    taste_loss_cases = sum(loss_of_taste == "Yes", na.rm = TRUE),
    fatigue_cases = sum(fatigue == "Yes" | fatigue == "Moderate" | fatigue == "Severe", na.rm = TRUE),
    total_visits = n()
  )

# Merge with climate data
analysis_data <- chadox_daily %>%
  left_join(climate_data, by = c("visit_date" = "date"))

# 2. Analyze heat and health relationships
cat("\nAnalyzing heat and health relationships...\n")

# Correlation analysis
health_vars <- c("avg_temperature", "avg_oxygen", "avg_respiratory", 
                "fever_cases", "cough_cases", "sob_cases", 
                "smell_loss_cases", "taste_loss_cases", "fatigue_cases")

climate_vars <- c("temp_mean", "temp_max", "temp_min", "heat_index", "pm25", "no2")

# Create correlation matrix
correlation_data <- analysis_data %>%
  select(all_of(c(health_vars, climate_vars)))

correlation_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

# 3. Analyze seasonal patterns
cat("\nAnalyzing seasonal patterns...\n")

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

# Seasonal analysis
seasonal_summary <- analysis_data %>%
  group_by(season) %>%
  summarize(
    avg_temp = mean(temp_mean, na.rm = TRUE),
    avg_max_temp = mean(temp_max, na.rm = TRUE),
    avg_heat_index = mean(heat_index, na.rm = TRUE),
    avg_pm25 = mean(pm25, na.rm = TRUE),
    avg_no2 = mean(no2, na.rm = TRUE),
    fever_rate = sum(fever_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    respiratory_rate = sum(sob_cases, na.rm = TRUE) / sum(total_visits, na.rm = TRUE) * 100,
    total_cases = sum(total_visits, na.rm = TRUE)
  )

# 4. Analyze environmental justice implications using GCRO data
cat("\nAnalyzing environmental justice implications...\n")

# Extract Soweto data from GCRO
soweto_2020 <- qol_2020 %>%
  filter(grepl("Soweto", Planning_region, ignore.case = TRUE))

# Identify SES indicators in GCRO
ses_cols <- grep("income|education|employment|poverty|wealth", 
                names(soweto_2020), ignore.case = TRUE, value = TRUE)

# Create SES groups
if (length(ses_cols) > 0) {
  # Use income as primary SES indicator if available
  income_col <- ses_cols[grep("income", ses_cols, ignore.case = TRUE)][1]
  
  if (!is.na(income_col)) {
    soweto_2020$ses_group <- cut(
      as.numeric(soweto_2020[[income_col]]),
      breaks = quantile(as.numeric(soweto_2020[[income_col]]), probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
      labels = c("Low SES", "Medium SES", "High SES"),
      include.lowest = TRUE
    )
  } else {
    # Create composite SES score if income not available
    soweto_2020$ses_score <- rowMeans(
      sapply(ses_cols, function(col) {
        as.numeric(soweto_2020[[col]])
      }),
      na.rm = TRUE
    )
    
    soweto_2020$ses_group <- cut(
      soweto_2020$ses_score,
      breaks = quantile(soweto_2020$ses_score, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE),
      labels = c("Low SES", "Medium SES", "High SES"),
      include.lowest = TRUE
    )
  }
  
  # Identify environmental indicators
  env_cols <- grep("environment|pollution|water|waste|green|housing|sanitation", 
                  names(soweto_2020), ignore.case = TRUE, value = TRUE)
  
  if (length(env_cols) > 0) {
    # Analyze environmental factors by SES
    env_justice <- soweto_2020 %>%
      group_by(ses_group) %>%
      summarize(across(all_of(env_cols), 
                      ~ mean(as.numeric(.), na.rm = TRUE),
                      .names = "{.col}_mean"))
  }
}

#----- 4. RESULTS AND VISUALIZATION -----#

# 1. Print correlation results
cat("\nCorrelation between climate variables and health outcomes:\n")
health_climate_corr <- correlation_matrix[health_vars, climate_vars]
print(health_climate_corr)

# 2. Print seasonal analysis
cat("\nSeasonal patterns in climate variables and health outcomes:\n")
print(seasonal_summary)

# 3. Print environmental justice results
if (exists("env_justice")) {
  cat("\nEnvironmental justice analysis by socioeconomic status:\n")
  print(env_justice)
}

# 4. Create visualizations

# Time series plot of temperature and health outcomes
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

# Seasonal comparison
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

# Environmental justice visualization
if (exists("env_justice")) {
  cat("\nCreating environmental justice visualization...\n")
  env_justice_long <- env_justice %>%
    pivot_longer(cols = -ses_group, 
                names_to = "environmental_factor", 
                values_to = "score")
  
  ggplot(env_justice_long, aes(x = environmental_factor, y = score, fill = ses_group)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Environmental Factors by Socioeconomic Status",
         x = "Environmental Factor",
         y = "Score",
         fill = "SES Group") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("environmental_justice.png", width = 12, height = 6)
}

# Save results
cat("\nSaving analysis results...\n")
save(correlation_matrix, seasonal_summary, env_justice, analysis_data,
    file = "heat_health_analysis_results.RData")

cat("\nAnalysis complete. Results saved to 'heat_health_analysis_results.RData'\n")
cat("Visualizations saved as PNG files\n")
