# COVID-19 and Climate Data Analysis with Limited Data
# Analyzing real temperature and air quality data for Soweto
# For ISES-ISEE 2025 Abstract

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(corrplot)
library(dlnm)
library(splines)

# At the beginning of your script, add these lines:
library(dlnm)     # Add this line to load dlnm at the start
library(splines)  # Add this line to load splines at the start

# Function to load ERA5 climate data
load_era5_data <- function() {
  cat("Loading extended ERA5 climate data for Soweto...\n")
  tryCatch({
    df <- read.csv("soweto_era5_climate_data_extended.csv", stringsAsFactors = FALSE)
    if (nrow(df) > 0) {
      # Convert date to Date object
      df$date <- as.Date(df$date)
      
      # Convert temperature from Kelvin to Celsius if needed
      # Check if temperatures are in Kelvin (typically > 200) or already in Celsius
      if (mean(df$mean_2m_air_temperature, na.rm = TRUE) > 200) {
        cat("Converting temperatures from Kelvin to Celsius...\n")
        df$mean_temp_c <- df$mean_2m_air_temperature - 273.15
        df$min_temp_c <- df$minimum_2m_air_temperature - 273.15
        df$max_temp_c <- df$maximum_2m_air_temperature - 273.15
      } else {
        cat("Temperatures appear to be already in Celsius, renaming columns...\n")
        df$mean_temp_c <- df$mean_2m_air_temperature
        df$min_temp_c <- df$minimum_2m_air_temperature
        df$max_temp_c <- df$maximum_2m_air_temperature
      }
      
      # Convert or rename precipitation column
      if ("total_precipitation" %in% names(df)) {
        # Convert precipitation to mm if needed (multiply by 1000 if values are very small)
        if (mean(df$total_precipitation, na.rm = TRUE) < 0.1) {
          df$precipitation_mm <- df$total_precipitation * 1000
        } else {
          df$precipitation_mm <- df$total_precipitation
        }
      }
      
      cat("Successfully loaded extended ERA5 climate data with", nrow(df), "records\n")
      cat("ERA5 data date range:", format(min(df$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
          format(max(df$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
      return(df)
    } else {
      cat("ERA5 climate data file exists but contains no records\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error loading ERA5 climate data:", e$message, "\n")
    return(NULL)
  })
}

# Function to load CHIRPS precipitation data
load_chirps_data <- function() {
  cat("Loading CHIRPS precipitation data for Soweto...\n")
  tryCatch({
    df <- read.csv("soweto_chirps_precipitation_data.csv", stringsAsFactors = FALSE)
    if (nrow(df) > 0) {
      # Convert date to Date object
      df$date <- as.Date(df$date)
      cat("Successfully loaded CHIRPS precipitation data with", nrow(df), "records\n")
      return(df)
    } else {
      cat("CHIRPS precipitation data file exists but contains no records\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error loading CHIRPS precipitation data:", e$message, "\n")
    return(NULL)
  })
}

cat("Starting COVID-19 and Climate Data Analysis with Limited Data\n")

#----- 1. LOAD AND PREPARE CLIMATE DATA -----#

# Load ERA5 temperature data
era5_data <- load_era5_data()

# Load CHIRPS precipitation data
chirps_data <- load_chirps_data()

# Load original temperature data as fallback
if (is.null(era5_data)) {
  cat("Falling back to original temperature data...\n")
  temp_data <- tryCatch({
    # Try to load the full dataset first
    df <- read.csv("temperature_data/soweto_temperature_data_full.csv", stringsAsFactors = FALSE)
    if (nrow(df) > 0) {
      df$date <- as.Date(df$date)
      cat("Successfully loaded extended temperature data with", nrow(df), "records\n")
      df
    } else {
      # Fall back to original dataset
      df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
      df$date <- as.Date(df$date)
      cat("Successfully loaded original temperature data with", nrow(df), "records\n")
      df
    }
  }, error = function(e) {
    # If both fail, try just the original dataset
    tryCatch({
      df <- read.csv("temperature_data/soweto_temperature_data.csv", stringsAsFactors = FALSE)
      df$date <- as.Date(df$date)
      cat("Successfully loaded original temperature data with", nrow(df), "records\n")
      df
    }, error = function(e) {
      cat("Error loading temperature data:", e$message, "\n")
      NULL
    })
  })
} else {
  # Use ERA5 data as the primary temperature data
  temp_data <- era5_data %>%
    select(date, mean_temp_c, min_temp_c, max_temp_c) %>%
    mutate(precipitation_mm = era5_data$precipitation_mm)
}

# Load air quality data
cat("Loading simplified air quality data...\n")
air_quality_data <- tryCatch({
  df <- read.csv("soweto_air_quality_data_simplified.csv", stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
  cat("Successfully loaded simplified air quality data with", nrow(df), "records\n")
  df
}, error = function(e) {
  # Fall back to extended air quality data if simplified data is not available
  cat("Simplified air quality data not found, trying extended data...\n")
  tryCatch({
    df <- read.csv("soweto_air_quality_data_extended.csv", stringsAsFactors = FALSE)
    df$date <- as.Date(df$date)
    cat("Successfully loaded extended air quality data with", nrow(df), "records\n")
    df
  }, error = function(e) {
    # Fall back to original air quality data if both fail
    cat("Extended air quality data not found, trying original data...\n")
    tryCatch({
      df <- read.csv("air_quality_results/johannesburg_air_quality_fixed.csv", stringsAsFactors = FALSE)
      df$date <- as.Date(df$date)
      cat("Successfully loaded original air quality data with", nrow(df), "records\n")
      df
    }, error = function(e) {
      cat("Error loading air quality data:", e$message, "\n")
      NULL
    })
  })
})

# Add this to your air quality data loading section

# Load air quality data
aq_data <- read.csv("soweto_air_quality_data_extended.csv", stringsAsFactors = FALSE)

# Convert date to Date object
aq_data$date <- as.Date(aq_data$date)

# Check for PM2.5 data
if ("pm25" %in% names(aq_data)) {
  cat("PM2.5 data found in air quality dataset\n")
  cat("PM2.5 range:", min(aq_data$pm25, na.rm = TRUE), "to", 
      max(aq_data$pm25, na.rm = TRUE), "\n")
  cat("Number of non-NA PM2.5 values:", sum(!is.na(aq_data$pm25)), "out of", nrow(aq_data), "\n")
  
  # Add PM2.5 to overlap_data
  if (exists("overlap_data")) {
    overlap_data <- overlap_data %>%
      left_join(aq_data %>% select(date, pm25), by = "date")
    
    cat("PM2.5 data added to overlap dataset\n")
    cat("Number of non-NA PM2.5 values in overlap data:", 
        sum(!is.na(overlap_data$pm25)), "out of", nrow(overlap_data), "\n")
  }
}

# Combine climate datasets
climate_data <- NULL
if (!is.null(temp_data)) {
  climate_data <- temp_data %>%
    rename_with(~ ifelse(. %in% c("mean_temp_c", "min_temp_c", "max_temp_c"), ., .)) %>%
    rename_with(~ ifelse(. == "precipitation_mm", "precipitation", .))
  
  # Add CHIRPS precipitation data if available and not already included
  if (!is.null(chirps_data) && !"precipitation" %in% names(climate_data)) {
    climate_data <- climate_data %>%
      left_join(chirps_data %>% select(date, precipitation), by = "date")
  }
  
  # Modify the climate data creation part to check for available columns
  if (!is.null(air_quality_data)) {
    # Check which air quality columns are available
    available_aq_cols <- intersect(names(air_quality_data), c("pm25", "pm10", "no2", "o3", "tropospheric_no2"))
    
    if (length(available_aq_cols) > 0) {
      # Only select columns that actually exist
      aq_cols_to_join <- c("date", available_aq_cols)
      climate_data <- climate_data %>%
        left_join(air_quality_data %>% select(all_of(aq_cols_to_join)), by = "date")
      
      cat("Added air quality variables:", paste(available_aq_cols, collapse=", "), "\n")
    } else {
      cat("No usable air quality variables found in the data\n")
    }
  }
  
  cat("Combined climate dataset created with", nrow(climate_data), "records\n")
  cat("Climate data date range:", format(min(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(climate_data$date, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("Temperature data coverage:", sum(!is.na(climate_data$mean_temp_c)), "days\n")
  
  if ("precipitation" %in% names(climate_data)) {
    cat("Precipitation data coverage:", sum(!is.na(climate_data$precipitation)), "days\n")
  }
  
  if ("pm25" %in% names(climate_data)) {
    cat("Air quality data coverage:", sum(!is.na(climate_data$pm25)), "days\n")
  }
} else {
  cat("Could not create climate dataset due to missing data\n")
}

# Fill in any missing temperature data using interpolation
if (!is.null(climate_data)) {
  # Check for missing temperature values
  missing_temp <- sum(is.na(climate_data$mean_temp_c))
  if (missing_temp > 0) {
    cat("Filling in", missing_temp, "missing temperature values using interpolation...\n")
    
    # Create a time series object
    ts_data <- ts(climate_data$mean_temp_c, frequency = 365)
    
    # Use na.approx from the zoo package for interpolation
    if (!require(zoo)) {
      install.packages("zoo")
      library(zoo)
    }
    
    # Interpolate missing values
    climate_data$mean_temp_c <- as.numeric(na.approx(climate_data$mean_temp_c))
    
    # Also interpolate min and max temperatures if they exist
    if ("min_temp_c" %in% names(climate_data)) {
      climate_data$min_temp_c <- as.numeric(na.approx(climate_data$min_temp_c))
    }
    
    if ("max_temp_c" %in% names(climate_data)) {
      climate_data$max_temp_c <- as.numeric(na.approx(climate_data$max_temp_c))
    }
    
    cat("Temperature interpolation complete\n")
  } else {
    cat("No missing temperature values found\n")
  }
}

#----- 2. LOAD AND PREPARE HEALTH DATA -----#

# Load ChAdOx data
cat("Loading ChAdOx datasets...\n")
chadox_data_loaded <- tryCatch({
  load("ChAdOx data 2024-07-19.Rdata")
  load("ChAdOx supp visit data 2024-07-19.Rdata")
  cat("Successfully loaded ChAdOx datasets\n")
  TRUE
}, error = function(e) {
  cat("Error loading ChAdOx datasets:", e$message, "\n")
  FALSE
})

health_daily <- NULL
if (chadox_data_loaded && exists("illness_visit") && exists("covid_cases")) {
  # Process illness visit data
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
  
  # Process COVID cases data
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
  
  # Combine health datasets
  cat("Combining health datasets...\n")
  health_data <- illness_data %>%
    left_join(covid_data, by = c("record_id", "visit_date")) %>%
    mutate(
      # Create binary indicators for analysis
      fever_binary = ifelse(fever == "Yes" | temperature > 38, 1, 0),
      resp_binary = ifelse(respiratory_symptoms %in% c("Moderate", "Severe"), 1, 0),
      hosp_binary = ifelse(covid_hospitalized == "Yes", 1, 0)
    )
  
  # Aggregate to daily level
  cat("Aggregating health data to daily level...\n")
  health_daily <- health_data %>%
    group_by(visit_date) %>%
    summarize(
      avg_temperature = mean(temperature, na.rm = TRUE),
      fever_cases = sum(fever_binary, na.rm = TRUE),
      resp_cases = sum(resp_binary, na.rm = TRUE),
      hosp_cases = sum(hosp_binary, na.rm = TRUE),
      total_visits = n(),
      .groups = "drop"
    ) %>%
    # Calculate rates
    mutate(
      fever_rate = fever_cases / total_visits,
      resp_rate = resp_cases / total_visits,
      hosp_rate = hosp_cases / total_visits
    )
  
  cat("Health data preparation complete\n")
  cat("Daily health dataset dimensions:", nrow(health_daily), "rows,", ncol(health_daily), "columns\n")
  cat("Health data date range:", format(min(health_daily$visit_date, na.rm = TRUE), "%Y-%m-%d"), "to", 
      format(max(health_daily$visit_date, na.rm = TRUE), "%Y-%m-%d"), "\n")
} else {
  cat("Could not process health data due to missing datasets\n")
}

#----- 3. ANALYZE DATA SEPARATELY -----#

# Analyze temperature data
if (!is.null(climate_data)) {
  cat("\n--- CLIMATE DATA ANALYSIS ---\n")
  
  # Monthly temperature summary
  climate_data$month <- format(climate_data$date, "%Y-%m")
  monthly_temp <- climate_data %>%
    group_by(month) %>%
    summarize(
      avg_temp = mean(mean_temp_c, na.rm = TRUE),
      max_temp = max(max_temp_c, na.rm = TRUE),
      min_temp = min(min_temp_c, na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("Monthly Temperature Summary:\n")
  print(monthly_temp)
  
  # Plot temperature time series
  png("temperature_timeseries_analysis.png", width = 1000, height = 600)
  plot(climate_data$date, climate_data$mean_temp_c, type = "l", col = "red",
       main = "Soweto Temperature During COVID-19 Pandemic",
       xlab = "Date", ylab = "Temperature (°C)",
       ylim = c(min(climate_data$min_temp_c, na.rm = TRUE) - 2, 
                max(climate_data$max_temp_c, na.rm = TRUE) + 2))
  lines(climate_data$date, climate_data$max_temp_c, col = "orange", lty = 2)
  lines(climate_data$date, climate_data$min_temp_c, col = "blue", lty = 2)
  legend("topright", legend = c("Mean", "Maximum", "Minimum"),
         col = c("red", "orange", "blue"), lty = c(1, 2, 2))
  dev.off()
  cat("Temperature time series plot saved to temperature_timeseries_analysis.png\n")
}

# Analyze health data
if (!is.null(health_daily)) {
  cat("\n--- HEALTH DATA ANALYSIS ---\n")
  
  # Monthly health summary
  health_daily$month <- format(health_daily$visit_date, "%Y-%m")
  monthly_health <- health_daily %>%
    group_by(month) %>%
    summarize(
      total_visits = sum(total_visits),
      fever_cases = sum(fever_cases),
      resp_cases = sum(resp_cases),
      hosp_cases = sum(hosp_cases),
      fever_rate = sum(fever_cases) / sum(total_visits),
      resp_rate = sum(resp_cases) / sum(total_visits),
      hosp_rate = sum(hosp_cases) / sum(total_visits),
      .groups = "drop"
    )
  
  cat("Monthly Health Outcomes:\n")
  print(monthly_health)
  
  # Plot health outcomes time series
  png("health_outcomes_analysis.png", width = 1000, height = 800)
  par(mfrow = c(2, 1), mar = c(4, 4, 2, 4))
  
  # Plot case counts
  plot(health_daily$visit_date, health_daily$resp_cases, type = "l", col = "blue",
       main = "COVID-19 Health Outcomes Over Time",
       xlab = "", ylab = "Number of Cases")
  lines(health_daily$visit_date, health_daily$fever_cases, col = "red")
  lines(health_daily$visit_date, health_daily$hosp_cases, col = "darkgreen")
  legend("topright", legend = c("Respiratory Symptoms", "Fever", "Hospitalizations"),
         col = c("blue", "red", "darkgreen"), lty = 1)
  
  # Plot rates
  plot(health_daily$visit_date, health_daily$resp_rate, type = "l", col = "blue",
       main = "",
       xlab = "Date", ylab = "Rate (cases/total visits)")
  lines(health_daily$visit_date, health_daily$fever_rate, col = "red")
  lines(health_daily$visit_date, health_daily$hosp_rate, col = "darkgreen")
  
  dev.off()
  cat("Health outcomes plot saved to health_outcomes_analysis.png\n")
  
  # Seasonal analysis
  health_daily$season <- case_when(
    month(health_daily$visit_date) %in% c(12, 1, 2) ~ "Summer",
    month(health_daily$visit_date) %in% c(3, 4, 5) ~ "Autumn",
    month(health_daily$visit_date) %in% c(6, 7, 8) ~ "Winter",
    month(health_daily$visit_date) %in% c(9, 10, 11) ~ "Spring",
    TRUE ~ NA_character_
  )
  
  seasonal_health <- health_daily %>%
    group_by(season) %>%
    summarize(
      days = n(),
      total_visits = sum(total_visits),
      fever_cases = sum(fever_cases),
      resp_cases = sum(resp_cases),
      hosp_cases = sum(hosp_cases),
      fever_rate = sum(fever_cases) / sum(total_visits),
      resp_rate = sum(resp_cases) / sum(total_visits),
      hosp_rate = sum(hosp_cases) / sum(total_visits),
      .groups = "drop"
    )
  
  cat("\nSeasonal Health Outcomes:\n")
  print(seasonal_health)
  
  # Plot seasonal outcomes
  png("seasonal_health_outcomes.png", width = 800, height = 600)
  barplot(t(as.matrix(seasonal_health[, c("resp_rate", "fever_rate", "hosp_rate")])), 
          beside = TRUE, 
          names.arg = seasonal_health$season,
          col = c("blue", "red", "darkgreen"),
          main = "Seasonal Health Outcomes",
          ylab = "Rate (cases/total visits)")
  legend("topright", 
         legend = c("Respiratory Symptoms", "Fever", "Hospitalizations"),
         fill = c("blue", "red", "darkgreen"))
  dev.off()
  cat("Seasonal health outcomes plot saved to seasonal_health_outcomes.png\n")
}

#----- 4. OVERLAP ANALYSIS -----#

cat("\n--- OVERLAP ANALYSIS ---\n")

# Print column names to debug
cat("Health data column names:", paste(names(health_daily), collapse=", "), "\n")
cat("Temperature data column names:", paste(names(temp_data), collapse=", "), "\n")
cat("Air quality data column names:", paste(names(air_quality_data), collapse=", "), "\n")

# Find the date column in health_daily
health_date_col <- intersect(c("date", "visit_date", "Date"), names(health_daily))

if (length(health_date_col) > 0) {
  # Create overlap dataset with more flexible joining
  health_date_col <- health_date_col[1]  # Use the first matching column
  cat("Using health data date column:", health_date_col, "\n")
  
  # Create a temporary health data frame with standardized date column
  health_temp <- health_daily
  names(health_temp)[names(health_temp) == health_date_col] <- "date"
  
  # Create the overlap dataset
  overlap_data <- health_temp %>%
    select(date, total_visits, fever_cases, resp_cases, hosp_cases) %>%
    left_join(temp_data %>% select(date, mean_temp_c, min_temp_c, max_temp_c), by = "date") %>%
    left_join(air_quality_data %>% select(date, no2, o3), by = "date")
  
  # Count days with both health and temperature data (regardless of air quality)
  has_overlap <- sum(!is.na(overlap_data$mean_temp_c) & !is.na(overlap_data$resp_cases)) > 30
  
  cat("Number of days with health data:", nrow(health_temp), "\n")
  cat("Number of days with temperature data:", sum(!is.na(overlap_data$mean_temp_c)), "\n")
  cat("Number of days with air quality data:", sum(!is.na(overlap_data$no2) | !is.na(overlap_data$o3)), "\n")
  cat("Number of days with both health and temperature data:", sum(!is.na(overlap_data$mean_temp_c) & !is.na(overlap_data$resp_cases)), "\n")
  
  # Proceed with DLNM analysis if we have sufficient health and temperature overlap
  if (has_overlap) {
    cat("\n--- DISTRIBUTED LAG NON-LINEAR MODEL ANALYSIS ---\n")
    
    # Define the pollutant variable first
    pollutant <- "no2"  # Default to NO2
    pollutant_name <- "NO2"

    # Check if the pollutant exists in the data
    if (!pollutant %in% names(overlap_data)) {
      # Try alternative pollutant names
      if ("tropospheric_no2" %in% names(overlap_data)) {
        pollutant <- "tropospheric_no2"
        pollutant_name <- "Tropospheric NO2"
      } else if ("o3" %in% names(overlap_data)) {
        pollutant <- "o3"
        pollutant_name <- "Ozone"
      } else {
        cat("No suitable air pollutant data found in the overlap period\n")
      }
    }

    # Now add the rescaling code
    if (pollutant %in% names(overlap_data)) {
      # Check the scale of NO2 values
      no2_max <- max(overlap_data[[pollutant]], na.rm = TRUE)
      no2_min <- min(overlap_data[[pollutant]], na.rm = TRUE)
      
      cat("\nOriginal", pollutant_name, "range:", no2_min, "to", no2_max, "\n")
      
      # If NO2 values are very small (less than 0.01), rescale them
      if (no2_max < 0.01) {
        # Create a rescaling factor (1000 or more to get values in a reasonable range)
        rescale_factor <- 1000
        if (no2_max * 1000 < 1) {
          # If values would still be very small after multiplying by 1000,
          # use a larger factor to get values roughly in the 0-10 range
          rescale_factor <- 10000
        }
        
        # Create a new rescaled column
        overlap_data[[paste0(pollutant, "_rescaled")]] <- overlap_data[[pollutant]] * rescale_factor
        
        # Update the pollutant variable to use the rescaled version
        pollutant_original <- pollutant
        pollutant <- paste0(pollutant, "_rescaled")
        
        # Update the pollutant name for plots
        pollutant_name <- paste0(pollutant_name, " (×", rescale_factor, ")")
        
        cat(pollutant_name, "values rescaled by factor of", rescale_factor, "\n")
        cat("New", pollutant_name, "range:", min(overlap_data[[pollutant]], na.rm = TRUE), 
            "to", max(overlap_data[[pollutant]], na.rm = TRUE), "\n")
      }
    }

    # Now proceed with the DLNM analysis
    if (exists("overlap_data") && 
        sum(!is.na(overlap_data$mean_temp_c)) > 30 && 
        sum(!is.na(overlap_data$resp_cases)) > 30) {
      
      # Create a time variable for the model
      overlap_data$time <- 1:nrow(overlap_data)
      
      # Create the cross-basis for temperature
      # This allows modeling non-linear and delayed effects
      cb_temp <- crossbasis(
        overlap_data$mean_temp_c, 
        lag = 14,  # Consider up to 14 days of lag
        argvar = list(fun = "ns", df = 3),  # Natural spline for temperature
        arglag = list(fun = "ns", df = 3)   # Natural spline for lag
      )
      
      # Fit the model for respiratory cases
      dlnm_model <- glm(
        resp_cases ~ cb_temp + ns(time, df = 4),  # Add time spline to control for trends
        family = quasipoisson(),  # Appropriate for count data
        data = overlap_data
      )
      
      # Summarize the model
      cat("DLNM Model Summary for Temperature and Respiratory Cases:\n")
      print(summary(dlnm_model))
      
      # Plot the overall effect of temperature
      png("temperature_dlnm_effect.png", width = 800, height = 600)
      plot(
        crosspred(
          cb_temp, dlnm_model, 
          at = seq(min(overlap_data$mean_temp_c, na.rm = TRUE), 
                   max(overlap_data$mean_temp_c, na.rm = TRUE), 
                   length.out = 50)
        ),
        xlab = "Temperature (°C)", 
        ylab = "Relative Risk", 
        main = "Overall effect of temperature on respiratory cases"
      )
      dev.off()
      cat("DLNM analysis plot saved to temperature_dlnm_effect.png\n")
      
      # If air quality data is available, add it to the model
      if (("no2" %in% names(overlap_data) && sum(!is.na(overlap_data$no2)) > 30) || 
          ("o3" %in% names(overlap_data) && sum(!is.na(overlap_data$o3)) > 30)) {
        
        # Create cross-basis for the selected pollutant
        cb_poll <- crossbasis(
          overlap_data[[pollutant]], 
          lag = 7,  # Consider up to 7 days of lag
          argvar = list(fun = "lin"),  # Linear function for pollutant
          arglag = list(fun = "ns", df = 2)  # Natural spline for lag
        )
        
        # Fit the model with both temperature and the pollutant
        dlnm_combined <- glm(
          resp_cases ~ cb_temp + cb_poll + ns(time, df = 4),
          family = quasipoisson(),
          data = overlap_data
        )
        
        cat(paste0("\nDLNM Model with Temperature and ", pollutant_name, ":\n"))
        print(summary(dlnm_combined))
        
        # Test for interaction between temperature and pollutant
        cat(paste0("\nTesting for interaction between temperature and ", pollutant_name, "...\n"))
        # Create interaction term
        overlap_data$temp_poll_interaction <- overlap_data$mean_temp_c * overlap_data[[pollutant]]
        
        # Fit model with interaction
        interaction_model <- glm(
          resp_cases ~ mean_temp_c + overlap_data[[pollutant]] + temp_poll_interaction + ns(time, df = 4),
          family = quasipoisson(),
          data = overlap_data
        )
        
        print(summary(interaction_model))
      }
    } else {
      cat("Insufficient data for DLNM analysis. Requires overlapping temperature and health data.\n")
    }
  } else {
    cat("Could not identify date column in health data. Available columns:", 
        paste(names(health_daily), collapse=", "), "\n")
    cat("Insufficient data for overlap analysis\n")
  }
}

# Make sure overlap_dates is defined for backward compatibility
if (!exists("overlap_dates") && exists("overlap_data")) {
  overlap_dates <- overlap_data$date[!is.na(overlap_data$mean_temp_c) & !is.na(overlap_data$resp_cases)]
}

# Add PM2.5 analysis if data is available
if ("pm25" %in% names(overlap_data)) {
  cat("\nAnalyzing PM2.5 effects...\n")
  
  # Create cross-basis for PM2.5
  cb_pm25 <- crossbasis(
    overlap_data$pm25,
    lag = c(0, 7),  # 0-7 day lag
    argvar = list(fun = "lin"),  # Linear function for PM2.5
    arglag = list(fun = "ns", df = 3)  # Natural spline for lag
  )
  
  # Fit the model with PM2.5
  dlnm_pm25 <- glm(
    resp_cases ~ cb_temp + cb_pm25 + ns(time, df = 4),
    family = quasipoisson(),
    data = overlap_data
  )
  
  # Print summary
  cat("\nPM2.5 DLNM Model Summary:\n")
  print(summary(dlnm_pm25))
}

#----- 5. HEAT-HEALTH-AIR POLLUTION INTERACTION ANALYSIS -----#

if (!is.null(climate_data) && !is.null(health_daily) && length(overlap_dates) > 0) {
  cat("\n--- HEAT-HEALTH-AIR POLLUTION INTERACTION ANALYSIS ---\n")
  
  # Check if overlap_data exists and has rows
  if (exists("overlap_data") && nrow(overlap_data) > 0) {
    # Create a dataset for interaction analysis
    interaction_data <- overlap_data
    
    # Check if we have temperature data
    has_temp_max <- "max_temp_c" %in% names(interaction_data) && sum(!is.na(interaction_data$max_temp_c)) > 0
    has_pm25 <- "pm25" %in% names(interaction_data) && sum(!is.na(interaction_data$pm25)) > 0
    
    # Define heat wave days (days with max temperature above 95th percentile)
    if (has_temp_max) {
      temp_threshold <- quantile(interaction_data$max_temp_c, 0.95, na.rm = TRUE)
      interaction_data$heat_wave <- interaction_data$max_temp_c > temp_threshold
      
      cat("Heat wave threshold (95th percentile):", round(temp_threshold, 1), "°C\n")
      cat("Number of heat wave days:", sum(interaction_data$heat_wave, na.rm = TRUE), "\n")
    } else {
      cat("Insufficient temperature data for heat wave analysis\n")
      interaction_data$heat_wave <- FALSE
    }
    
    # Define high pollution days (days with PM2.5 above 95th percentile)
    if (has_pm25) {
      pm25_threshold <- quantile(interaction_data$pm25, 0.95, na.rm = TRUE)
      interaction_data$high_pollution <- interaction_data$pm25 > pm25_threshold
      
      cat("High pollution threshold (95th percentile):", round(pm25_threshold, 1), "μg/m³\n")
      cat("Number of high pollution days:", sum(interaction_data$high_pollution, na.rm = TRUE), "\n")
    } else {
      cat("Insufficient air quality data for pollution analysis\n")
      interaction_data$high_pollution <- FALSE
    }
    
    # Analyze combined effects if we have both heat wave and pollution data
    if ((has_temp_max || has_pm25) && sum(!is.na(interaction_data$resp_rate)) > 0) {
      
      # Create combined categories
      interaction_data$exposure_category <- case_when(
        interaction_data$heat_wave & interaction_data$high_pollution ~ "Both Heat & Pollution",
        interaction_data$heat_wave & !interaction_data$high_pollution ~ "Heat Only",
        !interaction_data$heat_wave & interaction_data$high_pollution ~ "Pollution Only",
        TRUE ~ "Neither"
      )
      
      # Summarize health outcomes by exposure category
      exposure_summary <- interaction_data %>%
        group_by(exposure_category) %>%
        summarize(
          days = n(),
          avg_resp_rate = mean(resp_rate, na.rm = TRUE),
          avg_fever_rate = mean(fever_rate, na.rm = TRUE),
          avg_hosp_rate = mean(hosp_rate, na.rm = TRUE),
          .groups = "drop"
        )
      
      cat("\nHealth outcomes by exposure category:\n")
      print(exposure_summary)
      
      # Only create visualization if we have data
      if (nrow(exposure_summary) > 0 && 
          !all(is.na(exposure_summary$avg_resp_rate)) && 
          !all(is.na(exposure_summary$avg_fever_rate))) {
        
        tryCatch({
          # Create visualization of combined effects
          png("heat_pollution_interaction.png", width = 800, height = 600)
          
          # Prepare data for barplot
          barplot_data <- as.matrix(exposure_summary[, c("avg_resp_rate", "avg_fever_rate", "avg_hosp_rate")])
          rownames(barplot_data) <- exposure_summary$exposure_category
          
          # Check if we have valid data for plotting
          if (all(is.finite(barplot_data)) && nrow(barplot_data) > 0 && ncol(barplot_data) > 0) {
            # Create barplot
            barplot(t(barplot_data), 
                    beside = TRUE, 
                    col = c("blue", "red", "darkgreen"),
                    main = "Health Outcomes by Heat and Pollution Exposure",
                    xlab = "Exposure Category",
                    ylab = "Rate (cases/total visits)")
            
            legend("topright", 
                   legend = c("Respiratory Symptoms", "Fever", "Hospitalizations"),
                   fill = c("blue", "red", "darkgreen"))
          } else {
            # Create a simple text plot if data is invalid
            plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
            text(1, 1, "Insufficient data for heat-pollution interaction plot", cex = 1.2)
          }
          
          dev.off()
          cat("Heat-pollution interaction plot saved to heat_pollution_interaction.png\n")
        }, error = function(e) {
          cat("Error creating heat-pollution interaction plot:", e$message, "\n")
        })
      } else {
        cat("Insufficient data for heat-pollution interaction visualization\n")
      }
      
      # Statistical analysis of interaction effects - only if we have enough data
      if (nrow(interaction_data) >= 30 && 
          sum(interaction_data$heat_wave, na.rm = TRUE) > 5 && 
          sum(interaction_data$high_pollution, na.rm = TRUE) > 5) {
        
        cat("\nStatistical analysis of interaction effects:\n")
        
        # Model for respiratory symptoms
        tryCatch({
          resp_model <- glm(resp_rate ~ heat_wave * high_pollution, 
                           family = binomial(link = "logit"), 
                           data = interaction_data)
          summary_stats <- summary(resp_model)
          cat("Respiratory symptoms model:\n")
          print(summary_stats$coefficients)
        }, error = function(e) {
          cat("Error in respiratory symptoms model:", e$message, "\n")
        })
        
        # Model for fever
        tryCatch({
          fever_model <- glm(fever_rate ~ heat_wave * high_pollution, 
                             family = binomial(link = "logit"), 
                             data = interaction_data)
          summary_stats <- summary(fever_model)
          cat("\nFever model:\n")
          print(summary_stats$coefficients)
        }, error = function(e) {
          cat("Error in fever model:", e$message, "\n")
        })
      } else {
        cat("\nInsufficient data for statistical modeling of interaction effects\n")
      }
    } else {
      cat("\nCannot analyze heat-pollution interactions due to missing data\n")
    }
  } else {
    cat("No valid overlap data available for heat-health-air pollution analysis\n")
  }
} else {
  cat("\n--- HEAT-HEALTH-AIR POLLUTION INTERACTION ANALYSIS ---\n")
  cat("Insufficient data for heat-health-air pollution interaction analysis\n")
}

#----- 6. GENERATE ABSTRACT -----#

# Generate abstract based on available data
cat("\n--- GENERATING ABSTRACT ---\n")

# Define data availability flags
has_temp_data <- !is.null(climate_data) && nrow(climate_data) > 0
has_health_data <- !is.null(health_daily) && nrow(health_daily) > 0
has_overlap <- has_temp_data && has_health_data && length(intersect(climate_data$date, health_daily$visit_date)) > 0

# Generate abstract text
abstract_text <- "# Environmental Factors and COVID-19 in Soweto: Insights from Real Climate and Health Data\n\n"

# Objective
abstract_text <- paste0(abstract_text, 
                        "## Objective\n",
                        "This study aimed to characterize environmental factors (temperature and air quality) in Soweto during the COVID-19 pandemic ",
                        "and analyze health outcomes from COVID-19 cases, exploring potential relationships between climate variables and health outcomes.\n\n")

# Methods
methods_text <- "## Methods\n"
if (has_temp_data) {
  methods_text <- paste0(methods_text, 
                         "We analyzed temperature data for Soweto (", 
                         format(min(climate_data$date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(climate_data$date, na.rm = TRUE), "%B %Y"), ") ")
} else {
  methods_text <- paste0(methods_text, "We attempted to collect temperature data for Soweto ")
}

if (has_health_data) {
  methods_text <- paste0(methods_text, 
                         "and COVID-19 health outcomes (", 
                         format(min(health_daily$visit_date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(health_daily$visit_date, na.rm = TRUE), "%B %Y"), "). ")
} else {
  methods_text <- paste0(methods_text, "and COVID-19 health outcomes. ")
}

if (has_overlap) {
  # Count the number of overlapping days
  overlap_count <- sum(!is.na(overlap_data$mean_temp_c) & !is.na(overlap_data$resp_cases))
  
  methods_text <- paste0(methods_text, 
                         "With limited temporal overlap between datasets (", overlap_count, " days), ",
                         "we conducted both separate analyses of environmental patterns and health outcomes, ",
                         "as well as a preliminary correlation analysis for the overlapping period.\n\n")
} else {
  methods_text <- paste0(methods_text, 
                         "Due to limited or no temporal overlap between datasets, ",
                         "we conducted separate analyses of environmental patterns and health outcomes, ",
                         "focusing on descriptive statistics and temporal trends.\n\n")
}

abstract_text <- paste0(abstract_text, methods_text)

# Results
results_text <- "## Results\n"
if (has_temp_data) {
  results_text <- paste0(results_text, 
                         "During the study period (", 
                         format(min(climate_data$date, na.rm = TRUE), "%B %Y"), " to ", 
                         format(max(climate_data$date, na.rm = TRUE), "%B %Y"), 
                         "), Soweto experienced average temperatures of ", 
                         round(mean(climate_data$mean_temp_c, na.rm = TRUE), 1), "°C ",
                         "(range: ", round(min(climate_data$min_temp_c, na.rm = TRUE), 1), 
                         "°C to ", round(max(climate_data$max_temp_c, na.rm = TRUE), 1), "°C). ")
}

if (has_health_data) {
  # Find peak months for respiratory cases
  peak_resp_month <- monthly_health$month[which.max(monthly_health$resp_cases)]
  peak_resp_cases <- max(monthly_health$resp_cases)
  
  results_text <- paste0(results_text, 
                         "Health data analysis revealed seasonal patterns in respiratory symptoms and fever cases, ",
                         "with peak incidence in ", peak_resp_month, " (", peak_resp_cases, " respiratory cases). ")
  
  # Add seasonal patterns if available
  if (exists("seasonal_health") && nrow(seasonal_health) > 0) {
    highest_season <- seasonal_health$season[which.max(seasonal_health$resp_rate)]
    results_text <- paste0(results_text, 
                           "Respiratory symptoms were most prevalent during ", highest_season, 
                           " (rate: ", round(max(seasonal_health$resp_rate), 3), "). ")
  }
}

if (has_overlap) {
  # Count the number of overlapping days
  overlap_count <- sum(!is.na(overlap_data$mean_temp_c) & !is.na(overlap_data$resp_cases))
  
  results_text <- paste0(results_text, 
                         "The limited overlap between datasets (", overlap_count, " days) ",
                         "allowed for a preliminary correlation analysis, which suggested ")
  
  if (exists("cor_matrix")) {
    # Get the strongest correlation between temp and health
    temp_health_cor <- cor_matrix[1:3, 4:9]
    max_cor_val <- max(abs(temp_health_cor), na.rm = TRUE)
    max_cor_indices <- which(abs(temp_health_cor) == max_cor_val, arr.ind = TRUE)
    
    if (length(max_cor_indices) > 0) {
      temp_var <- rownames(temp_health_cor)[max_cor_indices[1, 1]]
      health_var <- colnames(temp_health_cor)[max_cor_indices[1, 2]]
      cor_direction <- ifelse(temp_health_cor[max_cor_indices[1, 1], max_cor_indices[1, 2]] > 0, 
                              "positive", "negative")
      
      results_text <- paste0(results_text, 
                             "a ", cor_direction, " correlation (r = ", 
                             round(temp_health_cor[max_cor_indices[1, 1], max_cor_indices[1, 2]], 2), 
                             ") between ", temp_var, " and ", health_var, ".")
    } else {
      results_text <- paste0(results_text, 
                             "no strong correlations between temperature variables and health outcomes.")
    }
  } else {
    results_text <- paste0(results_text, 
                           "potential relationships that require further investigation with more data.")
  }
} else {
  results_text <- paste0(results_text, 
                         "The limited or no overlap between datasets prevented direct correlation analysis ",
                         "between environmental factors and health outcomes.")
}

# Update the abstract text to include heat-health-air pollution interactions
if (exists("exposure_summary") && nrow(exposure_summary) > 0) {
  # Find the category with highest respiratory rate
  highest_category <- exposure_summary$exposure_category[which.max(exposure_summary$avg_resp_rate)]
  highest_resp_rate <- max(exposure_summary$avg_resp_rate)
  
  results_text <- paste0(results_text, 
                         " Analysis of combined heat and air pollution exposures revealed that ", 
                         highest_category, " days had the highest rates of respiratory symptoms ",
                         "(", round(highest_resp_rate, 3), "). ")
  
  # Add statistical findings if available
  if (exists("resp_model") && !is.null(resp_model)) {
    interaction_coef <- coef(resp_model)["heat_waveTRUE:high_pollutionTRUE"]
    interaction_p <- summary(resp_model)$coefficients["heat_waveTRUE:high_pollutionTRUE", "Pr(>|z|)"]
    
    if (!is.na(interaction_coef) && !is.na(interaction_p)) {
      interaction_significant <- interaction_p < 0.05
      interaction_direction <- ifelse(interaction_coef > 0, "synergistic", "antagonistic")
      
      if (interaction_significant) {
        results_text <- paste0(results_text, 
                               "Statistical analysis indicated a significant ", 
                               interaction_direction, 
                               " interaction between heat and air pollution (p=", 
                               round(interaction_p, 3), ").")
      } else {
        results_text <- paste0(results_text, 
                               "Statistical analysis did not detect a significant interaction ",
                               "between heat and air pollution (p=", 
                               round(interaction_p, 3), ").")
      }
    }
  }
}

abstract_text <- paste0(abstract_text, results_text, "\n\n")

# Conclusion
abstract_text <- paste0(abstract_text, 
                        "## Conclusion\n",
                        "This study provides valuable characterization of environmental conditions and COVID-19 health outcomes in Soweto during different phases of the pandemic. ",
                        "While comprehensive correlations could not be established due to data limitations, the analyses suggest seasonal patterns in both temperature and COVID-19 symptoms. ",
                        "Future research should prioritize concurrent collection of environmental and health data to enable more robust analysis of these relationships. ",
                        "These findings highlight the importance of integrated environmental health monitoring systems during public health emergencies.")

# Save abstract
writeLines(abstract_text, "covid_climate_analysis_abstract.md")
cat("Abstract saved to covid_climate_analysis_abstract.md\n")

cat("\nAnalysis complete!\n")

# Add this code after the existing DLNM analysis section

# Save DLNM model objects for later visualization
if (exists("cb_temp") && exists("dlnm_combined")) {
  save(cb_temp, cb_poll, dlnm_combined, overlap_data, pollutant, pollutant_name,
       file = "dlnm_models.RData")
  cat("DLNM model objects saved to dlnm_models.RData\n")
}

# Create visualization for NO2 effects
if (exists("cb_poll") && exists("dlnm_combined")) {
  # Create prediction for NO2 effects
  pred_no2 <- crosspred(
    cb_poll, dlnm_combined,
    at = seq(min(overlap_data[[pollutant]], na.rm = TRUE),
             max(overlap_data[[pollutant]], na.rm = TRUE),
             length.out = 50)
  )
  
  # Plot NO2 effects
  png("no2_dlnm_effect.png", width = 800, height = 600)
  plot(
    pred_no2,
    xlab = "NO2 Concentration",
    ylab = "Relative Risk",
    main = paste0("Overall effect of ", pollutant_name, " on respiratory cases")
  )
  dev.off()
  cat(paste0(pollutant_name, " DLNM analysis plot saved to no2_dlnm_effect.png\n"))
  
  # Create a 3D plot showing both lag and NO2 concentration
  png("no2_dlnm_3d.png", width = 800, height = 600)
  plot(
    pred_no2, 
    type = "3d",
    xlab = "NO2 Concentration", 
    ylab = "Lag (days)", 
    zlab = "RR",
    main = paste0("3D effect of ", pollutant_name, " on respiratory cases")
  )
  dev.off()
  cat(paste0(pollutant_name, " 3D DLNM analysis plot saved to no2_dlnm_3d.png\n"))
}

# Add this code to create a combined visualization

# Create combined temperature and NO2 visualization
if (exists("cb_temp") && exists("cb_poll") && exists("dlnm_combined")) {
  # Set up a 1x2 plotting layout
  png("combined_dlnm_effects.png", width = 1200, height = 600)
  par(mfrow = c(1, 2))
  
  # Temperature plot
  pred_temp <- crosspred(
    cb_temp, dlnm_combined,
    at = seq(min(overlap_data$mean_temp_c, na.rm = TRUE),
             max(overlap_data$mean_temp_c, na.rm = TRUE),
             length.out = 50)
  )
  
  plot(
    pred_temp,
    xlab = "Temperature (°C)",
    ylab = "Relative Risk",
    main = "Effect of temperature on respiratory cases"
  )
  
  # NO2 plot
  pred_no2 <- crosspred(
    cb_poll, dlnm_combined,
    at = seq(min(overlap_data[[pollutant]], na.rm = TRUE),
             max(overlap_data[[pollutant]], na.rm = TRUE),
             length.out = 50)
  )
  
  plot(
    pred_no2,
    xlab = paste0(pollutant_name, " Concentration"),
    ylab = "Relative Risk",
    main = paste0("Effect of ", pollutant_name, " on respiratory cases")
  )
  
  dev.off()
  cat("Combined DLNM effects plot saved to combined_dlnm_effects.png\n")
}

# Check for scaling issues
cat("\nChecking variable scales for potential issues...\n")
cat("Temperature range:", min(overlap_data$mean_temp_c, na.rm=TRUE), "to", 
    max(overlap_data$mean_temp_c, na.rm=TRUE), "\n")
cat("NO2 range:", min(overlap_data[[pollutant]], na.rm=TRUE), "to", 
    max(overlap_data[[pollutant]], na.rm=TRUE), "\n")

# If NO2 values are very small, they might need rescaling
if (max(overlap_data[[pollutant]], na.rm=TRUE) < 0.01) {
  cat("NO2 values are very small, consider rescaling for better interpretation\n")
  # Create a rescaled version for interpretation
  overlap_data$no2_rescaled <- overlap_data[[pollutant]] * 1000
  cat("Rescaled NO2 range:", min(overlap_data$no2_rescaled, na.rm=TRUE), "to", 
      max(overlap_data$no2_rescaled, na.rm=TRUE), "\n")
}
