# Earth Engine Data Retrieval with Batch Processing and Caching
# This script retrieves climate data from Earth Engine in smaller batches
# and caches the results to avoid timeout issues

# Load necessary libraries
library(dplyr)
library(lubridate)
library(reticulate)

# Configure reticulate to use conda environment
use_condaenv("base", required = TRUE)

# Function to fetch climate data from Earth Engine in batches
fetch_climate_data_batch <- function(start_date, end_date, location = "Soweto", 
                                    batch_size = 30, cache_file = "climate_data_cache.RData") {
  
  # Check if cache file exists
  if (file.exists(cache_file)) {
    cat("Loading cached climate data...\n")
    load(cache_file)
    
    # Check if the cached data covers our date range
    if (min(climate_data$date) <= as.Date(start_date) && 
        max(climate_data$date) >= as.Date(end_date)) {
      cat("Using cached climate data that covers the requested date range\n")
      return(climate_data)
    } else {
      cat("Cached data doesn't cover the full date range. Retrieving new data...\n")
    }
  }
  
  # Initialize Earth Engine
  cat("Initializing Earth Engine...\n")
  ee <- reticulate::import("ee")
  ee$Initialize()
  cat("Earth Engine initialized successfully\n")
  
  # Define location (Soweto coordinates)
  soweto_point <- ee$Geometry$Point(c(27.8579, -26.2485))  # longitude, latitude
  
  # Create a sequence of dates
  all_dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  
  # Create an empty dataframe for all climate data
  climate_data <- data.frame(
    date = all_dates,
    temp_mean = NA,
    temp_min = NA,
    temp_max = NA,
    pm25 = NA
  )
  
  # Process data in batches
  batch_count <- ceiling(length(all_dates) / batch_size)
  cat("Processing", length(all_dates), "days in", batch_count, "batches\n")
  
  for (batch in 1:batch_count) {
    batch_start_idx <- (batch - 1) * batch_size + 1
    batch_end_idx <- min(batch * batch_size, length(all_dates))
    
    batch_start_date <- all_dates[batch_start_idx]
    batch_end_date <- all_dates[batch_end_idx]
    
    cat("Processing batch", batch, "of", batch_count, ":", 
        batch_start_date, "to", batch_end_date, "\n")
    
    # Convert dates to EE format
    start_date_ee <- ee$Date(as.character(batch_start_date))
    end_date_ee <- ee$Date(as.character(batch_end_date))
    
    # Get ERA5 temperature data
    cat("  Retrieving ERA5 temperature data...\n")
    era5_dataset <- ee$ImageCollection('ECMWF/ERA5/DAILY')$
      filterDate(start_date_ee, end_date_ee)
    
    # Get temperature data
    temp_data <- era5_dataset$select(c('mean_2m_air_temperature', 'minimum_2m_air_temperature', 'maximum_2m_air_temperature'))
    
    # Get CAMS air quality data
    cat("  Retrieving CAMS air quality data...\n")
    cams_dataset <- ee$ImageCollection('ECMWF/CAMS/NRT')$
      filterDate(start_date_ee, end_date_ee)
    
    # Check if we have any CAMS data for this batch
    if (cams_dataset$size()$getInfo() > 0) {
      # Get the first image to check band names
      first_image <- ee$Image(cams_dataset$first())
      band_names <- first_image$bandNames()$getInfo()
      cat("  Available bands in CAMS dataset:", paste(band_names, collapse=", "), "\n")
      
      # Find PM2.5 band using pattern matching
      pm25_band <- NULL
      for (band in band_names) {
        if (grepl("particulate.*25", band, ignore.case = TRUE)) {
          pm25_band <- band
          break
        }
      }
      
      if (!is.null(pm25_band)) {
        cat("  Using PM2.5 band:", pm25_band, "\n")
        
        # Get PM2.5 data with correct band name
        air_quality_data <- cams_dataset$select(pm25_band)
        
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
        
        # Create a sequence of dates for this batch
        batch_dates <- all_dates[batch_start_idx:batch_end_idx]
        
        # Process temperature data
        cat("  Processing temperature data...\n")
        temp_size <- as.integer(temp_data$size()$getInfo())
        for (i in 1:min(temp_size, length(batch_dates))) {
          feature <- ee$Feature(temp_list$get(i - 1))
          props <- feature$getInfo()$properties
          
          if (!is.null(props)) {
            # Find the matching date index
            date_str <- names(props)[grep("system:time_start", names(props))]
            if (length(date_str) > 0) {
              timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
              date_index <- which(abs(as.numeric(batch_dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
              
              if (length(date_index) > 0) {
                global_index <- batch_start_idx + date_index - 1
                climate_data$temp_mean[global_index] <- props[["mean_2m_air_temperature"]] - 273.15  # Convert from K to C
                climate_data$temp_min[global_index] <- props[["minimum_2m_air_temperature"]] - 273.15
                climate_data$temp_max[global_index] <- props[["maximum_2m_air_temperature"]] - 273.15
              }
            }
          }
        }
        
        # Process air quality data
        cat("  Processing air quality data...\n")
        aq_size <- as.integer(air_quality_data$size()$getInfo())
        for (i in 1:min(aq_size, length(batch_dates))) {
          feature <- ee$Feature(air_quality_list$get(i - 1))
          props <- feature$getInfo()$properties
          
          if (!is.null(props)) {
            # Find the matching date index
            date_str <- names(props)[grep("system:time_start", names(props))]
            if (length(date_str) > 0) {
              timestamp <- as.numeric(props[[date_str]]) / 1000  # Convert from milliseconds to seconds
              date_index <- which(abs(as.numeric(batch_dates) - as.numeric(as.Date(as.POSIXct(timestamp, origin = "1970-01-01")))) < 1)
              
              if (length(date_index) > 0) {
                global_index <- batch_start_idx + date_index - 1
                climate_data$pm25[global_index] <- props[[pm25_band]]
              }
            }
          }
        }
      } else {
        cat("  WARNING: Could not find PM2.5 band in CAMS dataset for this batch\n")
      }
    } else {
      cat("  WARNING: No CAMS data available for this batch\n")
    }
    
    # Save intermediate results after each batch
    save(climate_data, file = cache_file)
    cat("  Saved intermediate results to cache file\n")
    
    # Add a small delay to avoid rate limiting
    Sys.sleep(2)
  }
  
  # Calculate heat index
  cat("Calculating heat index...\n")
  climate_data$heat_index <- with(climate_data, {
    # Simplified heat index calculation
    # Using a simplified version since we don't have humidity
    temp_mean + (temp_max - temp_mean) * 0.2
  })
  
  # Check data completeness
  missing_temp <- sum(is.na(climate_data$temp_mean))
  missing_pm25 <- sum(is.na(climate_data$pm25))
  
  cat("Data completeness:\n")
  cat("  Temperature data:", round((1 - missing_temp / nrow(climate_data)) * 100, 1), "% complete\n")
  cat("  PM2.5 data:", round((1 - missing_pm25 / nrow(climate_data)) * 100, 1), "% complete\n")
  
  # Save final results
  save(climate_data, file = cache_file)
  cat("Climate data retrieval complete. Results saved to", cache_file, "\n")
  
  return(climate_data)
}

# Example usage:
# Determine date range from ChAdOx data
# load("ChAdOx data 2024-07-19.Rdata")
# load("ChAdOx supp visit data 2024-07-19.Rdata")
# chadox_dates <- as.Date(illness_visit$ev_dat)
# date_range <- range(chadox_dates, na.rm = TRUE)
# start_date <- date_range[1]
# end_date <- date_range[2]
# climate_data <- fetch_climate_data_batch(start_date, end_date, batch_size = 30)
