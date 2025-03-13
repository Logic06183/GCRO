"""
Climate Data Collection Script

This script retrieves climate data for a specified geographic area and date range
using Google Earth Engine (GEE) APIs. It collects temperature, precipitation, 
air quality metrics, and vegetation indices.
"""

import os
import time
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import ee
import geemap

# Initialize Earth Engine
try:
    ee.Initialize()
    print("Earth Engine initialized successfully")
except Exception as e:
    print(f"Error initializing Earth Engine: {e}")
    raise Exception("Earth Engine initialization failed. Please check your authentication.")

def ee_to_pandas(fc):
    """Convert Earth Engine FeatureCollection to pandas DataFrame"""
    # Get the feature collection as a list
    features = fc.getInfo()['features']
    
    # Extract properties from each feature
    data = []
    for feature in features:
        properties = feature['properties']
        data.append(properties)
    
    # Convert to pandas DataFrame
    return pd.DataFrame(data)

def fetch_climate_data(region_name, coordinates, start_date_str, end_date_str, output_dir="data"):
    """
    Fetch climate data for a specified region and date range.
    
    Parameters:
    -----------
    region_name : str
        Name of the region for which to fetch data
    coordinates : list
        List of [lng, lat] coordinates defining the region
    start_date_str : str
        Start date in 'YYYY-MM-DD' format
    end_date_str : str
        End date in 'YYYY-MM-DD' format
    output_dir : str, optional
        Directory to save output files
    
    Returns:
    --------
    pandas.DataFrame
        DataFrame containing climate data for the specified region and date range
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Fetching climate data for {region_name} from {start_date_str} to {end_date_str}")
    
    # Convert coordinates to Earth Engine geometry
    if isinstance(coordinates[0], list):  # Polygon
        area = ee.Geometry.Polygon(coordinates)
    else:  # Point
        area = ee.Geometry.Point(coordinates)
    
    # Parse dates
    start_date = datetime.strptime(start_date_str, '%Y-%m-%d')
    end_date = datetime.strptime(end_date_str, '%Y-%m-%d')
    
    # Generate date range
    date_range = []
    current_date = start_date
    while current_date <= end_date:
        date_range.append(current_date.strftime('%Y-%m-%d'))
        current_date += timedelta(days=1)
    
    # Convert dates to Earth Engine format
    start_date_ee = ee.Date(start_date_str)
    end_date_ee = ee.Date(end_date_str).advance(1, 'day')  # Add 1 day to include the end date
    
    # Create dataframe to store climate data
    climate_data = pd.DataFrame({'date': pd.date_range(start=start_date, end=end_date)})
    climate_data['region'] = region_name
    
    # Fetch temperature data
    try:
        print("Fetching temperature data...")
        
        # Get ERA5 temperature data
        era5_collection = ee.ImageCollection('ECMWF/ERA5/DAILY') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['mean_2m_air_temperature'])
        
        # Function to extract temperature data for a specific date
        def extract_temperature_data(date_str):
            date_ee = ee.Date(date_str)
            next_day = date_ee.advance(1, 'day')
            
            # Get ERA5 data for the date
            era5_filtered = era5_collection.filterDate(date_ee, next_day)
            if era5_filtered.size().getInfo() > 0:
                era5_image = era5_filtered.first()
                
                # Extract temperature data
                try:
                    temp_data = era5_image.reduceRegion(
                        reducer=ee.Reducer.mean(),
                        geometry=area,
                        scale=10000  # Scale in meters
                    ).get('mean_2m_air_temperature').getInfo()
                    
                    if temp_data is not None:
                        # Convert from Kelvin to Celsius
                        return temp_data - 273.15
                except Exception as e:
                    print(f"Error extracting temperature data for {date_str}: {e}")
            
            return None
        
        # Extract temperature data for each date
        temp_success = 0
        for date_str in date_range:
            try:
                temp = extract_temperature_data(date_str)
                if temp is not None:
                    idx = climate_data[climate_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        climate_data.loc[idx, 'temperature'] = temp
                        temp_success += 1
                # Add a small delay to avoid rate limiting
                time.sleep(0.1)
            except Exception as e:
                print(f"Error extracting temperature data for {date_str}: {e}")
        
        print(f"Successfully retrieved temperature data for {temp_success} out of {len(date_range)} days ({temp_success/len(date_range)*100:.1f}%)")
        
    except Exception as e:
        print(f"Error fetching temperature data: {e}")
        raise Exception("Temperature data collection failed. Please check your Earth Engine setup.")
    
    # Fetch precipitation data
    try:
        print("Fetching precipitation data...")
        
        # Get ERA5 precipitation data
        era5_precip_collection = ee.ImageCollection('ECMWF/ERA5/DAILY') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['total_precipitation'])
        
        # Function to extract precipitation data for a specific date
        def extract_precipitation_data(date_str):
            date_ee = ee.Date(date_str)
            next_day = date_ee.advance(1, 'day')
            
            # Get ERA5 data for the date
            era5_filtered = era5_precip_collection.filterDate(date_ee, next_day)
            if era5_filtered.size().getInfo() > 0:
                era5_image = era5_filtered.first()
                
                # Extract precipitation data
                try:
                    precip_data = era5_image.reduceRegion(
                        reducer=ee.Reducer.mean(),
                        geometry=area,
                        scale=10000  # Scale in meters
                    ).get('total_precipitation').getInfo()
                    
                    if precip_data is not None:
                        # Convert from m to mm (1m = 1000mm)
                        return precip_data * 1000
                except Exception as e:
                    print(f"Error extracting precipitation data for {date_str}: {e}")
            
            return None
        
        # Extract precipitation data for each date
        precip_success = 0
        for date_str in date_range:
            try:
                precip = extract_precipitation_data(date_str)
                if precip is not None:
                    idx = climate_data[climate_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        climate_data.loc[idx, 'precipitation'] = precip
                        precip_success += 1
                # Add a small delay to avoid rate limiting
                time.sleep(0.1)
            except Exception as e:
                print(f"Error extracting precipitation data for {date_str}: {e}")
        
        print(f"Successfully retrieved precipitation data for {precip_success} out of {len(date_range)} days ({precip_success/len(date_range)*100:.1f}%)")
        
    except Exception as e:
        print(f"Error fetching precipitation data: {e}")
        raise Exception("Precipitation data collection failed. Please check your Earth Engine setup.")
    
    # Fetch air quality data from multiple sources
    try:
        print("Fetching air quality data...")
        
        # Get CAMS NRT data for PM2.5 and PM10
        cams_collection = ee.ImageCollection('ECMWF/CAMS/NRT') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['particulate_matter_d_less_than_25_um_surface',
                     'particulate_matter_d_less_than_10_um_surface'])
        
        # Get Sentinel-5P data for NO2
        no2_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_NO2') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['tropospheric_NO2_column_number_density'])
        
        # Get Sentinel-5P data for O3
        o3_collection = ee.ImageCollection('COPERNICUS/S5P/OFFL/L3_O3') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['O3_column_number_density'])
        
        # Check if we have data in each collection
        cams_size = cams_collection.size().getInfo()
        no2_size = no2_collection.size().getInfo()
        o3_size = o3_collection.size().getInfo()
        
        print(f"Found {cams_size} days of CAMS NRT data")
        print(f"Found {no2_size} days of Sentinel-5P NO2 data")
        print(f"Found {o3_size} days of Sentinel-5P O3 data")
        
        # Get a sample image to check available bands
        if cams_size > 0:
            sample_image = cams_collection.first()
            available_bands = sample_image.bandNames().getInfo()
            print(f"Available bands in CAMS dataset: {available_bands}")
        
        # Function to extract air quality data for a specific date
        def extract_air_quality_data(date_str):
            date_ee = ee.Date(date_str)
            next_day = date_ee.advance(1, 'day')
            
            result = {'date': date_str}
            
            # Get CAMS data for PM2.5 and PM10
            if cams_size > 0:
                cams_filtered = cams_collection.filterDate(date_ee, next_day)
                if cams_filtered.size().getInfo() > 0:
                    cams_image = cams_filtered.first()
                    
                    # Extract PM2.5 data
                    try:
                        pm25_data = cams_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('particulate_matter_d_less_than_25_um_surface').getInfo()
                        
                        if pm25_data is not None:
                            result['pm25'] = pm25_data
                    except Exception as e:
                        print(f"Error extracting PM2.5 data for {date_str}: {e}")
                    
                    # Extract PM10 data
                    try:
                        pm10_data = cams_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('particulate_matter_d_less_than_10_um_surface').getInfo()
                        
                        if pm10_data is not None:
                            result['pm10'] = pm10_data
                    except Exception as e:
                        print(f"Error extracting PM10 data for {date_str}: {e}")
            
            # Get NO2 data
            if no2_size > 0:
                no2_filtered = no2_collection.filterDate(date_ee, next_day)
                if no2_filtered.size().getInfo() > 0:
                    no2_image = no2_filtered.first()
                    
                    # Extract NO2 data
                    try:
                        no2_data = no2_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('tropospheric_NO2_column_number_density').getInfo()
                        
                        if no2_data is not None:
                            # Convert from mol/m^2 to μg/m^3 (approximate conversion)
                            # Multiply by 1e6 to convert from mol/m^2 to μmol/m^2
                            # Then multiply by 46 (molecular weight of NO2)
                            # Then divide by approximate column height (~10km = 10000m)
                            no2_value = (no2_data * 1e6 * 46) / 10000
                            result['no2'] = no2_value
                    except Exception as e:
                        print(f"Error extracting NO2 data for {date_str}: {e}")
            
            # Get O3 data
            if o3_size > 0:
                o3_filtered = o3_collection.filterDate(date_ee, next_day)
                if o3_filtered.size().getInfo() > 0:
                    o3_image = o3_filtered.first()
                    
                    # Extract O3 data
                    try:
                        o3_data = o3_image.reduceRegion(
                            reducer=ee.Reducer.mean(),
                            geometry=area,
                            scale=10000  # Scale in meters
                        ).get('O3_column_number_density').getInfo()
                        
                        if o3_data is not None:
                            # Convert from mol/m^2 to μg/m^3 (approximate conversion)
                            # Multiply by 1e6 to convert from mol/m^2 to μmol/m^2
                            # Then multiply by 48 (molecular weight of O3)
                            # Then divide by approximate column height (~10km = 10000m)
                            o3_value = (o3_data * 1e6 * 48) / 10000
                            result['o3'] = o3_value
                    except Exception as e:
                        print(f"Error extracting O3 data for {date_str}: {e}")
            
            if len(result) > 1:  # More than just date
                return result
            else:
                return None
        
        # Extract data for each date
        air_quality_success = {'pm25': 0, 'pm10': 0, 'no2': 0, 'o3': 0}
        for date_str in date_range:
            try:
                data = extract_air_quality_data(date_str)
                if data:
                    idx = climate_data[climate_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        if 'pm25' in data:
                            climate_data.loc[idx, 'pm25'] = data['pm25']
                            air_quality_success['pm25'] += 1
                        if 'pm10' in data:
                            climate_data.loc[idx, 'pm10'] = data['pm10']
                            air_quality_success['pm10'] += 1
                        if 'no2' in data:
                            climate_data.loc[idx, 'no2'] = data['no2']
                            air_quality_success['no2'] += 1
                        if 'o3' in data:
                            climate_data.loc[idx, 'o3'] = data['o3']
                            air_quality_success['o3'] += 1
                # Add a small delay to avoid rate limiting
                time.sleep(0.1)
            except Exception as e:
                print(f"Error extracting air quality data for {date_str}: {e}")
        
        print(f"Successfully retrieved air quality data:")
        print(f"  PM2.5: {air_quality_success['pm25']} out of {len(date_range)} days ({air_quality_success['pm25']/len(date_range)*100:.1f}%)")
        print(f"  PM10: {air_quality_success['pm10']} out of {len(date_range)} days ({air_quality_success['pm10']/len(date_range)*100:.1f}%)")
        print(f"  NO2: {air_quality_success['no2']} out of {len(date_range)} days ({air_quality_success['no2']/len(date_range)*100:.1f}%)")
        print(f"  O3: {air_quality_success['o3']} out of {len(date_range)} days ({air_quality_success['o3']/len(date_range)*100:.1f}%)")
        
    except Exception as e:
        print(f"Error fetching air quality data: {e}")
        raise Exception("Air quality data collection failed. Please check your Earth Engine setup.")
    
    # Fetch NDVI data (vegetation index)
    try:
        print("Fetching NDVI data...")
        
        # Get MODIS NDVI data
        ndvi_collection = ee.ImageCollection('MODIS/006/MOD13Q1') \
            .filterDate(start_date_ee, end_date_ee) \
            .select(['NDVI'])
        
        # Function to extract NDVI data for a specific date
        def extract_ndvi_data(date_str):
            date_ee = ee.Date(date_str)
            
            # MODIS data is usually every 16 days, so we need to get the closest image
            # Get a 32-day window around the date to ensure we capture at least one image
            window_start = date_ee.advance(-16, 'day')
            window_end = date_ee.advance(16, 'day')
            
            ndvi_filtered = ndvi_collection.filterDate(window_start, window_end)
            if ndvi_filtered.size().getInfo() > 0:
                # Sort by distance from target date
                def add_date_distance(image):
                    image_date = ee.Date(image.get('system:time_start'))
                    diff_days = image_date.difference(date_ee, 'day').abs()
                    return image.set('date_diff', diff_days)
                
                ndvi_filtered = ndvi_filtered.map(add_date_distance)
                ndvi_sorted = ndvi_filtered.sort('date_diff')
                ndvi_image = ndvi_sorted.first()
                
                # Extract NDVI data
                try:
                    ndvi_data = ndvi_image.reduceRegion(
                        reducer=ee.Reducer.mean(),
                        geometry=area,
                        scale=1000  # Scale in meters
                    ).get('NDVI').getInfo()
                    
                    if ndvi_data is not None:
                        # NDVI comes scaled by 10000, so convert to standard -1 to 1 range
                        return ndvi_data / 10000
                except Exception as e:
                    print(f"Error extracting NDVI data for {date_str}: {e}")
            
            return None
        
        # Extract NDVI data for each date in weekly intervals to save API calls
        # MODIS data is every 16 days, so daily values aren't meaningful
        weekly_dates = date_range[::7]
        if len(weekly_dates) > 0 and weekly_dates[-1] != date_range[-1]:
            weekly_dates.append(date_range[-1])
            
        ndvi_success = 0
        weekly_ndvi_values = {}
        
        for date_str in weekly_dates:
            try:
                ndvi = extract_ndvi_data(date_str)
                if ndvi is not None:
                    weekly_ndvi_values[date_str] = ndvi
                    ndvi_success += 1
                # Add a small delay to avoid rate limiting
                time.sleep(0.1)
            except Exception as e:
                print(f"Error extracting NDVI data for {date_str}: {e}")
        
        # Fill in all dates with the closest weekly value
        for date_str in date_range:
            date_obj = datetime.strptime(date_str, '%Y-%m-%d')
            if weekly_ndvi_values:  # Only proceed if we have at least one value
                closest_date = min(weekly_dates, key=lambda d: abs(datetime.strptime(d, '%Y-%m-%d') - date_obj))
                
                if closest_date in weekly_ndvi_values:
                    idx = climate_data[climate_data['date'] == pd.to_datetime(date_str)].index
                    if len(idx) > 0:
                        climate_data.loc[idx, 'ndvi'] = weekly_ndvi_values[closest_date]
        
        ndvi_available = climate_data['ndvi'].notna().sum()
        print(f"Successfully retrieved NDVI data for {ndvi_available} out of {len(date_range)} days ({ndvi_available/len(date_range)*100:.1f}%)")
        
    except Exception as e:
        print(f"Error fetching NDVI data: {e}")
        raise Exception("NDVI data collection failed. Please check your Earth Engine setup.")
    
    # Save data to CSV
    output_file = os.path.join(output_dir, f"{region_name.replace(' ', '_').lower()}_climate_data.csv")
    climate_data.to_csv(output_file, index=False)
    print(f"Climate data saved to {output_file}")
    
    return climate_data

def collect_soweto_extended_data():
    """Collect extended climate data for Soweto from 2015 to 2022"""
    print("Starting collection of extended climate data for Soweto...")
    
    # Define the Soweto region
    soweto_coords = [
        [27.8500, -26.2500],
        [27.8500, -26.3500],
        [27.9500, -26.3500],
        [27.9500, -26.2500]
    ]
    soweto_region = ee.Geometry.Polygon([soweto_coords])
    
    # Define the extended date range to match your health data
    start_date = '2015-01-01'
    end_date = '2022-03-31'  # Extended to cover your health data period
    
    print(f"Collecting ERA5 data for Soweto from {start_date} to {end_date}...")
    
    # Process in one-year chunks to avoid memory issues
    years = range(2015, 2023)
    all_data = []
    
    for year in years:
        year_start = f"{year}-01-01"
        year_end = f"{year}-12-31" if year < 2022 else "2022-03-31"
        
        print(f"Processing year {year} ({year_start} to {year_end})...")
        
        # Choose dataset based on year
        # Use ERA5 for 2015-2020, ERA5-Land for 2021-2022
        if year <= 2020:
            print(f"  Using ERA5 dataset for {year}")
            era5_dataset = ee.ImageCollection('ECMWF/ERA5/DAILY')
            
            # Select relevant climate variables for ERA5
            climate_vars = [
                'mean_2m_air_temperature',
                'minimum_2m_air_temperature', 
                'maximum_2m_air_temperature',
                'total_precipitation',
                'mean_sea_level_pressure',
                'u_component_of_wind_10m',
                'v_component_of_wind_10m'
            ]
        else:
            print(f"  Using ERA5-Land dataset for {year}")
            era5_dataset = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_RAW')
            
            # Select relevant climate variables for ERA5-Land
            # Updated with correct band names from the error message
            climate_vars = [
                'temperature_2m',  # Mean 2m air temperature
                'temperature_2m_min', 
                'temperature_2m_max',
                'total_precipitation_sum',  # Note the _sum suffix
                # ERA5-Land doesn't have sea level pressure
                'u_component_of_wind_10m',  # Same name
                'v_component_of_wind_10m'   # Same name
            ]
        
        # Filter by date and region
        era5_filtered = era5_dataset.filterDate(year_start, year_end).filterBounds(soweto_region)
        
        era5_selected = era5_filtered.select(climate_vars)
        
        # Function to calculate statistics for each day
        def calculate_stats(image):
            date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
            stats = image.reduceRegion(
                reducer=ee.Reducer.mean(),
                geometry=soweto_region,
                scale=30000,
                maxPixels=1e9
            )
            
            # Create a feature with properties, handling different variable names
            if year <= 2020:
                return ee.Feature(None, {
                    'date': date,
                    'mean_2m_air_temperature': stats.get('mean_2m_air_temperature'),
                    'minimum_2m_air_temperature': stats.get('minimum_2m_air_temperature'),
                    'maximum_2m_air_temperature': stats.get('maximum_2m_air_temperature'),
                    'total_precipitation': stats.get('total_precipitation'),
                    'mean_sea_level_pressure': stats.get('mean_sea_level_pressure'),
                    'u_component_of_wind_10m': stats.get('u_component_of_wind_10m'),
                    'v_component_of_wind_10m': stats.get('v_component_of_wind_10m')
                })
            else:
                # For ERA5-Land, map to consistent column names with updated band names
                return ee.Feature(None, {
                    'date': date,
                    'mean_2m_air_temperature': stats.get('temperature_2m'),
                    'minimum_2m_air_temperature': stats.get('temperature_2m_min'),
                    'maximum_2m_air_temperature': stats.get('temperature_2m_max'),
                    'total_precipitation': stats.get('total_precipitation_sum'),
                    'mean_sea_level_pressure': None,  # Not available in ERA5-Land
                    'u_component_of_wind_10m': stats.get('u_component_of_wind_10m'),
                    'v_component_of_wind_10m': stats.get('v_component_of_wind_10m')
                })
        
        # Map over the collection
        stats_collection = era5_selected.map(calculate_stats)
        
        try:
            # Convert to pandas DataFrame with retries
            max_attempts = 3
            for attempt in range(max_attempts):
                try:
                    # Use our custom function instead of geemap.ee_to_pandas
                    year_data = ee_to_pandas(stats_collection)
                    print(f"  Successfully retrieved {len(year_data)} days of data for {year}")
                    all_data.append(year_data)
                    break
                except Exception as e:
                    if attempt < max_attempts - 1:
                        print(f"  Attempt {attempt+1} failed: {e}. Retrying...")
                        time.sleep(5)  # Wait 5 seconds before retrying
                    else:
                        print(f"  Failed to retrieve data for {year} after {max_attempts} attempts: {e}")
                        raise
        except Exception as e:
            print(f"Error processing year {year}: {e}")
            # Continue with next year instead of stopping completely
            continue
    
    if all_data:
        # Combine all years
        era5_data = pd.concat(all_data)
        
        # Save to CSV
        era5_data.to_csv('soweto_era5_climate_data_extended.csv', index=False)
        print(f"Extended ERA5 data saved to soweto_era5_climate_data_extended.csv with {len(era5_data)} records")
        
        # Create a map visualization
        try:
            print("Creating interactive map visualization...")
            Map = geemap.Map()
            Map.centerObject(soweto_region, 10)
            
            # Access the ERA5 dataset for visualization (full period)
            # Use ERA5 for visualization as it covers most of the period
            era5_dataset = ee.ImageCollection('ECMWF/ERA5/DAILY')
            era5_filtered = era5_dataset.filterDate('2015-01-01', '2020-12-31').filterBounds(soweto_region)
            
            # Add the mean temperature layer
            mean_temp = era5_filtered.select('mean_2m_air_temperature').mean()
            mean_temp_vis = {
                'min': 270,
                'max': 305,
                'palette': ['blue', 'purple', 'cyan', 'green', 'yellow', 'red']
            }
            Map.addLayer(mean_temp, mean_temp_vis, 'Mean Temperature')
            
            # Add the precipitation layer
            precip = era5_filtered.select('total_precipitation').mean()
            precip_vis = {
                'min': 0,
                'max': 0.01,
                'palette': ['white', 'blue', 'purple']
            }
            Map.addLayer(precip, precip_vis, 'Precipitation')
            
            # Add the Soweto region
            Map.addLayer(soweto_region, {}, 'Soweto')
            
            # Save the map
            Map.to_html('soweto_climate_map.html')
            print("Interactive map saved to soweto_climate_map.html")
        except Exception as e:
            print(f"Error creating map visualization: {e}")
    else:
        print("No data was collected. Cannot create output files.")

# Example usage
if __name__ == "__main__":
    # Choose which function to run
    mode = "extended"  # Change to "original" if you want to run the original example
    
    if mode == "extended":
        collect_soweto_extended_data()
    else:
        # Original example code
        region_name = "Johannesburg"
        coordinates = [28.0473, -26.2041]  # [longitude, latitude]
        
        # Define date range
        start_date = "2023-01-01"
        end_date = "2023-12-31"
        
        # Fetch climate data
        try:
            climate_data = fetch_climate_data(
                region_name=region_name,
                coordinates=coordinates,
                start_date_str=start_date,
                end_date_str=end_date,
                output_dir="climate_data"
            )
            
            # Display summary
            print("\nClimate Data Summary:")
            print(f"Total days: {len(climate_data)}")
            for column in climate_data.columns:
                if column not in ['date', 'region']:
                    not_null = climate_data[column].notna().sum()
                    percent = not_null/len(climate_data)*100
                    print(f"{column}: {not_null} days ({percent:.1f}%)")
            
            # Display head of the dataframe
            print("\nSample data:")
            print(climate_data.head())
        except Exception as e:
            print(f"ERROR: {e}")
            print("Data collection failed. Please check your Earth Engine setup and try again.")
