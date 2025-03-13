import ee
import pandas as pd
import datetime
import os

# Initialize the Earth Engine API
ee.Initialize()

# Define Soweto area (approximate coordinates)
# You may want to adjust these coordinates for more precision
soweto = ee.Geometry.Rectangle([
    27.8500, -26.2800,  # min longitude, min latitude
    27.9800, -26.1600   # max longitude, max latitude
])

# Define your study period
start_date = '2015-01-01'  # Replace with your actual start date
end_date = '2023-12-31'    # Replace with your actual end date

# Function to get ERA5 climate data
def get_era5_data(start_date, end_date, region):
    # Access the ERA5 dataset
    era5_dataset = ee.ImageCollection('ECMWF/ERA5/DAILY')
    
    # Filter by date
    era5_filtered = era5_dataset.filterDate(start_date, end_date)
    
    # Select relevant climate variables
    climate_vars = [
        'mean_2m_air_temperature',
        'minimum_2m_air_temperature', 
        'maximum_2m_air_temperature',
        'total_precipitation',
        'mean_sea_level_pressure',
        'u_component_of_wind_10m',
        'v_component_of_wind_10m'
    ]
    
    era5_selected = era5_filtered.select(climate_vars)
    
    # Calculate regional statistics
    def calculate_stats(image):
        stats = image.reduceRegion(
            reducer=ee.Reducer.mean(),
            geometry=region,
            scale=1000,  # Scale in meters (ERA5 is approximately 31km)
            maxPixels=1e9
        )
        
        # Get the date
        date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
        
        # Return a feature with properties - don't call getInfo() here
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
    
    # Map over the collection
    stats_collection = era5_selected.map(calculate_stats)
    
    # Get the data from the server - call getInfo() only once at the end
    return stats_collection

# Function to get CHIRPS precipitation data (higher resolution alternative)
def get_chirps_data(start_date, end_date, region):
    chirps = ee.ImageCollection('UCSB-CHG/CHIRPS/DAILY')
    
    # Filter by date
    chirps_filtered = chirps.filterDate(start_date, end_date)
    
    # Calculate regional statistics
    def calculate_stats(image):
        stats = image.reduceRegion(
            reducer=ee.Reducer.mean(),
            geometry=region,
            scale=5000,  # CHIRPS is 0.05 degrees (about 5km)
            maxPixels=1e9
        )
        
        # Get the date
        date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
        
        # Return a feature with properties
        return ee.Feature(None, {
            'date': date,
            'precipitation': stats.get('precipitation')
        })
    
    # Map over the collection
    stats_collection = chirps_filtered.map(calculate_stats)
    
    return stats_collection

# Get ERA5 data
try:
    print("Fetching ERA5 climate data...")
    era5_data = get_era5_data(start_date, end_date, soweto)
    era5_features = era5_data.getInfo()['features']
    
    # Convert to DataFrame
    era5_df = pd.DataFrame([
        {**feature['properties']} 
        for feature in era5_features
    ])
    
    # Save to CSV
    era5_df.to_csv('soweto_era5_climate_data.csv', index=False)
    print(f"ERA5 data saved to soweto_era5_climate_data.csv")
    
except Exception as e:
    print(f"Error fetching ERA5 data: {e}")

# Get CHIRPS precipitation data
try:
    print("Fetching CHIRPS precipitation data...")
    chirps_data = get_chirps_data(start_date, end_date, soweto)
    chirps_features = chirps_data.getInfo()['features']
    
    # Convert to DataFrame
    chirps_df = pd.DataFrame([
        {**feature['properties']} 
        for feature in chirps_features
    ])
    
    # Save to CSV
    chirps_df.to_csv('soweto_chirps_precipitation_data.csv', index=False)
    print(f"CHIRPS data saved to soweto_chirps_precipitation_data.csv")
    
except Exception as e:
    print(f"Error fetching CHIRPS data: {e}")

print("Climate data collection complete!") 