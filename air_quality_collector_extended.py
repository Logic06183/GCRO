"""
Air Quality Data Collection Script for Soweto

This script retrieves air quality data for Soweto during the COVID-19 health data period
using Google Earth Engine (GEE) APIs. It collects NO2, O3, aerosol, PM2.5, and PM10 data.
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
    exit(1)

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

def collect_air_quality_data():
    """Collect air quality data for Soweto during the health data period"""
    print("Starting collection of air quality data for Soweto...")
    
    # Define the Soweto region
    soweto_coords = [
        [27.8500, -26.2500],
        [27.8500, -26.3500],
        [27.9500, -26.3500],
        [27.9500, -26.2500]
    ]
    soweto_region = ee.Geometry.Polygon([soweto_coords])
    
    # Define the date range to match your health data period
    start_date = '2020-07-01'  # A few days before health data starts
    end_date = '2022-03-20'    # A few days after health data ends
    
    print(f"Collecting air quality data for Soweto from {start_date} to {end_date}...")
    
    # Process in monthly chunks to avoid memory issues
    all_data = []
    
    # Generate list of month start dates
    start_dt = datetime.strptime(start_date, '%Y-%m-%d')
    end_dt = datetime.strptime(end_date, '%Y-%m-%d')
    
    current_dt = start_dt
    month_starts = []
    
    while current_dt <= end_dt:
        month_starts.append(current_dt.strftime('%Y-%m-%d'))
        # Move to first day of next month
        if current_dt.month == 12:
            current_dt = datetime(current_dt.year + 1, 1, 1)
        else:
            current_dt = datetime(current_dt.year, current_dt.month + 1, 1)
    
    # Add the end date to the list
    if month_starts[-1] != end_date:
        month_starts.append(end_date)
    
    # Process each month
    for i in range(len(month_starts) - 1):
        month_start = month_starts[i]
        month_end = month_starts[i + 1]
        
        print(f"Processing period {month_start} to {month_end}...")
        
        # 1. Sentinel-5P NO2 data
        try:
            print("  Collecting NO2 data...")
            s5p_no2 = (ee.ImageCollection('COPERNICUS/S5P/NRTI/L3_NO2')
                .filterDate(month_start, month_end)
                .filterBounds(soweto_region)
                .select(['NO2_column_number_density', 'tropospheric_NO2_column_number_density']))
            
            # 2. Sentinel-5P O3 data
            print("  Collecting O3 data...")
            s5p_o3 = (ee.ImageCollection('COPERNICUS/S5P/NRTI/L3_O3')
                .filterDate(month_start, month_end)
                .filterBounds(soweto_region)
                .select(['O3_column_number_density']))
            
            # 3. CAMS Global data for PM2.5 and PM10
            print("  Collecting PM2.5 and PM10 data...")
            cams = (ee.ImageCollection('ECMWF/CAMS/NRT')
                .filterDate(month_start, month_end)
                .filterBounds(soweto_region)
                .select(['particulate_matter_d_less_than_25_um_surface', 
                         'particulate_matter_d_less_than_10_um_surface']))
            
            # Function to calculate daily statistics for each dataset
            def calculate_daily_stats(image):
                date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
                stats = image.reduceRegion(
                    reducer=ee.Reducer.mean(),
                    geometry=soweto_region,
                    scale=10000,
                    maxPixels=1e9
                )
                return ee.Feature(None, {
                    'date': date,
                    'properties': stats
                })
            
            # Process each dataset to get daily values
            # NO2 data
            if s5p_no2.size().getInfo() > 0:
                no2_daily = s5p_no2.map(lambda img: calculate_daily_stats(img))
                no2_data = ee_to_pandas(no2_daily)
                print(f"    Retrieved {len(no2_data)} days of NO2 data")
            else:
                no2_data = pd.DataFrame()
                print("    No NO2 data available for this period")
            
            # O3 data
            if s5p_o3.size().getInfo() > 0:
                o3_daily = s5p_o3.map(lambda img: calculate_daily_stats(img))
                o3_data = ee_to_pandas(o3_daily)
                print(f"    Retrieved {len(o3_data)} days of O3 data")
            else:
                o3_data = pd.DataFrame()
                print("    No O3 data available for this period")
            
            # CAMS data
            if cams.size().getInfo() > 0:
                cams_daily = cams.map(lambda img: calculate_daily_stats(img))
                cams_data = ee_to_pandas(cams_daily)
                print(f"    Retrieved {len(cams_data)} days of PM data")
            else:
                cams_data = pd.DataFrame()
                print("    No PM data available for this period")
            
            # Combine the datasets
            # Create a date range for the month
            date_range = pd.date_range(start=month_start, end=month_end, freq='D')
            month_data = pd.DataFrame({'date': date_range.strftime('%Y-%m-%d')})
            
            # Merge with the air quality data
            if len(no2_data) > 0:
                month_data = month_data.merge(no2_data, on='date', how='left')
            
            if len(o3_data) > 0:
                month_data = month_data.merge(o3_data, on='date', how='left')
            
            if len(cams_data) > 0:
                month_data = month_data.merge(cams_data, on='date', how='left')
            
            all_data.append(month_data)
            print(f"  Successfully processed period {month_start} to {month_end}")
            
        except Exception as e:
            print(f"  Error processing period {month_start} to {month_end}: {e}")
            # Continue with next month instead of stopping completely
            continue
    
    if all_data:
        # Combine all months
        air_quality_data = pd.concat(all_data)
        
        # Clean up the data
        # Extract the air quality variables from the properties column
        # Add a safety check to handle non-dictionary values
        air_quality_data['no2'] = air_quality_data['properties'].apply(
            lambda x: x.get('NO2_column_number_density') if isinstance(x, dict) else None)
        air_quality_data['tropospheric_no2'] = air_quality_data['properties'].apply(
            lambda x: x.get('tropospheric_NO2_column_number_density') if isinstance(x, dict) else None)
        air_quality_data['o3'] = air_quality_data['properties'].apply(
            lambda x: x.get('O3_column_number_density') if isinstance(x, dict) else None)
        
        # Try to extract PM data if available
        try:
            air_quality_data['pm25'] = air_quality_data['properties'].apply(
                lambda x: x.get('particulate_matter_d_less_than_25_um_surface') if isinstance(x, dict) else None)
            air_quality_data['pm10'] = air_quality_data['properties'].apply(
                lambda x: x.get('particulate_matter_d_less_than_10_um_surface') if isinstance(x, dict) else None)
        except Exception as e:
            print(f"Error extracting PM data: {e}")
            print("Continuing with NO2 and O3 only")
        
        # Drop the properties column
        air_quality_data = air_quality_data.drop(columns=['properties'])
        
        # Save to CSV
        air_quality_data.to_csv('soweto_air_quality_data_extended.csv', index=False)
        print(f"Extended air quality data saved to soweto_air_quality_data_extended.csv with {len(air_quality_data)} records")
        
        # Create a map visualization
        try:
            print("Creating interactive air quality map visualization...")
            Map = geemap.Map()
            Map.centerObject(soweto_region, 10)
            
            # Add the NO2 layer
            s5p_no2_mean = s5p_no2.mean()
            no2_vis = {
                'min': 0,
                'max': 0.0002,
                'palette': ['black', 'blue', 'purple', 'cyan', 'green', 'yellow', 'red']
            }
            Map.addLayer(s5p_no2_mean, no2_vis, 'NO2')
            
            # Add the O3 layer
            s5p_o3_mean = s5p_o3.mean()
            o3_vis = {
                'min': 0.12,
                'max': 0.15,
                'palette': ['black', 'blue', 'green', 'yellow', 'red']
            }
            Map.addLayer(s5p_o3_mean, o3_vis, 'O3')
            
            # Add the PM2.5 layer
            cams_pm25_mean = cams.select('particulate_matter_d_less_than_25_um_surface').mean()
            pm25_vis = {
                'min': 0,
                'max': 50,
                'palette': ['blue', 'purple', 'cyan', 'green', 'yellow', 'red']
            }
            Map.addLayer(cams_pm25_mean, pm25_vis, 'PM2.5')
            
            # Add the Soweto region
            Map.addLayer(soweto_region, {}, 'Soweto')
            
            # Save the map
            Map.to_html('soweto_air_quality_map.html')
            print("Interactive air quality map saved to soweto_air_quality_map.html")
        except Exception as e:
            print(f"Error creating air quality map visualization: {e}")
    else:
        print("No air quality data was collected. Cannot create output files.")

# Run the data collection
if __name__ == "__main__":
    collect_air_quality_data() 