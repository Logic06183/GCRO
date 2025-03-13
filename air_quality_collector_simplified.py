"""
Simplified Air Quality Data Collection Script for Soweto
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

def collect_air_quality_data_simplified():
    """Collect air quality data for Soweto during the health data period"""
    print("Starting simplified collection of air quality data for Soweto...")
    
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
    
    # 1. Sentinel-5P NO2 data
    print("Collecting NO2 data...")
    s5p_no2 = (ee.ImageCollection('COPERNICUS/S5P/NRTI/L3_NO2')
        .filterDate(start_date, end_date)
        .filterBounds(soweto_region))
    
    # 2. Sentinel-5P O3 data
    print("Collecting O3 data...")
    s5p_o3 = (ee.ImageCollection('COPERNICUS/S5P/NRTI/L3_O3')
        .filterDate(start_date, end_date)
        .filterBounds(soweto_region))
    
    # Calculate daily means for NO2
    print("Calculating daily NO2 means...")
    no2_daily_means = []
    
    # Get a list of dates in the range
    start_dt = datetime.strptime(start_date, '%Y-%m-%d')
    end_dt = datetime.strptime(end_date, '%Y-%m-%d')
    date_list = [(start_dt + timedelta(days=x)).strftime('%Y-%m-%d') 
                for x in range((end_dt - start_dt).days + 1)]
    
    for date_str in date_list:
        try:
            # Filter to just this date
            date_start = date_str
            date_end = (datetime.strptime(date_str, '%Y-%m-%d') + timedelta(days=1)).strftime('%Y-%m-%d')
            
            # Get NO2 data for this date
            no2_day = s5p_no2.filterDate(date_start, date_end)
            
            if no2_day.size().getInfo() > 0:
                # Calculate mean NO2 for Soweto
                no2_mean = no2_day.mean().reduceRegion(
                    reducer=ee.Reducer.mean(),
                    geometry=soweto_region,
                    scale=10000,
                    maxPixels=1e9
                ).getInfo()
                
                no2_daily_means.append({
                    'date': date_str,
                    'no2': no2_mean.get('NO2_column_number_density'),
                    'tropospheric_no2': no2_mean.get('tropospheric_NO2_column_number_density')
                })
                
                if len(no2_daily_means) % 30 == 0:
                    print(f"  Processed {len(no2_daily_means)} days of NO2 data...")
        except Exception as e:
            print(f"  Error processing NO2 for {date_str}: {e}")
            continue
    
    # Calculate daily means for O3
    print("Calculating daily O3 means...")
    o3_daily_means = []
    
    for date_str in date_list:
        try:
            # Filter to just this date
            date_start = date_str
            date_end = (datetime.strptime(date_str, '%Y-%m-%d') + timedelta(days=1)).strftime('%Y-%m-%d')
            
            # Get O3 data for this date
            o3_day = s5p_o3.filterDate(date_start, date_end)
            
            if o3_day.size().getInfo() > 0:
                # Calculate mean O3 for Soweto
                o3_mean = o3_day.mean().reduceRegion(
                    reducer=ee.Reducer.mean(),
                    geometry=soweto_region,
                    scale=10000,
                    maxPixels=1e9
                ).getInfo()
                
                o3_daily_means.append({
                    'date': date_str,
                    'o3': o3_mean.get('O3_column_number_density')
                })
                
                if len(o3_daily_means) % 30 == 0:
                    print(f"  Processed {len(o3_daily_means)} days of O3 data...")
        except Exception as e:
            print(f"  Error processing O3 for {date_str}: {e}")
            continue
    
    # Convert to DataFrames
    no2_df = pd.DataFrame(no2_daily_means)
    o3_df = pd.DataFrame(o3_daily_means)
    
    # Merge the datasets
    print("Merging datasets...")
    air_quality_data = pd.DataFrame({'date': date_list})
    
    if len(no2_df) > 0:
        air_quality_data = air_quality_data.merge(no2_df, on='date', how='left')
        print(f"Added NO2 data: {len(no2_df)} days")
    
    if len(o3_df) > 0:
        air_quality_data = air_quality_data.merge(o3_df, on='date', how='left')
        print(f"Added O3 data: {len(o3_df)} days")
    
    # Save to CSV
    air_quality_data.to_csv('soweto_air_quality_data_simplified.csv', index=False)
    print(f"Simplified air quality data saved to soweto_air_quality_data_simplified.csv with {len(air_quality_data)} records")
    
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
        Map.addLayer(s5p_no2_mean.select('NO2_column_number_density'), no2_vis, 'NO2')
        
        # Add the O3 layer
        s5p_o3_mean = s5p_o3.mean()
        o3_vis = {
            'min': 0.12,
            'max': 0.15,
            'palette': ['black', 'blue', 'green', 'yellow', 'red']
        }
        Map.addLayer(s5p_o3_mean.select('O3_column_number_density'), o3_vis, 'O3')
        
        # Add the Soweto region
        Map.addLayer(soweto_region, {}, 'Soweto')
        
        # Save the map
        Map.to_html('soweto_air_quality_map_simplified.html')
        print("Interactive air quality map saved to soweto_air_quality_map_simplified.html")
    except Exception as e:
        print(f"Error creating air quality map visualization: {e}")

# Run the data collection
if __name__ == "__main__":
    collect_air_quality_data_simplified() 