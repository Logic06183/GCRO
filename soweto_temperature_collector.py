"""
Soweto Temperature Data Collector

This script collects temperature data for Soweto during the COVID-19 pandemic period
using Google Earth Engine's ERA5 Daily dataset.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime
import ee

# Initialize Earth Engine
try:
    ee.Initialize()
    print("Earth Engine initialized successfully")
except Exception as e:
    print(f"Error initializing Earth Engine: {e}")
    print("Please authenticate with 'earthengine authenticate' if needed")
    raise Exception("Earth Engine initialization failed")

def fetch_soweto_temperature(start_date_str, end_date_str, output_dir="temperature_data"):
    """
    Fetch temperature data for Soweto for a specified date range using ERA5 Daily dataset.
    
    Parameters:
    -----------
    start_date_str : str
        Start date in 'YYYY-MM-DD' format
    end_date_str : str
        End date in 'YYYY-MM-DD' format
    output_dir : str, optional
        Directory to save output files
    
    Returns:
    --------
    pandas.DataFrame
        DataFrame containing temperature data for Soweto
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Fetching temperature data for Soweto from {start_date_str} to {end_date_str}")
    
    # Define Soweto coordinates [longitude, latitude]
    soweto_coords = [27.9300, -26.2585]
    soweto_point = ee.Geometry.Point(soweto_coords)
    
    # Convert dates to Earth Engine format
    start_date_ee = ee.Date(start_date_str)
    end_date_ee = ee.Date(end_date_str).advance(1, 'day')  # Add 1 day to include the end date
    
    # Get ERA5 temperature data
    # ERA5 Daily dataset includes mean, min, and max temperature
    era5_collection = ee.ImageCollection('ECMWF/ERA5/DAILY') \
        .filterDate(start_date_ee, end_date_ee) \
        .select(['mean_2m_air_temperature', 'minimum_2m_air_temperature', 'maximum_2m_air_temperature'])
    
    # Function to extract data for a single image
    def extract_data(image):
        # Get the date of the image
        date = ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
        
        # Extract temperature values at Soweto point
        data = image.reduceRegion(
            reducer=ee.Reducer.first(),
            geometry=soweto_point,
            scale=27830  # ERA5 pixel size in meters
        )
        
        # Create a feature with the extracted data
        return ee.Feature(None, {
            'date': date,
            'mean_temp': data.get('mean_2m_air_temperature'),
            'min_temp': data.get('minimum_2m_air_temperature'),
            'max_temp': data.get('maximum_2m_air_temperature')
        })
    
    # Map the extraction function over the image collection
    features = era5_collection.map(extract_data)
    
    # Convert the feature collection to a list
    data_list = features.getInfo()['features']
    
    # Create a pandas DataFrame from the list
    rows = []
    for feature in data_list:
        props = feature['properties']
        # Convert from Kelvin to Celsius
        mean_temp = props.get('mean_temp')
        min_temp = props.get('min_temp')
        max_temp = props.get('max_temp')
        
        if mean_temp is not None:
            mean_temp = mean_temp - 273.15
        if min_temp is not None:
            min_temp = min_temp - 273.15
        if max_temp is not None:
            max_temp = max_temp - 273.15
            
        rows.append({
            'date': props.get('date'),
            'mean_temp_c': mean_temp,
            'min_temp_c': min_temp,
            'max_temp_c': max_temp
        })
    
    # Create DataFrame
    df = pd.DataFrame(rows)
    
    # Sort by date
    if not df.empty and 'date' in df.columns:
        df['date'] = pd.to_datetime(df['date'])
        df = df.sort_values('date')
    
    # Calculate data coverage
    if not df.empty:
        total_days = (pd.to_datetime(end_date_str) - pd.to_datetime(start_date_str)).days + 1
        available_days = df['mean_temp_c'].notna().sum()
        coverage_pct = (available_days / total_days) * 100
        print(f"Temperature data coverage: {available_days} out of {total_days} days ({coverage_pct:.1f}%)")
    
    # Save to CSV
    output_file = os.path.join(output_dir, "soweto_temperature_data.csv")
    df.to_csv(output_file, index=False)
    print(f"Temperature data saved to {output_file}")
    
    return df

if __name__ == "__main__":
    # Define COVID-19 pandemic period in South Africa
    start_date = "2020-03-01"  # First COVID-19 case in South Africa
    end_date = "2021-12-31"    # Covering major pandemic waves
    
    print(f"Collecting temperature data for Soweto during COVID-19 pandemic period: {start_date} to {end_date}")
    
    # Fetch temperature data
    temp_data = fetch_soweto_temperature(start_date, end_date)
    
    # Calculate and print summary statistics
    if not temp_data.empty:
        print("\nTemperature Summary Statistics:")
        print(f"Mean temperature: {temp_data['mean_temp_c'].mean():.1f}°C")
        print(f"Maximum temperature: {temp_data['max_temp_c'].max():.1f}°C")
        print(f"Minimum temperature: {temp_data['min_temp_c'].min():.1f}°C")
        
        # Count extreme heat days (days above 30°C)
        extreme_heat_days = (temp_data['max_temp_c'] > 30).sum()
        total_days = len(temp_data)
        print(f"Extreme heat days (max temp > 30°C): {extreme_heat_days} days ({extreme_heat_days/total_days*100:.1f}%)")
        
        print("\nData collection complete!")
    else:
        print("No temperature data was collected. Please check for errors.")
