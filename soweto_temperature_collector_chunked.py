"""
Soweto Temperature Data Collector (Chunked)

This script collects temperature data for Soweto during the full COVID-19 health data period
using Google Earth Engine's ERA5 Daily dataset, processing in 3-month chunks to avoid API limitations.
"""

import os
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import ee

# Initialize Earth Engine
try:
    ee.Initialize()
    print("Earth Engine initialized successfully")
except Exception as e:
    print(f"Error initializing Earth Engine: {e}")
    print("Please authenticate with 'earthengine authenticate' if needed")
    raise Exception("Earth Engine initialization failed")

def fetch_soweto_temperature_chunk(start_date_str, end_date_str):
    """
    Fetch temperature data for Soweto for a specified date range using ERA5 Daily dataset.
    
    Parameters:
    -----------
    start_date_str : str
        Start date in 'YYYY-MM-DD' format
    end_date_str : str
        End date in 'YYYY-MM-DD' format
    
    Returns:
    --------
    pandas.DataFrame
        DataFrame containing temperature data for Soweto
    """
    print(f"Fetching chunk: {start_date_str} to {end_date_str}")
    
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
        print(f"Chunk coverage: {available_days} out of {total_days} days ({coverage_pct:.1f}%)")
    else:
        print("No data found for this chunk")
    
    return df

def fetch_soweto_temperature_full_period(start_date_str, end_date_str, chunk_days=90, output_dir="temperature_data"):
    """
    Fetch temperature data for the full period by breaking it into smaller chunks
    
    Parameters:
    -----------
    start_date_str : str
        Start date in 'YYYY-MM-DD' format
    end_date_str : str
        End date in 'YYYY-MM-DD' format
    chunk_days : int
        Number of days in each chunk
    output_dir : str
        Directory to save output files
    
    Returns:
    --------
    pandas.DataFrame
        DataFrame containing temperature data for the full period
    """
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    print(f"Fetching temperature data for Soweto from {start_date_str} to {end_date_str}")
    
    # Convert string dates to datetime objects
    start_date = pd.to_datetime(start_date_str)
    end_date = pd.to_datetime(end_date_str)
    
    # Create a list of chunk start and end dates
    chunk_dates = []
    current_date = start_date
    
    while current_date <= end_date:
        chunk_end = min(current_date + timedelta(days=chunk_days-1), end_date)
        chunk_dates.append((current_date.strftime('%Y-%m-%d'), chunk_end.strftime('%Y-%m-%d')))
        current_date = chunk_end + timedelta(days=1)
    
    print(f"Breaking down collection into {len(chunk_dates)} chunks")
    
    # Fetch data for each chunk and combine
    all_data = []
    for i, (chunk_start, chunk_end) in enumerate(chunk_dates):
        print(f"\nProcessing chunk {i+1}/{len(chunk_dates)}")
        chunk_data = fetch_soweto_temperature_chunk(chunk_start, chunk_end)
        if not chunk_data.empty:
            all_data.append(chunk_data)
    
    # Combine all chunks
    if all_data:
        combined_df = pd.concat(all_data, ignore_index=True)
        combined_df = combined_df.sort_values('date')
        
        # Remove duplicates if any
        combined_df = combined_df.drop_duplicates(subset=['date'])
        
        # Calculate overall coverage
        total_days = (end_date - start_date).days + 1
        available_days = combined_df['mean_temp_c'].notna().sum()
        coverage_pct = (available_days / total_days) * 100
        print(f"\nOverall temperature data coverage: {available_days} out of {total_days} days ({coverage_pct:.1f}%)")
        
        # Save to CSV
        output_file = os.path.join(output_dir, "soweto_temperature_data_full.csv")
        combined_df.to_csv(output_file, index=False)
        print(f"Temperature data saved to {output_file}")
        
        return combined_df
    else:
        print("No data was collected for any chunk")
        return pd.DataFrame()

if __name__ == "__main__":
    # Define period matching the health data from ChAdOx dataset
    start_date = "2020-07-01"  # Start of health data period
    end_date = "2022-03-31"    # End of health data period
    
    print(f"Collecting temperature data for Soweto to match health data period: {start_date} to {end_date}")
    
    # Fetch temperature data in chunks
    temp_data = fetch_soweto_temperature_full_period(start_date, end_date, chunk_days=90)
    
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
        
        # Count cold days (days below 10°C)
        cold_days = (temp_data['min_temp_c'] < 10).sum()
        print(f"Cold days (min temp < 10°C): {cold_days} days ({cold_days/total_days*100:.1f}%)")
        
        print("\nData collection complete!")
    else:
        print("No temperature data was collected. Please check for errors.")
