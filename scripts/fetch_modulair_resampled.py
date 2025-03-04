"""Retrieve and merge QuantAQ Modulair sensor data via API.

Note: this script is currently non-functioning due to the resampled-data API endpoint being removed. Resampled data endpoint is being changed (hopefully soon) and script will update to macth new endpoint.

This script fetches resampled data from multiple QuantAQ Modulair
devices, merges them into a single dataset, and saves the output
to a CSV file.

The script follows best practices:
- Uses API authentication via environment variables.
- Allows user input for serial numbers, date range, and interval.
- Ensures merged data columns are prefixed with the device serial number.
- Conforms to the Google Python Style Guide.

Example:
    Run the script and provide user input when prompted:
    $ python quantaq_fetch.py

    Sample input:
    Serial numbers: MOD123, MOD456
    Start date: 2024-01-01
    End date: 2024-01-31
    Interval: 1H
"""

import os
import requests
import pandas as pd
from datetime import datetime
from typing import List, Dict, Any
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Constants
BASE_URL = "https://api.quant-aq.com/v1"
# API_KEY = os.getenv("QUANTAQ_API_KEY")
# Alternatively, you can set the API key directly in the script: 
API_KEY ="5IZB5K8JE3HWLU0B0QJS9QNM"

if API_KEY is None:
    raise ValueError("API key not found. Please set QUANTAQ_API_KEY in the .env file.")

def get_resampled_data(serial_number: str, start_date: str, 
                       end_date: str, interval: str) -> List[Dict[str, Any]]:
    """Fetches resampled data from the QuantAQ API.

    Args:
        serial_number: The Modulair sensor serial number.
        start_date: Start date in YYYY-MM-DD format.
        end_date: End date in YYYY-MM-DD format.
        interval: Resampling interval (e.g., '15min', '1H', '1D').

    Returns:
        A list of dictionaries containing the JSON response from the API.
    """
    endpoint = f"/devices/{serial_number}/data/resampled"
    url = f"{BASE_URL}{endpoint}"
    headers = {"x-api-key": API_KEY}
    params = {
        "start": start_date,
        "end": end_date,
        "interval": interval
    }

    response = requests.get(url, headers=headers, params=params)
    
    if response.status_code != 200:
        raise RuntimeError(
            f"Failed to retrieve data for {serial_number}. "
            f"Status Code: {response.status_code}, Response: {response.text}"
        )

    return response.json()


def process_data(data: List[Dict[str, Any]], serial_number: str) -> pd.DataFrame:
    """Processes API JSON data into a pandas DataFrame.

    Args:
        data: List of dictionaries containing sensor data.
        serial_number: The Modulair sensor serial number.

    Returns:
        A pandas DataFrame with timestamp as the index and column names
        formatted as '<parameter>_<serial_number>'.
    """
    if not data:
        return pd.DataFrame()  # Return empty DataFrame if no data

    df = pd.DataFrame(data)
    if 'timestamp' not in df:
        raise KeyError(f"Expected 'timestamp' in API response for {serial_number}.")

    df.set_index('timestamp', inplace=True)
    df.rename(columns=lambda x: f"{x}_{serial_number}", inplace=True)
    return df


def merge_sensor_data(serial_numbers: List[str], start_date: str, 
                      end_date: str, interval: str) -> pd.DataFrame:
    """Fetches and merges data from multiple QuantAQ sensors.

    Args:
        serial_numbers: List of Modulair sensor serial numbers.
        start_date: Start date in YYYY-MM-DD format.
        end_date: End date in YYYY-MM-DD format.
        interval: Resampling interval.

    Returns:
        A merged pandas DataFrame containing data from all sensors.
    """
    merged_df = pd.DataFrame()

    for serial in serial_numbers:
        serial = serial.strip()  # Remove leading/trailing whitespace
        print(f"Fetching data for device {serial}...")

        try:
            data = get_resampled_data(serial, start_date, end_date, interval)
            df = process_data(data, serial)
            
            if df.empty:
                print(f"Warning: No data available for device {serial}.")
                continue

            if merged_df.empty:
                merged_df = df
            else:
                merged_df = merged_df.join(df, how='outer')

        except Exception as error:
            print(f"Error retrieving data for {serial}: {error}")

    return merged_df


def main():
    """Main function to retrieve and merge QuantAQ Modulair data."""
    # Get user inputs
    serial_numbers = input("Enter serial numbers (comma-separated): ").split(',')
    start_date = input("Enter start date (YYYY-MM-DD): ")
    end_date = input("Enter end date (YYYY-MM-DD): ")
    interval = input("Enter resampling interval (e.g., '15min', '1H', '1D'): ")

    # Validate date format
    try:
        datetime.strptime(start_date, "%Y-%m-%d")
        datetime.strptime(end_date, "%Y-%m-%d")
    except ValueError:
        raise ValueError("Incorrect date format. Please use YYYY-MM-DD.")

    # Retrieve and merge data
    merged_data = merge_sensor_data(serial_numbers, start_date, end_date, interval)

    if merged_data.empty:
        print("No data retrieved. Exiting...")
        return

    # Save to CSV
    output_file = "merged_quantAQ_data.csv"
    merged_data.to_csv(output_file)
    print(f"Data successfully saved to {output_file}")


if __name__ == "__main__":
    main()
