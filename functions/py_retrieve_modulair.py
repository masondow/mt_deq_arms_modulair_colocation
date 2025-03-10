import dotenv 
import os
import quantaq 
import pandas as pd
from dotenv import load_dotenv
from quantaq import client

# Load environment variables
load_dotenv()

# Retrieve and check API key
API_KEY = os.getenv("QUANTAQ_API_KEY")

if API_KEY is None:
    raise ValueError("API key not found. Please set QUANTAQ_API_KEY in the .env file.")

def authenticate():
    client = quantaq.QuantAQAPIClient(API_KEY)
    return client

def get_sensors(client):
    devices = client.devices.list()
    print (devices)

def download_data(client, serial_number, start, end, interval='1h'):
    data = client.get_data(
        device_id=serial_number,
        start=start,
        end=end,
        interval=interval
    )
    return data

def save_data(data, filename='data/raw/modulair_pm25.csv'):
    df = pd.DataFrame(data)
    df.to_csv(filename, index=False)

