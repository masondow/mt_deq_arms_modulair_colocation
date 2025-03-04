import dotenv 
import os
import pandas as pd
import quantaq

def authenticate():
    client = APIClient(api_key=API_KEY)
    return client

def get_sensors(client):
    sensors = client.get_devices()
    return sensors

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

