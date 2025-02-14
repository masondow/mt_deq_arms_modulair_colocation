# Script for using QuantAQ's API to retrieve data from the ModulAir device

# needed packages:

library(httr)
library(jsonlite)
library(openssl)
library(keyring)

# set API Key, endpoints:
api_url <- "https://api.quant-aq.com/device-api/v1/"  # this is proper API key
api_key <- "5IZB5K8JE3HWLU0B0QJS9QNM" # Replace with *your* API key (if you have permission, save the key as a system variable or using the keyring package)
endpoint <- "devices/MOD-00514/data-by-date/2023-11-29/" # Set your endpoint HERE
full_url <- paste0(api_url, endpoint)
rossiter_device_serial <- "MOD-00514"

# list of needed notes/endpoint locations for API call:

#ModulAir device SN: MOD-00514
# Account endpoint (useful for testing API): "account"
# Most recent data endpoint: "devices/MOD-0051/data/"
# Data by date endpoint: "devices/<serial-number>/data-by-date/<date>/" 
  # note that the Data by Date format must be YYYY-MM-DD and all dates are in GMT. 
  # thus, if you request data for 2022-01-01, it will return all data for the device on Jan. 1st GMT.
  # ex: "devices/MOD-00514/data-by-date/2023-11-29/"


# API call:

auth_value <- paste("Basic", base64_enc(paste(api_key, ":", sep = "")))
response <- GET(url = full_url, add_headers(Authorization = auth_value))

if (status_code(response) == 200) {
  modulair_data <- fromJSON(content(response, "text")) # adjust this object name for description fit
  View(modulair_data[["data"]]) 
  head(modulair_data[["data"]])
} else {
  print(paste("Failed to fetch data. Status code:", status_code(response)))
}

