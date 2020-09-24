# weather_processing

## Overview
This R script allows the user to process weather data from the NOAA data service.  The NOAA data can be found [here](https://www.ncdc.noaa.gov/cdo-web/).  The end-use of this script is to develop useful datas for creating forecasts which utilize weather-based variables. 

The hourly data is taken from the FM-15 report.  Our main data points of interest are:
* Temperature
* Dewpoint
* Relative Humidity
* Precipitation
* Atmospheric Pressure
* Sky Conditions
* Wind Direction
* Wind Speed

## Dependencies
This script was developed in the RStudio environment and will require the following libraries to be installed:
* stats
* data.table
* lubridate
* readxl
* stringr
* car
* sandwich
* timeSeries
* tseries
* urca
* tidyr

## Status
This script is in the early stages of development.  The following key improvements are still required:
* Ensure that the data is complete for the period of investigation.  This assurance includes:
  * No missing hours within the date-time period. 
  * When data for an hour is missing, fill-in the missing data
* Remove/clean outliers
