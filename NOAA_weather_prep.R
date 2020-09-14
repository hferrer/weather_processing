library(stats)
library(data.table)
library(lubridate)
library(readxl)
library(magrittr)
library(stringr)
library(car)
library(sandwich)
library(timeSeries)
library(tseries)
library(urca)
library(tidyr)

#Set working directory
setwd("C:/R/Git Repo/projects_github/weather_processing/")

####
#Define data file nane
file.name <- "weather_data_teterboro.csv"

#Upload data
weather.prelim <- fread(file.name, header = TRUE) %>% setDT()

#CoNVERST 'DATE' column into a 'Date' and 'HE' column
weather.prelim[,c("Date","Time") := tstrsplit(DATE,"T")]

#FILTER for hourly data
weather.prelim.hourly <- weather.prelim[REPORT_TYPE == "FM-15" ,]

#BUILD data hourly data
weather.clean.initial <- weather.prelim.hourly[,list(Date,Time)]
weather.clean.initial <- weather.clean.initial[, HE := as.integer(substr(Time, 1,2))+1]

#Convert Station ID and Name
weather.clean.initial[, ":=" (Station.No = weather.prelim.hourly[,STATION],
                      Station.Name =gsub(" .*$","",weather.prelim.hourly[,NAME]),
                      CALL_SIGN = weather.prelim.hourly[,CALL_SIGN])]

#Convert temperature and dewpoint into numerical format
weather.clean.initial[, TEMP_CELC := as.double(str_replace(weather.prelim.hourly[,TMP],",","."))/10 ]
weather.clean.initial[, DEWP_CELC := as.double(str_replace(weather.prelim.hourly[,DEW],",","."))/10 ]

#Impute Relative Humidity from Temperature and Dewpoint
weather.clean.initial[, RH := 100 * (exp((17.625 * DEWP_CELC)/ (243.04 + DEWP_CELC)) / exp((17.625 * TEMP_CELC) / (243.04 + TEMP_CELC))) ]

#Add Precipitation Data
weather.clean.initial[, ":=" (PRECIP.PERIOD.HOURS = as.double(str_extract(weather.prelim.hourly[,AA1],"^.{2}")),
                              PRECIP.DEPTH = as.double(substr(weather.prelim.hourly[,AA1],4,7))/10)]


