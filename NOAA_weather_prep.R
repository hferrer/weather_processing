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

#DEFINE FUNCTION TO CHECK FOR NAs

col_check_na <- function(data.table.name,data_columns){

  #print(column)
  #print(length(column))
  
  na_list = matrix(0L, length(column), 2)
  col2chk <- as.vector(paste0(rep("weather.clean.initial[,",length(data_columns)),data_columns,"]",sep=""))
  
  for (i in 1:length(column)){
    
    #print(column[i])
    #print(paste0(eval(data.table.name)))
    
    #output <- eval(parse(text=paste0(eval(data.table.name),"[which(is.na(",column[i],"))]",sep="")))
   output <- eval(parse(text=paste0("which(is.na(",col2chk[i],"))",sep="")))
    
    #print(output)
    
    if (sum(output) > 0){
      na_list[i,1] <- (data_columns[i])
      na_list[i,2] <- 1
      #na_list[i,3] <- grep(pattern = c(paste(data.table.name,"[,",sep = ""),"]"),column[i])
    } else {
      na_list[i,1] <- (col2chk[i])
      na_list[i,2] <- 0
      #na_list[i,3] <- 0
      
    }

  }
    return(na_list)
}



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

#CHECK data columns for missing data.  You have to specify which columns to check
data_columns <- c("TEMP_CELC","DEWP_CELC","RH","PRECIP.PERIOD.HOURS","PRECIP.DEPTH")

columns.2b.adj <- col_check_na("weather.clean.initial",data_columns)

for (i in 1: nrow(columns.2b.adj)){
  
  weather.clean.initial[ , eval(data_columns[i]) := na.locf(get(data_columns[i]))]
  
}

any(is.na(weather.clean.initial[,PRECIP.DEPTH]))
any(is.na(weather.clean.initial[,PRECIP.PERIOD.HOURS]))
