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
  
  na_list = matrix(0L, length(data_columns), 2)
  col2chk <- as.vector(paste0(rep("weather.clean.initial[,",length(data_columns)),data_columns,"]",sep=""))
  
  for (i in 1:length(data_columns)){
    
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
                              PRECIP.DEPTH = as.double(substr(weather.prelim.hourly[,AA1],4,7))/10,
                              PRECIP.CONDITION = as.double(substr(weather.prelim.hourly[,AA1],9,9)),
                              PRECIP.QUALITY = as.double(substr(weather.prelim.hourly[,AA1],11,11)),
                              SKY.COVER = as.double(str_extract(weather.prelim.hourly[,GA1],"^.{2}")),
                              SKY.COVER.QUALITY = as.double(substr(weather.prelim.hourly[,GA1],4,4)),
                              SKY.COVER.HEIGHT = as.double(substr(weather.prelim.hourly[,GA1],6,11)),
                              SKY.COVER.HEIGHT.QUALITY = as.double(substr(weather.prelim.hourly[,GA1],13,13)),
                              SKY.COVER.CLOUD.TYPE = as.double(substr(weather.prelim.hourly[,GA1],15,16)),
                              SKY.COVER.CLOUD.TYPE.QUALITY = as.double(substr(weather.prelim.hourly[,GA1],18,18)),
                              ATMOS.PRESS.ALTIMETER = as.double(str_extract(weather.prelim.hourly[,MA1],"^.{5}")),
                              ATMOS.PRESS.ALTIMETER.QUALITY = as.double(substr(weather.prelim.hourly[,MA1],7,7)),
                              ATMOS.PRESS.STATION = as.double(substr(weather.prelim.hourly[,MA1],9,13)),
                              ATMOS.PRESS.STATION.QUALITY = as.double(substr(weather.prelim.hourly[,MA1],15,15)),
                              WIND.DIRECTION = as.double(str_extract(weather.prelim.hourly[,WND],"^.{3}")),
                              WIND.DIRECTION.QUALITY = as.double(substr(weather.prelim.hourly[,WND],5,5)),
                              WIND.TYPE = substr(weather.prelim.hourly[,WND],7,7),
                              WIND.SPEED = as.double(substr(weather.prelim.hourly[,WND],9,12)),
                              WIND.SPEED.QUALITY = as.double(substr(weather.prelim.hourly[,WND],14,14)))]

#CHECK data columns for missing data.  You have to specify which columns to check
data_columns <- c("TEMP_CELC","DEWP_CELC","RH","PRECIP.PERIOD.HOURS","PRECIP.DEPTH")

columns.2b.adj <- col_check_na("weather.clean.initial",data_columns)

for (i in 1: nrow(columns.2b.adj)){
  
  weather.clean.initial[ , eval(data_columns[i]) := na.locf(get(data_columns[i]))]
  
}

any(is.na(weather.clean.initial[,PRECIP.DEPTH]))
any(is.na(weather.clean.initial[,PRECIP.PERIOD.HOURS]))

weather.clean.initial[, c("LIQ.PRECIP.PRD","LIQ.PRECIP.DEPTH","LIQ.PRECIP.CONDITION","LIQ.PRECIP.QUALITY") :=
                        str_split(weather.prelim.hourly[,AA1],",")]


