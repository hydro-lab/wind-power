#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)
library(curl)

#reading in data from weather station
#x1 is from May 2022 and includes relative humidity information
#x2 is from December and does not include relative humidity information
#both skip the first 4 rows of data as they are actually the titles of the variables
#x1 <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat", skip = 4, col_names = FALSE)
#x2 <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat.backup", skip = 4, col_names = FALSE)
x1 <- read_csv("https://duq.box.com/shared/static/hmths6ofu40ox2n9shvrdydawd57e7v2.dat", skip = 4, col_names = FALSE) 
x2 <- read_csv("https://duq.box.com/shared/static/hf1av8h6a3zny2eb9wcjwkfijs6zul8y.dat", skip = 4, col_names = FALSE) 

#adding 4 columns to dataset missing rel humidity so the column numbers match and we can put the datasets together (~November 2021)
x2$X20 <- NA
x2$X21 <- NA
x2$X22 <- NA
x2$X23 <- NA
#combining two datasets
x <- rbind(x2,x1)

#renaming columns then removing things we do not need (RECORD, -WS_ms_S_WVT, -AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) then changing to correct time format for us and Cuahsi
y <- x %>% 
     rename(TIMESTAMP = X1, # date and time
            RECORD = X2, # sequential record number
            BattV_Avg = X3, # battery voltage, average of 15 minute
            AirTC_Avg = X4, # air temperature, degrees C
            AirTC_Max = X5, # maximum temperature over period
            AirTC_TMx = X6, # time of maximum temperature
            AirTC_Min = X7, # minimum temperature over period
            AirTC_TMn = X8, # time of minimum temperature
            AirTC_Std = X9, # standard deviation of temperature over period: 1 Hz over 15 minutes
            WS_ms_Avg = X10, # wind speed in m/s
            WS_ms_Max = X11, # maximum wind speed over period
            WS_ms_TMx = X12, # time of maximum wind speed
            WS_ms_Min = X13, # minimum wind speed
            WS_ms_TMn = X14, # time of minimum wind speed
            WS_ms_Std = X15, # standard deviation of wind speed over period
            WS_ms_S_WVT = X16, # don't know; appears to be same as average
            WindDir_D1_WVT = X17, # wind direction
            WindDir_SD1_WVT = X18, # standard deviation of wind direction
            Rain_mm_Tot = X19, # incremental rainfall, mm
            RHpct_Max = X20, # maximum relative humidity
            RHpct_TMx = X21, # time of maximum relative humidity
            RHpct_Min = X22, # minimum relative humidity
            RHpct_TMn = X23) %>% # time of minimum relative humidity
     select(-RECORD, -WS_ms_S_WVT, -AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) %>% 
     mutate(time_utc = ymd_hms(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600 ) %>%
     select(-TIMESTAMP)

# Fix the wind vane orientation (it was originally off by 180 degrees)
#We are either adding 180 degrees or subtracting 180 degrees
#Also removing all zero datapoints as there are about 3 weeks of data in which RM Young was stuck on zero
y$WindDir <- NA
for (i in 1:nrow(y)) {
     if (y$WindDir_D1_WVT[i] < 180) {
          if (y$WindDir_D1_WVT[i] == 0) {
               y$WindDir[i] <- NA
          } else {
               y$WindDir[i] <- y$WindDir_D1_WVT[i] + 180
          }
     } else {
          y$WindDir[i] <- y$WindDir_D1_WVT[i] - 180
     }
}

#breaking up by month
#finding mean and standard deviation
monthly.wind <- y %>% 
     mutate(y=year(time_et), m=month(time_et), ym=y*100+m) %>%
     group_by(ym) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg), ws_mean_sd=mean(WS_ms_Std))

daily.wind <- y %>% 
     mutate(d=as_date(time_et)) %>%
     group_by(d) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_mean_sd=mean(WS_ms_Std))


#mean of all wind speed standard deviations
ws_sd_means=mean(y$WS_ms_Std)

#mean and standard devs of wind speed each month 
december.wind<- y %>%
     filter(time_et>ymd_hms("2021-12-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2021-12-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

january.wind<- y %>%
     filter(time_et>ymd_hms("2022-01-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2022-01-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

february.wind<- y %>%
     filter(time_et>ymd_hms("2022-02-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2022-02-28 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

march.wind<- y %>%
     filter(time_et>ymd_hms("2022-03-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2022-03-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

april.wind<- y %>%
     filter(time_et>ymd_hms("2022-04-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2022-04-30 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

may.wind<- y %>%
     filter(time_et>ymd_hms("2022-05-01 00:00:00")) %>%
     filter(time_et>ymd_hms("2022-05-17 10:30:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

h$mids

h = hist(y$WS_ms_Avg, # built-in histogram function.  To find values only.  Plotting is at the end of this loop.
         breaks=seq(0,12,by=0.1),
         plot=TRUE) 
