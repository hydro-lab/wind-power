# This code is to read in the data file from the Mellon Hall weather station, process, 
# error check, and write a CSV for publication to CUAHSI.
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)

x <- read_csv("mellon_MellonRoof.dat", skip = 4, col_names = FALSE)

# station installed on:
install <- as.numeric(ymd_hms("2021-11-12T20:30:00"))
# sort and tidy data. 
x <- x %>% 
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
             WS_ms_S_WVT = X16, # don't know
             WindDir_D1_WVT = X17, # wind direction
             WindDir_SD1_WVT = X18, # standard deviation of wind direction
             Rain_mm_Tot = X19) %>% # incremental rainfall, mm
      mutate(unix_utc = as.numeric(ymd_hms(TIMESTAMP)), 
             BattV_Avg_qc = NA, 
             AirTC_Avg_qc = NA, 
             AirTC_Max_qc = NA, 
             AirTC_TMx_qc = NA, 
             AirTC_Min_qc = NA, 
             AirTC_TMn_qc = NA, 
             AirTC_Std_qc = NA, 
             WS_ms_Avg_qc = NA, 
             WS_ms_Max_qc = NA, 
             WS_ms_TMx_qc = NA, 
             WS_ms_Min_qc = NA, 
             WS_ms_TMn_qc = NA, 
             WS_ms_Std_qc = NA, 
             WS_ms_S_WVT_qc = NA, 
             WindDir_D1_WVT_qc = NA, 
             WindDir_SD1_WVT_qc = NA, 
             Rain_mm_Tot_qc = NA) %>% 
      filter(unix_utc >= install)

# For export to CUAHSI:
for (i in 1:nrow(x)) {
      if (x$BattV_Avg[i] < 10) {
            x$BattV_Avg_qc[i] <- "b" # "b" battery fault
      } else {
            x$BattV_Avg_qc[i] <- "a" # "a" battery stable
      }
      if (x$AirTC_Avg[i] <- -40) {
            x$AirTC_Avg_qc[i] <- "lt" # "lt" temperature below valid range
      } else if (x$AirTC_Avg[i] > 70) {
            x$AirTC_Avg_qc[i] <- "ht" # "ht" temperature above valid range
      } else {
            x$AirTC_Avg_qc[i] <- "a"
      }
      if (x$AirTC_Max[i] <- -40) {
            x$AirTC_Max_qc[i] <- "lt" # "lt" temperature below valid range
      } else if (x$AirTC_Max[i] > 70) {
            x$AirTC_Max_qc[i] <- "ht" # "ht" temperature above valid range
      } else {
            x$AirTC_Max_qc[i] <- "a"
      }
      if (x$AirTC_Min[i] <- -40) {
            x$AirTC_Min_qc[i] <- "lt" # "lt" temperature below valid range
      } else if (x$AirTC_Min[i] > 70) {
            x$AirTC_Min_qc[i] <- "ht" # "ht" temperature above valid range
      } else {
            x$AirTC_Min_qc[i] <- "a"
      }
}



# For export to hydro-lab.github.io, daily data and plots
