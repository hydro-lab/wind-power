# This code is to read in the data file from the Mellon Hall weather station, process, 
# error check, and write a CSV for publication to CUAHSI.
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(latex2exp)

x <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat", skip = 4, col_names = FALSE)

# sort and tidy data. 
cr1000x <- x %>% 
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

# Adjust wind direction
for (i in 1:nrow(cr1000x)) {
     if (cr1000x$WindDir_D1_WVT[i] > 180) {
          cr1000x$WindDir_D1_WVT[i] <- cr1000x$WindDir_D1_WVT[i] -180
     } else {
          cr1000x$WindDir_D1_WVT[i] <- cr1000x$WindDir_D1_WVT[i] + 180
     }
}

# For export to hydro-lab.github.io, daily data and plots
this <- week(Sys.Date())
lastweek <- cr1000x %>% 
     pivot_longer(cols = c(BattV_Avg,AirTC_Avg,AirTC_Min,AirTC_Max,AirTC_Std,RHpct_Min,RHpct_Max,WS_ms_Avg,WS_ms_Min,WS_ms_Max,WS_ms_Std,WindDir_D1_WVT,WindDir_SD1_WVT,Rain_mm_Tot),
                  names_to = "Variable",
                  values_to = "Value") %>% 
     mutate(time_et=ymd_hms(time_et)) %>% 
     mutate(w = week(time_et)) %>% 
     mutate(y = year(time_et)) %>% 
     filter(w >= (this-1)) %>% 
     filter(y == year(Sys.Date())) %>% 
     select(time_et,Variable,Value)

out <- lastweek %>% 
     pivot_wider(names_from = Variable, 
                 values_from = "Value") %>% 
     select(time_et,AirTC_Avg,Rain_mm_Tot,WS_ms_Avg,WindDir_D1_WVT) %>% 
     rename(dir=WindDir_D1_WVT) %>% 
     mutate(spd_mph=WS_ms_Avg * 2.23694) %>% 
     mutate(AirTF = 32 + (9 * AirTC_Avg/5)) %>% 
     mutate(prcp_in=Rain_mm_Tot/25.4) %>% 
     select(-WS_ms_Avg, -Rain_mm_Tot, -AirTC_Avg)

write_csv(out,"forcast_prep.csv")
