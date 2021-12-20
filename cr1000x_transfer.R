# This code is to read in the data file from the Mellon Hall weather station, process, 
# error check, and write a CSV for publication to CUAHSI.
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)

x <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat", skip = 4, col_names = FALSE)
t <- read_csv("download_record.csv", col_names = FALSE) # all in UTC

# station installed on:
#install <- as.numeric(ymd_hms("2021-11-12T20:30:00")) # this is time in UTC
# previous download got to:
first_record <- t$X3[nrow(t)] + 1 # moves to one second after last download, must rewrite
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
            WS_ms_S_WVT = X16, # don't know; appears to be same as average
            WindDir_D1_WVT = X17, # wind direction
            WindDir_SD1_WVT = X18, # standard deviation of wind direction
            Rain_mm_Tot = X19, # incremental rainfall, mm
            RHpct_Max = X20, # maximum relative humidity
            RHpct_TMx = X21, # time of maximum relative humidity
            RHpct_Min = X22, # minimum relative humidity
            RHpct_TMn = X23) %>% # time of minimum relative humidity
     select(-RECORD, -WS_ms_S_WVT, -AirTC_TMx, -AirTC_TMn, -RHpct_TMx, -RHpct_TMn, -WS_ms_TMx, -WS_ms_TMn) %>% 
     mutate(time_utc = ymd_hms(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600, 
            BattV_Avg_qc = "", 
            AirTC_Avg_qc = "", 
            AirTC_Max_qc = "", 
            AirTC_Min_qc = "", 
            AirTC_Std_qc = "", 
            RHpct_Min_qc = "", 
            RHpct_Max_qc = "", 
            WS_ms_Avg_qc = "", 
            WS_ms_Max_qc = "", 
            WS_ms_Min_qc = "", 
            WS_ms_Std_qc = "", 
            WindDir_D1_WVT_qc = "", 
            WindDir_SD1_WVT_qc = "", 
            Rain_mm_Tot_qc = "") %>% 
     select(-TIMESTAMP) %>% 
     filter(unix_utc >= first_record) # first_record or install date

first_record <- x$unix_utc[1] # use this in subsequent downloads, not if the post-install must be redone.

# For export to CUAHSI:
for (i in 1:nrow(x)) {
     if (x$BattV_Avg[i] >= 10) {
          x$BattV_Avg_qc[i] <- "n" # "n" battery stable
     } else {
          x$BattV_Avg_qc[i] <- "b" # "b" battery fault
          x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "b,")
          x$AirTC_Max_qc[i] <- paste0(x$AirTC_Max_qc[i], "b,")
          x$AirTC_Min_qc[i] <- paste0(x$AirTC_Min_qc[i], "b,")
          x$AirTC_Std_qc[i] <- paste0(x$AirTC_Std_qc[i], "b,")
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "b,")
          x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "b,")
          x$WS_ms_Avg_qc[i] <- paste0(x$WS_ms_Avg_qc[i], "b,")
          x$WS_ms_Max_qc[i] <- paste0(x$WS_ms_Max_qc[i], "b,")
          x$WS_ms_Min_qc[i] <- paste0(x$WS_ms_Min_qc[i], "b,")
          x$WS_ms_Std_qc[i] <- paste0(x$WS_ms_Std_qc[i], "b,")
          x$WindDir_D1_WVT_qc[i] <- paste0(x$WindDir_D1_WVT_qc[i], "b,")
          x$WindDir_SD1_WVT_qc[i] <- paste0(x$WindDir_SD1_WVT_qc[i], "b,")
          x$Rain_mm_Tot_qc[i] <- paste0(x$Rain_mm_Tot_qc[i], "b,")
     }
     if (is.na(x$AirTC_Avg[i])) {
          x$AirTC_Avg[i] <- -9999
          x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "m,") #
     } else {
          if (x$AirTC_Avg[i] < -40) {
               x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "l,") # "l" temperature below valid range
          } else if (x$AirTC_Avg[i] > 70) {
               x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "h,") # "h" temperature above valid range
          }
          if ((x$AirTC_Avg[i] <= x$AirTC_Max[i]) & (x$AirTC_Avg[i] >= x$AirTC_Min[i])) { # If average is outside the range [min,max]
               x$AirTC_Avg_qc[i] <- x$AirTC_Avg_qc[i]
          } else {
               x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "i,") # "i" instrument error
          }
          if (i > 4) {
               if ( (x$AirTC_Avg[i] < (mean(x$AirTC_Avg[(i-4):(i-1)]) - 3 * ( max(x$AirTC_Avg[(i-4):(i-1)]) - min(x$AirTC_Avg[(i-4):(i-1)]) ) )) | (x$AirTC_Avg[i] > (mean(x$AirTC_Avg[(i-4):(i-1)]) + 3 * ( max(x$AirTC_Avg[(i-4):(i-1)]) - min(x$AirTC_Avg[(i-4):(i-1)]) ) )) ) {
                    x$AirTC_Avg_qc[i] <- paste0(x$AirTC_Avg_qc[i], "s,") # "s" spike detected
               }
          }
     }
     if (is.na(x$AirTC_Max[i])) {
          x$AirTC_Max[i] <- -9999
          x$AirTC_Max_qc[i] <- paste0(x$AirTC_Max_qc[i], "m,") #
     } else {
          if (x$AirTC_Max[i] < -40) {
               x$AirTC_Max_qc[i] <- paste0(x$AirTC_Max_qc[i], "l,")
          } else if (x$AirTC_Max[i] > 70) {
               x$AirTC_Max_qc[i] <- paste0(x$AirTC_Max_qc[i], "h,")
          }
     }
     if (is.na(x$AirTC_Min[i])) {
          x$AirTC_Min[i] <- -9999
          x$AirTC_Min_qc[i] <- paste0(x$AirTC_Min_qc[i], "m,") #
     } else {
          if (x$AirTC_Min[i] < -40) {
               x$AirTC_Min_qc[i] <- paste0(x$AirTC_Min_qc[i], "l,")
          } else if (x$AirTC_Min[i] > 70) {
               x$AirTC_Min_qc[i] <- paste0(x$AirTC_Min_qc[i], "h,")
          }
     }
     if (is.na(x$AirTC_Std[i])) {
          x$AirTC_Std[i] <- -9999
          x$AirTC_Std_qc[i] <- paste0(x$AirTC_Std_qc[i], "m,") #
     }
     if (is.na(x$RHpct_Min[i])) {
          x$RHpct_Min[i] <- -9999
          x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "m,") #
     } else {
          if (x$RHpct_Min[i] < 0) {
               x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "l,")
          }
          if (x$RHpct_Min[i] > 100) {
               x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "h,")
          }
          if (i > 4) {
               if ( (x$RHpct_Min[i] < (mean(x$RHpct_Min[(i-4):(i-1)]) - 3 * ( max(x$RHpct_Min[(i-4):(i-1)]) - min(x$RHpct_Min[(i-4):(i-1)]) ) )) | (x$RHpct_Min[i] > (mean(x$RHpct_Min[(i-4):(i-1)]) + 3 * ( max(x$RHpct_Min[(i-4):(i-1)]) - min(x$RHpct_Min[(i-4):(i-1)]) ) )) ) {
                    x$RHpct_Min_qc[i] <- paste0(x$RHpct_Min_qc[i], "s,") # "s" spike detected
               }
          }
     }
     if (is.na(x$RHpct_Max[i])) {
          x$RHpct_Max[i] <- -9999
          x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "m,") #
     } else {
          if (x$RHpct_Max[i] < 0) {
               x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "l,")
          }
          if (x$RHpct_Max[i] > 100) {
               x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "h,")
          }
          if (i > 4) {
               if ( (x$RHpct_Max[i] < (mean(x$RHpct_Max[(i-4):(i-1)]) - 3 * ( max(x$RHpct_Max[(i-4):(i-1)]) - min(x$RHpct_Max[(i-4):(i-1)]) ) )) | (x$RHpct_Max[i] > (mean(x$RHpct_Max[(i-4):(i-1)]) + 3 * ( max(x$RHpct_Max[(i-4):(i-1)]) - min(x$RHpct_Max[(i-4):(i-1)]) ) )) ) {
                    x$RHpct_Max_qc[i] <- paste0(x$RHpct_Max_qc[i], "s,") # "s" spike detected
               }
          }
     }
     if (is.na(x$WS_ms_Avg[i])) {
          x$WS_ms_Avg[i] <- -9999
          x$WS_ms_Avg_qc[i] <- paste0(x$WS_ms_Avg_qc[i], "m,") #
     } else {
          if (x$WS_ms_Avg[i] < 0) {
               x$WS_ms_Avg_qc[i] <- paste0(x$WS_ms_Avg_qc[i], "l,")
          }
          if (x$WS_ms_Avg[i] > 100) {
               x$WS_ms_Avg_qc[i] <- paste0(x$WS_ms_Avg_qc[i], "h,")
          }
     }
     if (is.na(x$WS_ms_Min[i])) {
          x$WS_ms_Min[i] <- -9999
          x$WS_ms_Min_qc[i] <- paste0(x$WS_ms_Min_qc[i], "m,") #
     } else {
          if (x$WS_ms_Min[i] < 0) {
               x$WS_ms_Min_qc[i] <- paste0(x$WS_ms_Min_qc[i], "l,")
          }
          if (x$WS_ms_Min[i] > 100) {
               x$WS_ms_Min_qc[i] <- paste0(x$WS_ms_Min_qc[i], "h,")
          }
     }
     if (is.na(x$WS_ms_Max[i])) {
          x$WS_ms_Max[i] <- -9999
          x$WS_ms_Max_qc[i] <- paste0(x$WS_ms_Max_qc[i], "m,") #
     } else {
          if (x$WS_ms_Max[i] < 0) {
               x$WS_ms_Max_qc[i] <- paste0(x$WS_ms_Max_qc[i], "l,")
          }
          if (x$WS_ms_Min[i] > 100) {
               x$WS_ms_Max_qc[i] <- paste0(x$WS_ms_Max_qc[i], "h,")
          }
     }
     if (is.na(x$WindDir_D1_WVT[i])) {
          x$WindDir_D1_WVT[i] <- -9999
          x$WindDir_D1_WVT_qc[i] <- paste0(x$WindDir_D1_WVT_qc[i], "m,") #
     } else {
          if (x$WindDir_D1_WVT[i] < 0) {
               x$x$WindDir_D1_WVT_qc[i] <- paste0(x$WindDir_D1_WVT_qc[i], "l,")
          }
          if (x$WindDir_D1_WVT[i] > 360) {
               x$WindDir_D1_WVT_qc[i] <- paste0(x$WindDir_D1_WVT_qc[i], "h,")
          }
     }
     # If there haven't been any problems so far, assign each "a, normal operation"
     if (x$AirTC_Avg_qc[i] == "") {x$AirTC_Avg_qc[i] <- "a"}
     if (x$AirTC_Max_qc[i] == "") {x$AirTC_Max_qc[i] <- "a"}
     if (x$AirTC_Min_qc[i] == "") {x$AirTC_Min_qc[i] <- "a"}
     if (x$AirTC_Std_qc[i] == "") {x$AirTC_Std_qc[i] <- "a"}
     if (x$RHpct_Min_qc[i] == "") {x$RHpct_Min_qc[i] <- "a"}
     if (x$RHpct_Max_qc[i] == "") {x$RHpct_Max_qc[i] <- "a"}
     if (x$WS_ms_Avg_qc[i] == "") {x$WS_ms_Avg_qc[i] <- "a"}
     if (x$WS_ms_Max_qc[i] == "") {x$WS_ms_Max_qc[i] <- "a"}
     if (x$WS_ms_Min_qc[i] == "") {x$WS_ms_Min_qc[i] <- "a"}
     if (x$WS_ms_Std_qc[i] == "") {x$WS_ms_Std_qc[i] <- "a"}
     if (x$WindDir_D1_WVT_qc[i] == "") {x$WindDir_D1_WVT_qc[i] <- "a"}
     if (x$WindDir_SD1_WVT_qc[i] == "") {x$WindDir_SD1_WVT_qc[i] <- "a"}
     if (x$Rain_mm_Tot_qc[i] == "") {x$Rain_mm_Tot_qc[i] <- "a"}
}

y <- x[,c(15,16,17,18,1,19,2,20,4,22,3,21,5,23,14,24,13,25,6,26,8,28,7,27,9,29,10,30,11,31,12,32)]
write_csv(y, "/Users/davidkahler/Documents/Wind_Turbines/mellon_table.csv", append = TRUE)

# Long format for CUAHSI upload
y$site <- "DuqMellon"
y$source <- "Duq-CERE"

z <- pivot_longer(y, cols = c(BattV_Avg,AirTC_Avg,AirTC_Min,AirTC_Max,AirTC_Std,RHpct_Min,RHpct_Max,WS_ms_Avg,WS_ms_Min,WS_ms_Max,WS_ms_Std,WindDir_D1_WVT,WindDir_SD1_WVT,Rain_mm_Tot),
                  names_to = "Variable",
                  values_to = "Value")

batt <- z %>% 
     filter(Variable=="BattV_Avg") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,BattV_Avg_qc) %>% 
     rename(qc = BattV_Avg_qc) %>% 
     mutate(method = "Voltmeter")
air_avg <- z %>% 
     filter(Variable=="AirTC_Avg") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,AirTC_Avg_qc) %>% 
     rename(qc = AirTC_Avg_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
air_min <- z %>% 
     filter(Variable=="AirTC_Min") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,AirTC_Min_qc) %>% 
     rename(qc = AirTC_Min_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
air_max <- z %>% 
     filter(Variable=="AirTC_Max") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,AirTC_Max_qc) %>% 
     rename(qc = AirTC_Max_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
air_std <- z %>% 
     filter(Variable=="AirTC_Std") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,AirTC_Std_qc) %>% 
     rename(qc = AirTC_Std_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
rh_min <- z %>% 
     filter(Variable=="RHpct_Min") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,RHpct_Min_qc) %>% 
     rename(qc = RHpct_Min_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
rh_max <- z %>% 
     filter(Variable=="RHpct_Max") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,RHpct_Max_qc) %>% 
     rename(qc = RHpct_Max_qc) %>% 
     mutate(method = "Thermometer_hygrometer")
ws_avg <- z %>% 
     filter(Variable=="WS_ms_Avg") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WS_ms_Avg_qc) %>% 
     rename(qc = WS_ms_Avg_qc) %>% 
     mutate(method = "Wind")
ws_min <- z %>% 
     filter(Variable=="WS_ms_Min") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WS_ms_Min_qc) %>% 
     rename(qc = WS_ms_Min_qc) %>% 
     mutate(method = "Wind")
ws_max <- z %>% 
     filter(Variable=="WS_ms_Max") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WS_ms_Max_qc) %>% 
     rename(qc = WS_ms_Max_qc) %>% 
     mutate(method = "Wind")
ws_std <- z %>% 
     filter(Variable=="WS_ms_Std") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WS_ms_Std_qc) %>% 
     rename(qc = WS_ms_Std_qc) %>% 
     mutate(method = "Wind")
wdir <- z %>% 
     filter(Variable=="WindDir_D1_WVT") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WindDir_D1_WVT_qc) %>% 
     rename(qc = WindDir_D1_WVT_qc) %>% 
     mutate(method = "Wind")
wdir_sd <- z %>% 
     filter(Variable=="WindDir_SD1_WVT") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,WindDir_SD1_WVT_qc) %>% 
     rename(qc = WindDir_SD1_WVT_qc) %>% 
     mutate(method = "Wind")
rain <- z %>% 
     filter(Variable=="Rain_mm_Tot") %>% 
     select(unix_utc,Value,time_et,utc_offset,time_utc,site,Variable,source,Rain_mm_Tot_qc) %>% 
     rename(qc = Rain_mm_Tot_qc) %>% 
     mutate(method = "Rain")
export <- rbind(batt,air_avg,air_min,air_max,air_std,rh_min,rh_max,ws_avg,ws_min,ws_max,ws_std,wdir,wdir_sd,rain)
rm(batt,air_avg,air_min,air_max,air_std,rh_min,rh_max,ws_avg,ws_min,ws_max,ws_std,wdir,wdir_sd,rain)
export <- export[order(export$unix_utc),]

write_csv(export, "/Users/davidkahler/Documents/Wind_Turbines/mellon_longer.csv", append = TRUE)

today <- Sys.Date()
last_record <- x$unix_utc[nrow(x)]
r <- data.frame(today,first_record,last_record)
write_csv(r, "download_record.csv", append = TRUE)

# For export to hydro-lab.github.io, daily data and plots
