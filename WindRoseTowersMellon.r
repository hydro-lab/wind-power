#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

#towers
x10 <- read_csv("https://duq.box.com/shared/static/7otcf32r2nlf8hy28zc1vcwrl6eqa7q2.csv", skip = 2, col_names = FALSE)

#renaming columns
x10 <- x10 %>%
     rename(TIMESTAMP = X1, # date and time
            Solar_Rad = X2, #solar radiation, w/m2
            Rain_mm_Tot = X3, # incremental rainfall, mm
            lightning = X4, #lightning activity
            light_dist = X5, #lightning distance, km
            WindDir_D1_WVT = X6, # wind direction
            WS_ms = X7, # wind speed in m/s
            WS_ms_Gust = X8, #wind gust in m/s
            AirTC = X9, # air temperature, degrees C
            RHpct = X10, #vapor pressure, kPa
            AtmoPressure = X11, #atmospheric pressure, kPa
            AxisX = X12, #degrees x-axis level
            AxisY = X13, #degrees y-axis level
            Rain_mm_Max = X14 , #max. precipitation rate, mm/hr
            RH_temp = X15, # RH sensor temperature, degrees C
            Batt_Percent = X16, # battery Percent
            BattV_Avg = X17) %>% #battery voltage, mV
     select(-lightning, -light_dist, -AxisY, -AxisX, -BattV_Avg, -Batt_Percent, -RH_temp, Rain_mm_Max) # %>%
#     pivot_longer(c(Solar_Rad, Rain_mm_Tot, WindDir_D1_WVT, WS_ms, WS_ms_Gust, AirTC, AtmoPressure, RHpct), names_to="measurement",values_to="values")

x11 <- read_csv("https://duq.box.com/shared/static/qfqrx92gablv9sj6ucohr04ot0fpoq9b.csv", skip = 2, col_names = FALSE)

#renaming columns
x11 <- x11 %>%
     rename(TIMESTAMP = X1, # date and time
            Solar_Rad = X2, #solar radiation, w/m2
            Rain_mm_Tot = X3, # incremental rainfall, mm
            lightning = X4, #lightning activity
            light_dist = X5, #lightning distance, km
            WindDir_D1_WVT = X6, # wind direction
            WS_ms = X7, # wind speed in m/s
            WS_ms_Gust = X8, #wind gust in m/s
            AirTC = X9, # air temperature, degrees C
            RHpct = X10, #vapor pressure, kPa
            AtmoPressure = X11, #atmospheric pressure, kPa
            AxisX = X12, #degrees x-axis level
            AxisY = X13, #degrees y-axis level
            Rain_mm_Max = X14 , #max. precipitation rate, mm/hr
            RH_temp = X15, # RH sensor temperature, degrees C
            Batt_Percent = X16, # battery Percent
            BattV_Avg = X17) %>% #battery voltage, mV
     select(-lightning, -light_dist, -AxisY, -AxisX, -BattV_Avg, -Batt_Percent, -RH_temp, Rain_mm_Max) 

# ytower <- pivot_wider(x10, names_from = "measurement", values_from = "values")

#combining  datasets
xtower <- rbind(x10,x11)
#ytower <- pivot_wider(xtower, names_from = "measurement", values_from = "values")

ytower <- xtower %>% 
     mutate(time_utc = mdy_hm(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600 ) %>%
     select(-TIMESTAMP)


atower <- ytower %>%
     select(unix_utc, time_et, WS_ms)

# ## NEW METHOD - with speed binning
# ## sort data:
     d <- ytower$WindDir_D1_WVT
     s <- ytower$WS_ms
speed.bins <- 8 # this used for monthly data at HFP
# speed.bins <- ceiling(max(s)) # HFP will work with this binning.
# speed.bins <- 6 # PIT needs this binning
wind <- array(0, dim = c(36,speed.bins))
for (i in 1:(length(s))) {
      speed.index <- ceiling(s[i]) # HFP will work with this binning.
      if (speed.index >8) {
           speed.index <- 8
      }

      
      wind[ceiling(d[i]/10),speed.index] <- wind[ceiling(d[i]/10),speed.index] + 1
}
## Now, form long array rather than wide:
wind.long <- array(NA, dim = 36*speed.bins)
for (i in 1:speed.bins) {
      for (j in 1:36) {
            wind.long[(36*(i-1))+j] <- wind[j,i]
      }
}
# speeds <- c(rep("0-2",36), rep("2-4",36), rep("4-6",36), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36), rep("7+",36)) # for HFP
directions <- rep(5+10*(c(0:35)), speed.bins)
rose <- data.frame(directions, speeds, wind.long)

windTowers <- ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
      labs(caption = paste("Towers Hall")) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_brewer("Speed (m/s)", palette = "Blues") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export


#reading in data from weather station

#x1 is from November 2021 to December 2021 - missing relative humidity data
#"TIMESTAMP","RECORD","BattV_Avg","AirTC_Avg","AirTC_Max","AirTC_TMx","AirTC_Min","AirTC_TMn","AirTC_Std","WS_ms_Avg","WS_ms_Max","WS_ms_TMx","WS_ms_Min","WS_ms_TMn","WS_ms_Std","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","Rain_mm_Tot"
x1 <- read_csv("https://duq.box.com/shared/static/ylm116m8qs37l9byeuom3auunmmorzu4.backup", skip = 4, col_names = FALSE)
x1 <- x1 %>%
     mutate(X20=as.numeric(NA))%>%
     mutate(X21 = as_datetime(0)) %>%
     mutate(X22=as.numeric(NA),X23 = as_datetime(0)) %>%
     
     #adding 4 columns to dataset missing rel humidity so the column numbers match and we can put the datasets together (~November 2021)
     #x2$X20 <- NA
     #x2$X21 <- NA
     #x2$X22 <- NA
     #x2$X23 <- NA
     #combining two datasets
     #x <- rbind(x2,x1)
     
     #renaming columns then removing things we do not need (RECORD, -WS_ms_S_WVT, -AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) then changing to correct time format for us and Cuahsi
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
     select(-AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) %>%
     pivot_longer(c(BattV_Avg, AirTC_Avg, AirTC_Max, AirTC_Min, AirTC_Std, WS_ms_Avg, WS_ms_Max, WS_ms_Min, WS_ms_Std, WS_ms_S_WVT, WindDir_D1_WVT, WindDir_SD1_WVT, Rain_mm_Tot, RHpct_Max, RHpct_Min), names_to="measurement",values_to="values")

#x2 <- december 2021 to june 2022
#"TIMESTAMP","RECORD","BattV_Avg","AirTC_Avg","AirTC_Max","AirTC_TMx","AirTC_Min","AirTC_TMn","AirTC_Std","WS_ms_Avg","WS_ms_Max","WS_ms_TMx","WS_ms_Min","WS_ms_TMn","WS_ms_Std","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","Rain_mm_Tot","RH_Max","RH_TMx","RH_Min","RH_TMn"
x2 <- read_csv("https://duq.box.com/shared/static/89e22w7bek5t7kusga7a12p4ty4i607k.dat", skip = 4, col_names = FALSE)
x2 <- x2 %>%
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
     select(-AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) %>%
     pivot_longer(c(BattV_Avg, AirTC_Avg, AirTC_Max, AirTC_Min, AirTC_Std, WS_ms_Avg, WS_ms_Max, WS_ms_Min, WS_ms_Std, WS_ms_S_WVT, WindDir_D1_WVT, WindDir_SD1_WVT, Rain_mm_Tot, RHpct_Max, RHpct_Min), names_to="measurement",values_to="values")

#x3 <- june 2022 to feb 2023
#"TIMESTAMP","RECORD","BattV_Avg","AirTC_Avg","AirTC_Max","AirTC_TMx","AirTC_Min","AirTC_TMn","AirTC_Std","RH_Max","RH_TMx","RH_Min","RH_TMn","WS_ms_Avg","WS_ms_Max","WS_ms_TMx","WS_ms_Min","WS_ms_TMn","WS_ms_Std","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","Rain_mm_Tot"
x3 <- read_csv("https://duq.box.com/shared/static/lynyjfvbqqaa0r8v7egfilwr4ugnn4yk.dat", skip = 4, col_names = FALSE)
x3 <- x3 %>%
     rename(TIMESTAMP = X1, # date and time
            RECORD = X2, # sequential record number
            BattV_Avg = X3, # battery voltage, average of 15 minute
            AirTC_Avg = X4, # air temperature, degrees C
            AirTC_Max = X5, # maximum temperature over period
            AirTC_TMx = X6, # time of maximum temperature
            AirTC_Min = X7, # minimum temperature over period
            AirTC_TMn = X8, # time of minimum temperature
            AirTC_Std = X9, # standard deviation of temperature over period: 1 Hz over 15 minutes
            WS_ms_Avg = X14, # wind speed in m/s
            WS_ms_Max = X15, # maximum wind speed over period
            WS_ms_TMx = X16, # time of maximum wind speed
            WS_ms_Min = X17, # minimum wind speed
            WS_ms_TMn = X18, # time of minimum wind speed
            WS_ms_Std = X19, # standard deviation of wind speed over period
            WS_ms_S_WVT = X20, # don't know; appears to be same as average
            WindDir_D1_WVT = X21, # wind direction
            WindDir_SD1_WVT = X22, # standard deviation of wind direction
            Rain_mm_Tot = X23, # incremental rainfall, mm
            RHpct_Max = X10, # maximum relative humidity
            RHpct_TMx = X11, # time of maximum relative humidity
            RHpct_Min = X12, # minimum relative humidity
            RHpct_TMn = X13) %>% # time of minimum relative humidity
     select(-AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) %>%
     pivot_longer(c(BattV_Avg, AirTC_Avg, AirTC_Max, AirTC_Min, AirTC_Std, WS_ms_Avg, WS_ms_Max, WS_ms_Min, WS_ms_Std, WS_ms_S_WVT, WindDir_D1_WVT, WindDir_SD1_WVT, Rain_mm_Tot, RHpct_Max, RHpct_Min), names_to="measurement",values_to="values")

#June 21, 2022 to April 27, 2023
#"TIMESTAMP","RECORD","BattV_Avg","AirTC_Avg","AirTC_Max","AirTC_TMx","AirTC_Min","AirTC_TMn","AirTC_Std","RH_Max","RH_TMx","RH_Min","RH_TMn","WS_ms_Avg","WS_ms_Max","WS_ms_TMx","WS_ms_Min","WS_ms_TMn","WS_ms_Std","WS_ms_S_WVT","WindDir_D1_WVT","WindDir_SD1_WVT","Rain_mm_Tot"
#x4 <- read_csv("https://duq.box.com/shared/static/lynyjfvbqqaa0r8v7egfilwr4ugnn4yk.dat", skip = 4, col_names = FALSE)
# temp <- x4 %>%
#      rename(TIMESTAMP = X1, # date and time
#             RECORD = X2, # sequential record number
#             BattV_Avg = X3, # battery voltage, average of 15 minute
#             AirTC_Avg = X4, # air temperature, degrees C
#             AirTC_Max = X5, # maximum temperature over period
#             AirTC_TMx = X6, # time of maximum temperature
#             AirTC_Min = X7, # minimum temperature over period
#             AirTC_TMn = X8, # time of minimum temperature
#             AirTC_Std = X9, # standard deviation of temperature over period: 1 Hz over 15 minutes
#             WS_ms_Avg = X14, # wind speed in m/s
#             WS_ms_Max = X15, # maximum wind speed over period
#             WS_ms_TMx = X16, # time of maximum wind speed
#             WS_ms_Min = X17, # minimum wind speed
#             WS_ms_TMn = X18, # time of minimum wind speed
#             WS_ms_Std = X19, # standard deviation of wind speed over period
#             WS_ms_S_WVT = X20, # don't know; appears to be same as average
#             WindDir_D1_WVT = X21, # wind direction
#             WindDir_SD1_WVT = X22, # standard deviation of wind direction
#             Rain_mm_Tot = X23, # incremental rainfall, mm
#             RHpct_Max = X10, # maximum relative humidity
#             RHpct_TMx = X11, # time of maximum relative humidity
#             RHpct_Min = X12, # minimum relative humidity
#             RHpct_TMn = X13) %>% # time of minimum relative humidity
#      select(-AirTC_TMx, -AirTC_TMn, -WS_ms_TMx, -WS_ms_TMn, -RHpct_TMx, -RHpct_TMn) %>%
#      pivot_longer(c(BattV_Avg, AirTC_Avg, AirTC_Max, AirTC_Min, AirTC_Std, WS_ms_Avg, WS_ms_Max, WS_ms_Min, WS_ms_Std, WS_ms_S_WVT, WindDir_D1_WVT, WindDir_SD1_WVT, Rain_mm_Tot, RHpct_Max, RHpct_Min), names_to="measurement",values_to="values")

#combining four datasets
x <- rbind(x1,x2,x3)
y <- pivot_wider(x, names_from = "measurement", values_from = "values")

y <- y %>% 
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
     if (is.na(y$WindDir_D1_WVT[i]) == FALSE) {
          if (y$WindDir_D1_WVT[i] < 180) {
               if (y$WindDir_D1_WVT[i] == 0) {
                    y$WindDir[i] <- NA
                    y$WS_ms_Avg[i] <- NA
               } else {
                    y$WindDir[i] <- y$WindDir_D1_WVT[i] + 180
               }
          } else {
               y$WindDir[i] <- y$WindDir_D1_WVT[i] - 180
          }
     }
}

a <- y %>%
     select(unix_utc, time_et, WS_ms_Avg)

# ## NEW METHOD - with speed binning - Mellon Hall
# ## sort data:
d <- y$WindDir_D1_WVT
s <- y$WS_ms_Avg
speed.bins <- 8 # this used for monthly data at HFP
# speed.bins <- ceiling(max(s)) # HFP will work with this binning.
# speed.bins <- 6 # PIT needs this binning
wind <- array(0, dim = c(36,speed.bins))
for (i in 1:(length(s))) {
     speed.index <- ceiling(s[i]) # HFP will work with this binning.
     if (speed.index >8) {
          speed.index <- 8
     }
     
     
     wind[ceiling(d[i]/10),speed.index] <- wind[ceiling(d[i]/10),speed.index] + 1
}
## Now, form long array rather than wide:
wind.long <- array(NA, dim = 36*speed.bins)
for (i in 1:speed.bins) {
     for (j in 1:36) {
          wind.long[(36*(i-1))+j] <- wind[j,i]
     }
}
# speeds <- c(rep("0-2",36), rep("2-4",36), rep("4-6",36), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36), rep("7+",36)) # for HFP
directions <- rep(5+10*(c(0:35)), speed.bins)
rose <- data.frame(directions, speeds, wind.long)

windMellon <- ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
     labs(caption = paste("Towers Hall")) +
     geom_bar(position="stack", stat="identity") +
     scale_fill_brewer("Speed (m/s)", palette = "Blues") +
     coord_polar(theta = "x", start = 0) +
     scale_x_continuous(breaks = seq(0, 360, 45)) +
     theme_linedraw() +
     theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export
