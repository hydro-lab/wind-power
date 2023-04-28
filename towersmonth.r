#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

#towers data
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

# ytower <- pivot_wider(x10, names_from = "measurement", values_from = "values")

ytower <- x10 %>% 
     mutate(time_utc = mdy_hm(TIMESTAMP), 
            unix_utc = as.numeric(time_utc), 
            time_et = with_tz(time_utc, tz = "US/Eastern"), 
            utc_offset = (as.numeric(force_tz(time_et, tz = "UTC")) - unix_utc)/3600 ) %>%
     select(-TIMESTAMP)


atower <- ytower %>%
     select(unix_utc, time_et, WS_ms)

ptower <- read_csv("https://duq.box.com/shared/static/oicvh5p4dmv2a8sj1tz9qg8wwuneukwo.csv")
ptower <- rename(ptower, c(speed=`m/s`, power=Watts)) 
#do linear interpolation of power curve
#plot(p$`m/s`, p$Watts)

for (i in 1:nrow(atower)) {
     if (is.na(atower$WS_ms[i])) {
          atower$power[i] <- NA
     } else if (atower$WS_ms[i] < ptower$speed[1]) {
          atower$power[i] <- 0
     } else if (atower$WS_ms[i] >= ptower$speed[nrow(ptower)]) {
          atower$power[i] <- ptower$power[nrow(ptower)]
     } else {
          for (j in 2:nrow(ptower)) {
               if ( (ptower$speed[j-1] <= atower$WS_ms[i]) & (atower$WS_ms[i] < ptower$speed[j]) ) {
                    mtower <- (ptower$power[j] - ptower$power[j-1])/(ptower$speed[j] - ptower$speed[j-1])
                    atower$power[i] <- mtower * (atower$WS_ms[i] - ptower$speed[j-1]) + ptower$power[j-1]
               }
          }
     }
}

#power to energy
energytower <- atower %>%
     mutate(y=year(time_et)) %>%
     mutate(m=month(time_et)) %>%
     mutate(ym=100*y+m) %>%
     mutate(e=power*15*60) %>% #energy in Joules (Watts/ times seconds), 15 minute interval
     group_by(ym) %>%
     summarize(energy=sum(e)) %>%
     mutate(y=floor(ym/100)) %>%
     mutate(m=ym)

#Creating a Weibull distribution using our Towers data. This produced the mean and the standard deviation
ztower <- eweibull(ytower$WS_ms, method = "mle") # https://search.r-project.org/CRAN/refmans/EnvStats/html/eweibull.html

# Weibull distribution parameterized by our data
m <- (c(1:100)) / 10  #creating a range of numbers to run through the distribution (from 0.1 to 10)
shape <- as.numeric(ztower$parameters[1]) #shape factor from weibull method
scal <- as.numeric(ztower$parameters[2]) #scale factor from weibull method
n <- (shape/scal) * (m/scal)^(shape-1) * exp(-((m/scal)^shape)) #Weibull equation
wei <- data.frame(m,n) #getting equation into dataframe

#histogram of wind speed
h <- hist(ytower$WS_ms)
pos <- h$mids
dns <- h$density
dat <- data.frame(pos,dns) #getting histogram into dataframe so we can plot

geo_mean <- scal* gamma((shape+1)/shape) #geometric mean using gamma function of weibull dist

ggplot() + #graphing histogram idealized dist and idealized geometric mean
     geom_col(data=dat, aes(x=pos,y=dns)) +
     #geom_line(data=wei, aes(x=m,y=n)) + 
     geom_vline(xintercept = geo_mean) +
     xlab("Wind Speed (m/s)") + 
     ylab("Frequency") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))


#breaking up by month
#finding mean and standard deviation
monthly.wind.towers <- ytower %>% 
     mutate(y=year(time_et), m=month(time_et), ym=y*100+m) %>%
     group_by(ym) %>%
     summarise(ws_mean=mean(WS_ms), ws_sd=sd(WS_ms), ws_mean_sd=mean(WS_ms_Std))

daily.wind <- y %>% 
     mutate(d=as_date(time_et)) %>%
     group_by(d) %>%
     summarise(ws_mean=mean(WS_ms), ws_mean_sd=mean(WS_ms_Std))



#mean of all wind speed standard deviations
ws_sd_means=mean(ytower$WS_ms_Std)



#both skip the first 4 rows of data as they are actually the titles of the variables
#x1 <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat", skip = 4, col_names = FALSE)
#x2 <- read_csv("/Users/davidkahler/Documents/Wind_Turbines/mellon_MellonRoof.dat.backup", skip = 4, col_names = FALSE)

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
x4 <- read_csv("https://duq.box.com/shared/static/lynyjfvbqqaa0r8v7egfilwr4ugnn4yk.dat", skip = 4, col_names = FALSE)
x4 <- x4 %>%
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

#combining four datasets
x <- rbind(x1,x2,x3,x4)
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

a <- y %>%
     select(unix_utc, time_et, WS_ms_Avg)
     
p <- read_csv("https://duq.box.com/shared/static/oicvh5p4dmv2a8sj1tz9qg8wwuneukwo.csv")
p <- rename(p, c(speed=`m/s`, power=Watts)) # Didn't like the names...
#do linear interpolation of power curve
#plot(p$`m/s`, p$Watts)

for (i in 1:nrow(a)) {
     if (is.na(a$WS_ms_Avg[i])) {
          a$power[i] <- NA
     } else if (a$WS_ms_Avg[i] < p$speed[1]) {
          a$power[i] <- 0
     } else if (a$WS_ms_Avg[i] >= p$speed[nrow(p)]) {
          a$power[i] <- p$power[nrow(p)]
     } else {
          for (j in 2:nrow(p)) {
               if ( (p$speed[j-1] <= a$WS_ms_Avg[i]) & (a$WS_ms_Avg[i] < p$speed[j]) ) {
                    m <- (p$power[j] - p$power[j-1])/(p$speed[j] - p$speed[j-1])
                    a$power[i] <- m * (a$WS_ms_Avg[i] - p$speed[j-1]) + p$power[j-1]
               }
          }
     }
}

#power to energy
energy <- a %>%
     mutate(y=year(time_et)) %>%
     mutate(m=month(time_et)) %>%
     mutate(ym=100*y+m) %>%
     mutate(e=power*15*60) %>% #energy in Joules (Watts/ times seconds), 15 minute interval
     group_by(ym) %>%
     summarize(energy=sum(e)) %>%
     mutate(y=floor(ym/100)) %>%
     mutate(m=ym)
     

#Creating a Weibull distribution using our Mellon data. This produced the mean and the standard deviation
z <- eweibull(y$WS_ms_Avg, method = "mle") # https://search.r-project.org/CRAN/refmans/EnvStats/html/eweibull.html

# Weibull distribution parameterized by our data
m <- (c(1:100)) / 10  #creating a range of numbers to run through the distribution (from 0.1 to 10)
shape <- as.numeric(z$parameters[1]) #shape factor from weibull method
scal <- as.numeric(z$parameters[2]) #scale factor from weibull method
n <- (shape/scal) * (m/scal)^(shape-1) * exp(-((m/scal)^shape)) #Weibull equation
wei <- data.frame(m,n) #getting equation into dataframe

#histogram of wind speed
h <- hist(y$WS_ms_Avg)
pos <- h$mids
dns <- h$density
dat <- data.frame(pos,dns) #getting histogram into dataframe so we can plot

geo_mean <- scal* gamma((shape+1)/shape) #geometric mean using gamma function of weibull dist

ggplot() + #graphing histogram idealized dist and idealized geometric mean
     geom_col(data=dat, aes(x=pos,y=dns)) +
     #geom_line(data=wei, aes(x=m,y=n)) + 
     geom_vline(xintercept = geo_mean) +
     xlab("Wind Speed (m/s)") + 
     ylab("Frequency") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))


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
#no november data?
november21.wind<- y %>%
     filter(time_et>=ymd_hms("2021-11-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2021-11-30 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

december21.wind<- y %>%
     filter(time_et>=ymd_hms("2021-12-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2021-12-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

january22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-01-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-01-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

february22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-02-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-02-28 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

march22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-03-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-03-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

april22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-04-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-04-30 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

may22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-05-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-05-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#No data
june22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-06-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-06-30 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#No data
july22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-07-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-07-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#No data
august22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-08-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-08-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#No data
september22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-09-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-09-30 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#No data
october22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-10-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-10-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

#failed to parse?
november22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-11-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-11-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

december22.wind<- y %>%
     filter(time_et>=ymd_hms("2022-12-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-12-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

january23.wind<- y %>%
     filter(time_et>=ymd_hms("2023-01-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2022-01-31 23:45:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))

february23.wind<- y %>%
     filter(time_et>=ymd_hms("2023-02-01 00:00:00")) %>%
     filter(time_et<=ymd_hms("2023-02-16 13:00:00")) %>%
     summarise(ws_mean=mean(WS_ms_Avg), ws_sd=sd(WS_ms_Avg))







h$mids

# built-in histogram function.  broke up wind speed averages into bins
h = hist(y$WS_ms_Avg,
         breaks=seq(0,12,by=0.1),
         plot=TRUE) 

