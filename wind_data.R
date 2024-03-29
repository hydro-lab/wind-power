#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

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



## Wind Rose
#Creating several "buckets" for the wind speed to be in - there are 8 buckets
## sort data:
d <- y$WindDir

s <- y$WS_ms_Avg
speed.bins <- 8 
wind <- array(0, dim = c(36,speed.bins))
for (i in 1:(length(s))) {
      if (s[i] <= 1) {
            speed.index <- 1
      } else if (s[i] <= 2) {
            speed.index <- 2
      } else if (s[i] <= 3) {
            speed.index <- 3
      } else if (s[i] <= 4) {
            speed.index <- 4
      } else if (s[i] <= 5) {
            speed.index <- 5
      } else if (s[i] <= 6) {
           speed.index <- 6
      } else if (s[i] <= 7) {
           speed.index <- 7
      } else {
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
speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36), rep("7 and up",36)) # for HFP
directions <- rep(5+10*(c(0:35)), speed.bins)
rose <- data.frame(directions, speeds, wind.long)

#Creating wind rose
wind_rose <- ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) +
      labs(caption = paste("Mellon Hall")) +
      geom_bar(position="stack", stat="identity") +
      scale_fill_brewer("Speed (m/s)", palette = "Blues") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) # NOTE: ylim used in export

#Removing wind speeds that are zero
y$ws <- NA
for (i in 1:nrow(y)) {
     if (y$WS_ms_Avg[i] == 0) {
          y$ws[i] <- NA
     } else {
          y$ws[i] <- y$WS_ms_Avg[i]
     }
}
#Creating a Weibull distribution using our Mellon data. This produced the mean and the standard deviation
z <- eweibull(y$ws, method = "mle") # https://search.r-project.org/CRAN/refmans/EnvStats/html/eweibull.html

# Weibull distrubution parameterized by our data
m <- (c(1:100)) / 10  #creating a range of numbers to run through the distribution (from 0.1 to 10)
shape <- as.numeric(z$parameters[1]) #shape factor from weibull method
scal <- as.numeric(z$parameters[2]) #scale factor from weibull method
n <- (shape/scal) * (m/scal)^(shape-1) * exp(-((m/scal)^shape)) #Weibull equation
wei <- data.frame(m,n) #getting equation into dataframe

#histogram of wind speed
h <- hist(y$ws)
pos <- h$mids
dns <- h$density
dat <- data.frame(pos,dns) #getting histogram into dataframe so we can plot

geo_mean <- scal* gamma((shape+1)/shape) #geometric mean using gamma function of weibull dist

ggplot() + #graphing histogram idealized dist and idealized geometric mean
     geom_col(data=dat, aes(x=pos,y=dns)) +
     geom_line(data=wei, aes(x=m,y=n)) + 
     geom_vline(xintercept = geo_mean) +
     xlab("Wind Speed (m/s)") + 
     ylab("Frequency") + 
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(legend.position="right") + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

