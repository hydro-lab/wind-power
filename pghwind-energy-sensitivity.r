library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

# Convert parameters to common names, units
# Date and time   dt    POSIXct
# Precipitation   prcp  mm
# Wind speed      wspd  m/s

# Cathedral of Learning
cath <- read_csv("https://duq.box.com/shared/static/9dryp6f7zdqxnd5hevx01d2j5sxuj0yt.csv", col_types = "cnnnnnnnnc")
cath <- cath %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane (Degrees)`) %>%
      mutate(dt = mdy_hm(cath$Timestamp, tz="US/Eastern")) %>% # Stores time with time zone in lubridate function (POSIXct)
      select(dt, wspd, gust, rhum, prcp, temp, wdir, baro) %>%
      mutate(dt = with_tz(dt,"UTC"))
cath$wspd <- cath$wspd * 0.44704
cath$gust <- cath$gust * 0.44704
cath$baro <- cath$baro * 25.4
cath$prcp <- cath$prcp * 25.4
cath$temp <- (cath$temp - 32) * 0.5556

a.cath <- cath %>%
     select(dt, wspd)

p.cath <- read_csv("https://duq.box.com/shared/static/oicvh5p4dmv2a8sj1tz9qg8wwuneukwo.csv") # POWER CURVE
p.cath <- rename(p.cath, c(speed=`m/s`, power=Watts)) 
#do linear interpolation of power curve
#plot(p$`m/s`, p$Watts)
a.cath$power <- NA # Preallocation of column

for (i in 1:nrow(a.cath)) {
     if (is.na(a.cath$wspd[i])) {
          a.cath$power[i] <- NA
     } else if (a.cath$wspd[i] < p.cath$speed[1]) {
          a.cath$power[i] <- 0
     } else if (a.cath$wspd[i] >= p.cath$speed[nrow(p.cath)]) {
          a.cath$power[i] <- p.cath$power[nrow(p.cath)]
     } else {
          for (j in 2:nrow(p.cath)) {
               if ( (p.cath$speed[j-1] <= a.cath$wspd[i]) & (a.cath$wspd[i] < p.cath$speed[j]) ) {
                    m.cath <- (p.cath$power[j] - p.cath$power[j-1])/(p.cath$speed[j] - p.cath$speed[j-1])
                    a.cath$power[i] <- m.cath * (a.cath$wspd[i] - p.cath$speed[j-1]) + p.cath$power[j-1]
               }
          }
     }
}

#power to energy
energy.cath <- a.cath %>%
     mutate(y=year(dt)) %>%
     mutate(m=month(dt)) %>%
     mutate(ym=100*y+m) %>%
     mutate(e=power*60*60) %>% #energy in Joules (Watts/ times seconds), 60 minute interval
     group_by(ym) %>%
     summarize(energy=sum(e)) %>%
     mutate(y=floor(ym/100)) %>%
     mutate(m=ym)
