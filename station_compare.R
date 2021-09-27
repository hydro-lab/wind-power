# Read in and visualize weather station data

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

pit <- read_csv("https://duq.box.com/shared/static/sfyi486t9mqio179il6w7bxyla3liicn.csv")
pit <- pit %>%
      mutate(dt = mdy_hm(`Date/Time (GMT)`), 
             atmp = 5*(`Average Temp (F)`-32)/9, 
             maxt = 5*(`Max Temp (F)`-32)/9, 
             mint = 5*(`Min Temp (F)`-32)/9, 
             prcp = 25.4*`1 Hour Precip (in)`, 
             gust = 0.44704*`Max Wind Gust (mph)`, 
             awsp = 0.44704*`Average Wind Speed (mph)`, 
             mwsp = 0.44704*`Max Wind Speed (mph)`) %>%
      rename(relh = `Average Relative Humidity (%)`, 
             pres = `Average Station Pressure (mb)`, 
             wdir = `Average Wind Direction (deg)`) %>%
      select(dt, atmp, maxt, mint, prcp, gust, awsp, mwsp, pres, wdir)

hfd <- read_csv("https://duq.box.com/shared/static/95ervz58nrc0od4h9jd7mwfpmjpsyq7d.csv")
hfd <- hfd %>%
      mutate(dt = mdy_hm(Site), 
             gust = 0.44704*`10 Minute Wind Gust (Miles per hour)`, 
             awsp = 0.44704*`Anemometer (Miles per Hour)`, 
             atmp = 5*(`Thermometer (Fahrenheit)`-32)/9) %>%
      rename(relh = `Hygrometer (% Humidity)`, 
             wdir = `Wind Vane (Degrees)`, 
             pres = `Barometer (Inches of Hg)`) %>%
      select(dt, atmp, gust, awsp, relh, wdir, pres)

pit$hours <- as.numeric(pit$dt)/3600 # gives hours since 1970-01-01 00:00:00
hfd$hours <- as.numeric(hfd$dt)/3600

start <- max(c(min(pit$hours),min(hfd$hours)))
end <- min(c(max(pit$hours),max(hfd$hours)))
rg <- 1+(as.numeric(end)-as.numeric(start))
dt <- c(start:end)
pit_speed <- array(NA, dim = rg)
hfd_speed <- array(NA, dim = rg)
datum <- start-1

for (i in 1:nrow(pit)) {
      if ((pit$hours[i] >= start) & (pit$hours[i] <= end)) {
            pit_speed[(pit$hours[i]-datum)] <- pit$awsp[i]
      }
}

for (i in 1:nrow(hfd)) {
      if ((hfd$hours[i] >= start) & (hfd$hours[i] <= end)) {
            hfd_speed[(hfd$hours[i]-datum)] <- hfd$awsp[i]
      }
}

comp <- data.frame(dt, pit_speed, hfd_speed)

