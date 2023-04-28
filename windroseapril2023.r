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
