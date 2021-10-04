library(readr)
library(dplyr)
library(lubridate)

# Convert parameters to common names, units
# Date and time   dt    POSIXct
# Precipitation   prcp  mm
# Wind speed      wspd  m/s

# Cathedral of Learning
cath <- read_csv("https://duq.box.com/shared/static/9dryp6f7zdqxnd5hevx01d2j5sxuj0yt.csv")
cath <- cath %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      mutate(dt = mdy_hm(cath$Timestamp, tz="US/Eastern")) # Stores time with time zone in lubridate function (POSIXct)
cath$wspd <- cath$wspd * 0.44704

# Heinz Field
henz <- read_csv("https://duq.box.com/shared/static/hg7mr85uqnqwoqlrgcd37x3ce7n8qpdm.csv")
henz <- henz %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      mutate(dt = mdy_hm(henz$Timestamp, tz="US/Eastern"))
