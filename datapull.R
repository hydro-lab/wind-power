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
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane (Degrees)`) %>%
      mutate(dt = mdy_hm(cath$Timestamp, tz="US/Eastern")) # Stores time with time zone in lubridate function (POSIXct)
cath$wspd <- cath$wspd * 0.44704
cath$gust <- cath$gust * 0.44704
cath$baro <- cath$baro * 25.4
cath$prcp <- cath$prcp * 25.4
cath$temp <- cath$temp - 32 * 0.5556

# Heinz Field
henz <- read_csv("https://duq.box.com/shared/static/hg7mr85uqnqwoqlrgcd37x3ce7n8qpdm.csv")
henz <- henz %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane  (Degrees)`) %>%
      mutate(dt = mdy_hm(henz$Timestamp, tz="US/Eastern"))
henz$wspd <- henz$wspd * 0.44704
henz$gust <- henz$gust * 0.44704
henz$baro <- henz$baro * 25.4
henz$prcp <- henz$prcp * 25.4
henz$temp <- henz$temp - 32 * 0.5556

#Falk School
falk <- read_csv("https://duq.box.com/shared/static/uhwwur6mv9kipo6tdxa7h265x5ss1x36.csv")
falk <- falk %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane (Degrees)`) %>%
      mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))
falk$wspd <- falk$wspd * 0.44704
falk$gust <- falk$gust * 0.44704
falk$baro <- falk$baro * 25.4
falk$prcp <- falk$prcp * 25.4
falk$temp <- falk$temp - 32 * 0.5556


#Environmental Charter School
char <- read_csv("https://duq.box.com/shared/static/q2wptl6izaeu1f4piz9gglxk0k50qfwr.csv")
char <- char %>%
  rename(wspd=`Anemometer (Miles Per Hour)`) %>%
  rename(gust=`10 Minute Wind Gust (Miles Per Hour)`) %>%
  rename(baro=`Barometer (Inches of Hg)`) %>%
  rename(rhum=`Hygrometer (% Humidity)`) %>%
  rename(prcp=`Rain Gauge (Inches)`) %>%
  rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
  rename(temp=`Thermometer (Fahrenheit)`) %>%
  rename(wdir=`Wind Vane (Degrees)`) %>%
  mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))
char$wspd <- char$wspd * 0.44704
char$gust <- char$gust * 0.44704
char$baro <- char$baro * 25.4
char$prcp <- char$prcp * 25.4
char$temp <- char$temp - 32 * 0.5556


#Penn State
penn <- read_csv("https://duq.box.com/shared/static/m88w4uhb5nx5uctq7t4wfmzd3lq8v20r.csv")
penn <- penn %>%
  rename(wspd=`Anemometer (MPH)`) %>%
  rename(gust=`10 Minute Wind Gust (MPH)`) %>%
  rename(baro=`Barometer (Inches of Hg)`) %>%
  rename(rhum=`Hygrometer (% Humidity)`) %>%
  rename(prcp=`Rain Gauge (Inches)`) %>%
  rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
  rename(temp=`Thermometer (Fahrenheit)`) %>%
  rename(wdir=`Wind Vane (Degrees)`) %>%
  mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))
penn$wspd <- penn$wspd * 0.44704
penn$gust <- penn$gust * 0.44704
penn$baro <- penn$baro * 25.4
penn$prcp <- penn$prcp * 25.4
penn$temp <- penn$temp - 32 * 0.5556


#Pittsburgh International Airport
#pita <- read_csv("https://duq.box.com/shared/static/1rdzbgmp6kti0prpturzzzym7k161c37.csv")
pita <- read_csv("https://duq.box.com/shared/static/g2h3c34irmotlh2n1wz4dttjs8tgonwl.csv", col_types = "cnnnnnnnnnnnccccc")
pita <- pita%>%
  rename(wspd=`Average Wind Speed (mph)`) %>%
  rename(gust=`Max Wind Gust (mph)`) %>%
  rename(rhum=`Average Relative Humidity (%)`) %>%
  rename(prcp=`1 Hour Precip (in)`) %>%
  rename(temp=`Average Temp (F)`) %>%
  rename(wdir=`Average Wind Direction (deg)`) %>%
  rename(baro=`Average Station Pressure (mb)`) %>%
  mutate(dt = mdy_hm(`Date/Time (GMT)`, tz="GMT"))%>%
  select(dt, wspd, gust, rhum, prcp, temp, wdir, baro)
pita$wspd <- pita$wspd * 0.44704
pita$gust <- pita$gust * 0.44704
pita$prcp <- pita$prcp * 25.4
pita$temp <- pita$temp - 32 * 0.5556
pita$baro <- pita$baro * 0.750062

#Allegheny County Airport
alla <- read_csv("https://duq.box.com/shared/static/irlevykc8efzckg91zxt4319kevjypcb.csv")
alla <- read_csv("https://duq.box.com/shared/static/irlevykc8efzckg91zxt4319kevjypcb.csv", col_types = "cnnnnnnnnnnnccccc")
alla <- alla%>%
  rename(wspd=`Average Wind Speed (mph)`) %>%
  rename(gust=`Max Wind Gust (mph)`) %>%
  rename(rhum=`Average Relative Humidity (%)`) %>%
  rename(prcp=`1 Hour Precip (in)`) %>%
  rename(temp=`Average Temp (F)`) %>%
  rename(wdir=`Average Wind Direction (deg)`) %>%
  rename(baro=`Average Station Pressure (mb)`) %>%
  mutate(dt = mdy_hm(`Date/Time (GMT)`, tz="GMT"))%>%
  select(dt, wspd, gust, rhum, prcp, temp, wdir, baro)
alla$wspd <- alla$wspd * 0.44704
alla$gust <- alla$gust * 0.44704
alla$prcp <- alla$prcp * 25.4
alla$temp <- alla$temp - 32 * 0.5556
alla$baro <- alla$baro * 0.750062

#Liberty 1 - Allegheny County Health Dept
lib1 <- read_csv("https://duq.box.com/shared/static/pton8ikhj7ylsaqkyrja9cupa73bkw61.csv")
lib1 <- read_csv("https://duq.box.com/shared/static/pton8ikhj7ylsaqkyrja9cupa73bkw61.csv", col_types = "cnnnnnnnnnn")
lib1 <- lib1%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms('prc', tz="US/Eastern"))
lib1$wspd <- lib1$wspd * 0.44704
lib1$prcp <- lib1$prcp * 25.4


#Lawrenceville - Allegheny County Health Dept
lawr <- read_csv("https://duq.box.com/shared/static/5ryuf0dr2uf0w3q1dmchpyueikmwgacm.csv")
lawr <- read_csv("https://duq.box.com/shared/static/5ryuf0dr2uf0w3q1dmchpyueikmwgacm.csv", col_types = "cnnnnnnnnnn")
lawr <- lawr%>%
  rename(wspd=`sonicws`) %>%
  rename(rhum=`out_rh`) %>%
  rename(prcp=`rainfall`) %>%
  rename(temp=`out_t`) %>%
  rename(wdir=`sonicwd`) %>%
  rename(baro=`bp`) %>%
  rename(srrd=`solarrad`) %>%
  mutate(dt = ymd_hms('dt', tz="US/Eastern"))
lawr$wspd <- lawr$wspd * 0.44704
lawr$prcp <- lawr$prcp * 25.4


#North Braddock - Allegheny County Health Dept
nobr <- read_csv("https://duq.box.com/shared/static/a2o963bjz8djd1jiq79rd3xquqkhczbh.csv")
nobr <- read_csv("https://duq.box.com/shared/static/a2o963bjz8djd1jiq79rd3xquqkhczbh.csv", col_types = "cnnnnnnnnnn")
nobr <- nobr%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms('dt', tz="US/Eastern"))
nobr$wspd <- nobr$wspd * 0.44704
nobr$prcp <- nobr$prcp * 25.4

#Parkway East 1 - Allegheny County Health Dept
pea1 <- read_csv("https://duq.box.com/shared/static/esvm9lszv6n2d97hx740f8za4bg2ppgy.csv")
pea1 <- read_csv("https://duq.box.com/shared/static/esvm9lszv6n2d97hx740f8za4bg2ppgy.csv", col_types = "cnnnnnnnnnn")
pea1 <- pea1%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms('dt', tz="US/Eastern"))
pea1$wspd <- pea1$wspd * 0.44704
pea1$prcp <- pea1$prcp * 25.4


#Parkway East 2 - Allegheny County Health Dept
pea2 <- read_csv("https://duq.box.com/shared/static/16vz69uenlm7hpapjvzep6dpahh7luip.csv")
pea2 <- read_csv("https://duq.box.com/shared/static/16vz69uenlm7hpapjvzep6dpahh7luip.csv", col_types = "cnnnnnnnnnn")
pea2 <- pea2%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms('dt', tz="US/Eastern"))
pea2$wspd <- pea2$wspd * 0.44704
pea2$prcp <- pea2$prcp * 25.4
