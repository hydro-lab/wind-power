library(readr)
library(dplyr)
library(lubridate)

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
cath$temp <- cath$temp - 32 * 0.5556

# Heinz Field
henz <- read_csv("https://duq.box.com/shared/static/hg7mr85uqnqwoqlrgcd37x3ce7n8qpdm.csv", col_types = "cnnnnnnnn")
henz <- henz %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane  (Degrees)`) %>%
      mutate(dt = mdy_hm(henz$Timestamp, tz="US/Eastern")) %>%
      mutate(dt = with_tz(dt,"UTC"))
henz$wspd <- henz$wspd * 0.44704
henz$gust <- henz$gust * 0.44704
henz$baro <- henz$baro * 25.4
henz$prcp <- henz$prcp * 25.4
henz$temp <- henz$temp - 32 * 0.5556

#Falk School
falk <- read_csv("https://duq.box.com/shared/static/uhwwur6mv9kipo6tdxa7h265x5ss1x36.csv", col_types = "cnnnnnnnn")
falk <- falk %>%
      rename(wspd=`Anemometer (MPH)`) %>%
      rename(gust=`10 Minute Wind Gust (MPH)`) %>%
      rename(baro=`Barometer (Inches of Hg)`) %>%
      rename(rhum=`Hygrometer (% Humidity)`) %>%
      rename(prcp=`Rain Gauge (Inches)`) %>%
      rename(srrd=`Solar Radiation Sensor (Watts per Square Meter)`) %>%
      rename(temp=`Thermometer (Fahrenheit)`) %>%
      rename(wdir=`Wind Vane (Degrees)`) %>%
      mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))%>%
      mutate(dt = with_tz(dt,"UTC"))
falk$wspd <- falk$wspd * 0.44704
falk$gust <- falk$gust * 0.44704
falk$baro <- falk$baro * 25.4
falk$prcp <- falk$prcp * 25.4
falk$temp <- falk$temp - 32 * 0.5556

#Environmental Charter School
char <- read_csv("https://duq.box.com/shared/static/q2wptl6izaeu1f4piz9gglxk0k50qfwr.csv", col_types = "cnnnnnnnn")
char <- char %>%
  rename(wspd=`Anemometer (Miles Per Hour)`) %>%
  rename(gust=`10 Minute Wind Gust (Miles Per Hour)`) %>%
  rename(baro=`Barometer (Inches of Hg)`) %>%
  rename(rhum=`Hygrometer (% Humidity)`) %>%
  rename(prcp=`Rain Gauge (Inches)`) %>%
  rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
  rename(temp=`Thermometer (Fahrenheit)`) %>%
  rename(wdir=`Wind Vane (Degrees)`) %>%
  mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
char$wspd <- char$wspd * 0.44704
char$gust <- char$gust * 0.44704
char$baro <- char$baro * 25.4
char$prcp <- char$prcp * 25.4
char$temp <- char$temp - 32 * 0.5556


#Penn State
penn <- read_csv("https://duq.box.com/shared/static/m88w4uhb5nx5uctq7t4wfmzd3lq8v20r.csv", col_types = "cnnnnnnnn")
penn <- penn %>%
  rename(wspd=`Anemometer (MPH)`) %>%
  rename(gust=`10 Minute Wind Gust (MPH)`) %>%
  rename(baro=`Barometer (Inches of Hg)`) %>%
  rename(rhum=`Hygrometer (% Humidity)`) %>%
  rename(prcp=`Rain Gauge (Inches)`) %>%
  rename(srrd=`Solar Radiation Sensor (Watts Per Square Meter)`) %>%
  rename(temp=`Thermometer (Fahrenheit)`) %>%
  rename(wdir=`Wind Vane (Degrees)`) %>%
  mutate(dt = mdy_hm(Timestamp, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
penn$wspd <- penn$wspd * 0.44704
penn$gust <- penn$gust * 0.44704
penn$baro <- penn$baro * 25.4
penn$prcp <- penn$prcp * 25.4
penn$temp <- penn$temp - 32 * 0.5556


#Pittsburgh International Airport
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
lib1 <- read_csv("https://duq.box.com/shared/static/pton8ikhj7ylsaqkyrja9cupa73bkw61.csv", col_types = "cnnnnnnnnnn")
lib1 <- lib1%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms(prc, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
lib1$wspd <- lib1$wspd * 0.44704
lib1$prcp <- lib1$prcp * 25.4


#Lawrenceville - Allegheny County Health Dept
lawr <- read_csv("https://duq.box.com/shared/static/5ryuf0dr2uf0w3q1dmchpyueikmwgacm.csv", col_types = "cnnnnnnnnnn")
lawr <- lawr%>%
  rename(wspd=`sonicws`) %>%
  rename(rhum=`out_rh`) %>%
  rename(prcp=`rainfall`) %>%
  rename(temp=`out_t`) %>%
  rename(wdir=`sonicwd`) %>%
  rename(baro=`bp`) %>%
  rename(srrd=`solarrad`) %>%
  mutate(dt = ymd_hms(dt, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
lawr$wspd <- lawr$wspd * 0.44704
lawr$prcp <- lawr$prcp * 25.4


#North Braddock - Allegheny County Health Dept
nobr <- read_csv("https://duq.box.com/shared/static/a2o963bjz8djd1jiq79rd3xquqkhczbh.csv", col_types = "cnnnnnnnnnn")
nobr <- nobr%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms(dt, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
nobr$wspd <- nobr$wspd * 0.44704
nobr$prcp <- nobr$prcp * 25.4

#Parkway East 1 - Allegheny County Health Dept
pea1 <- read_csv("https://duq.box.com/shared/static/esvm9lszv6n2d97hx740f8za4bg2ppgy.csv", col_types = "cnnnnnnnnnn")
pea1 <- pea1%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms(dt, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
pea1$wspd <- pea1$wspd * 0.44704
pea1$prcp <- pea1$prcp * 25.4


#Parkway East 2 - Allegheny County Health Dept
pea2 <- read_csv("https://duq.box.com/shared/static/16vz69uenlm7hpapjvzep6dpahh7luip.csv", col_types = "cnnnnnnnnnn")
pea2 <- pea2%>%
  rename(wspd=`scalar wind speed (mph)`) %>%
  rename(rhum=`rhum (%)`) %>%
  rename(prcp=`prcp (inches)`) %>%
  rename(temp=`temp (celsius)`) %>%
  rename(wdir=`scalar wind direction (degrees)`) %>%
  rename(baro=`baro (mmhg)`) %>%
  rename(srrd=`solarrad (w/m2)`) %>%
  mutate(dt = ymd_hms(dt, tz="US/Eastern"))%>%
  mutate(dt = with_tz(dt,"UTC"))
pea2$wspd <- pea2$wspd * 0.44704
pea2$prcp <- pea2$prcp * 25.4

#Get Dates/Times
Earliest <- c(min(as.numeric(pita$dt)), min(as.numeric(falk$dt)), min(as.numeric(cath$dt)), min(as.numeric(henz$dt)), min(as.numeric(char$dt)), min(as.numeric(penn$dt)), min(as.numeric(alla$dt)), min(as.numeric(lib1$dt), na.rm = TRUE), min(as.numeric(lawr$dt), na.rm = TRUE), min(as.numeric(nobr$dt), na.rm = TRUE), min(as.numeric(pea1$dt), na.rm = TRUE), min(as.numeric(pea2$dt), na.rm = TRUE))

Start <- min(Earliest)

ST_HR <- Start/3600

Last <- max(c(max(as.numeric(pita$dt)), max(as.numeric(falk$dt)), max(as.numeric(cath$dt)), max(as.numeric(henz$dt)), max(as.numeric(char$dt)), max(as.numeric(penn$dt)), max(as.numeric(alla$dt)), max(as.numeric(lib1$dt), na.rm = TRUE), max(as.numeric(lawr$dt), na.rm = TRUE), max(as.numeric(nobr$dt), na.rm = TRUE), max(as.numeric(pea1$dt), na.rm = TRUE), max(as.numeric(pea2$dt), na.rm = TRUE)))

#Find Positions
pita <- mutate(pita, order=(as.numeric(dt)/3600) - ST_HR + 1)
cath <- mutate(cath, order=(as.numeric(dt)/3600) - ST_HR + 1)
henz <- mutate(henz, order=(as.numeric(dt)/3600) - ST_HR + 1)
falk <- mutate(falk, order=(as.numeric(dt)/3600) - ST_HR + 1)
char <- mutate(char, order=(as.numeric(dt)/3600) - ST_HR + 1)
penn <- mutate(penn, order=(as.numeric(dt)/3600) - ST_HR + 1)
alla <- mutate(alla, order=(as.numeric(dt)/3600) - ST_HR + 1)
lib1 <- mutate(lib1, order=(as.numeric(dt)/3600) - ST_HR + 1)
lawr <- mutate(lawr, order=(as.numeric(dt)/3600) - ST_HR + 1)
nobr <- mutate(nobr, order=(as.numeric(dt)/3600) - ST_HR + 1)
pea1 <- mutate(pea1, order=(as.numeric(dt)/3600) - ST_HR + 1)
pea2 <- mutate(pea2, order=(as.numeric(dt)/3600) - ST_HR + 1)

#Build Table (in hours)
len <- (Last - Start)/3600 + 1

pita.sp <- array(NA, dim =len)
cath.sp <- array(NA, dim =len)
henz.sp <- array(NA, dim =len)
falk.sp <- array(NA, dim =len)
char.sp <- array(NA, dim =len)
penn.sp <- array(NA, dim =len)
alla.sp <- array(NA, dim =len)
lib1.sp <- array(NA, dim =len)
lawr.sp <- array(NA, dim =len)
nobr.sp <- array(NA, dim =len)
pea1.sp <- array(NA, dim =len)
pea2.sp <- array(NA, dim =len)

#Loop - Filling the Table 1
for(i in 1:nrow(pita)) {
  pita.sp[pita$order[i]]<-pita$wspd[i]
}
for(i in 1:nrow(cath)) {
  cath.sp[cath$order[i]]<-cath$wspd[i]
}
for(i in 1:nrow(henz)) {
  henz.sp[henz$order[i]]<-henz$wspd[i]
}
for(i in 1:nrow(falk)) {
  falk.sp[falk$order[i]]<-falk$wspd[i]
}
for(i in 1:nrow(char)) {
  char.sp[char$order[i]]<-char$wspd[i]
}
for(i in 1:nrow(penn)) {
  penn.sp[penn$order[i]]<-penn$wspd[i]
}
for(i in 1:nrow(alla)) {
  alla.sp[alla$order[i]]<-alla$wspd[i]
}
for(i in 1:nrow(lib1)) {
  lib1.sp[lib1$order[i]]<-lib1$wspd[i]
}
for(i in 1:nrow(lawr)) {
  lawr.sp[lawr$order[i]]<-lawr$wspd[i]
}
for(i in 1:nrow(nobr)) {
  nobr.sp[nobr$order[i]]<-nobr$wspd[i]
}
for(i in 1:nrow(pea1)) {
  pea1.sp[pea1$order[i]]<-pea1$wspd[i]
}
for(i in 1:nrow(pea2)) {
  pea2.sp[pea2$order[i]]<-pea2$wspd[i]
}

#Filling the Table 2
HR <- c((Start/3600): (Last/3600))
speeds <- data.frame(HR, pita.sp, cath.sp, henz.sp, falk.sp, char.sp, penn.sp, alla.sp, lib1.sp, lawr.sp, nobr.sp, pea1.sp, pea2.sp)

library(tidyr)
speeds_long <- pivot_longer(speeds, names_to = "site", values_to = "speed")
ggplot(speeds_long) +
      geom_line(aes(x = HR, y = speed, color = site))

mod <- lm(pita.sp~cath.sp+henz.sp, data = speeds)
summary(mod)
confint(mod)

hist(speeds$pita.sp)

#Add Mellon Data
mell <- read_csv("https://duq.box.com/shared/static/hf1av8h6a3zny2eb9wcjwkfijs6zul8y.dat", skip=4, col_names=FALSE, col_types = "cnnnncncnnncncnnnnnncnc")
mell <- mell%>%
  rename(time=X1) %>%
  rename(rcrd=`X2`) %>%
  rename(batt=`X3`) %>%
  rename(temp=`X4`) %>%
  rename(tpmx=`X5`) %>%
  rename(tmxt=`X6`) %>%
  rename(tpmn=`X7`) %>%
  rename(tmnt=`X8`) %>%
  rename(tpsd=`X9`) %>%
  rename(wspd=`X10`) %>%
  rename(wsmx=`X11`) %>%
  rename(wsxt=`X12`) %>%
  rename(wsmn=`X13`) %>%
  rename(wsnt=`X14`) %>%
  rename(wssd=`X15`) %>%
  rename(wsvt=`X16`) %>%
  rename(wdir=`X17`) %>%
  rename(wdsd=`X18`) %>%
  rename(prcp=`X19`) %>%
  rename(hmmx=`X20`) %>%
  rename(hmxt=`X21`) %>%
  rename(hmmn=`X22`) %>%
  rename(hmnt=`X23`) %>%
  mutate(time = ymd_hms(time))

plot(wssd ~ time, data = mell)

#Heinz Field and Airport - Sept 2021 Dataframe - One Month
henz.sept2021 <- henz %>%
  filter(dt>ymd_hms("2021-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-10-01 00:00:00")) %>%
  rename(henz.wind = wspd, henz.temp = temp, henz.baro = baro) %>%
  select(dt, henz.wind, henz.temp, henz.baro) %>%
  pivot_longer(cols = c (henz.wind, henz.temp, henz.baro), names_to = "variable", values_to = "value") 

pita.sept2021 <- pita %>%
  filter(dt>ymd_hms("2021-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-10-01 00:00:00")) %>%
  rename(pita.wind = wspd, pita.temp = temp, pita.baro = baro) %>%
  select(dt, pita.wind, pita.temp, pita.baro) %>%
  pivot_longer(cols = c (pita.wind, pita.temp, pita.baro), names_to = "variable", values_to = "value")

henz.pita.sept2021 <- rbind(henz.sept2021, pita.sept2021)

henz.pita.sept2021 <- pivot_wider(henz.pita.sept2021, names_from = "variable", values_from = "value")

modelsept2021 <- lm(henz.wind~pita.wind, data = henz.pita.sept2021)
summary(modelsept2021)

henz.pita.sept2021$henz.model <- modelsept2021$coefficients[1]+henz.pita.sept2021$pita.wind*modelsept2021$coefficients[2]
library(ggplot2)
ggplot(henz.pita.sept2021) + 
     geom_point(aes(x=henz.wind,y=henz.model)) +
     geom_abline(slope = 1, intercept = 0) +
     xlab("Actual Wind Speed (m/s)") +
     ylab("Modeled Wind Speed (m/s)") +
     xlim(c(0,5)) +
     ylim(c(0,5)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))

#Gamma Distribution Model
henz.pita.sept2021.nonzero <- henz.pita.sept2021 %>%
  filter(pita.wind >0) %>% filter(henz.wind>0)
modelsept2021.glm <- glm(henz.wind~pita.wind, family = Gamma(link="identity"), data = henz.pita.sept2021.nonzero)
summary(modelsept2021.glm)
henz.pita.sept2021.nonzero$henz.model <- modelsept2021.glm$coefficients[1]+henz.pita.sept2021.nonzero$pita.wind*modelsept2021.glm$coefficients[2]

ggplot(henz.pita.sept2021.nonzero) + 
  geom_point(aes(x=henz.wind,y=henz.model)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Wind Speed (m/s)") +
  ylab("Modeled Wind Speed (m/s)") +
  xlim(c(0,5)) +
  ylim(c(0,5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))

cor(henz.pita.sept2021.nonzero$henz.wind,henz.pita.sept2021.nonzero$henz.model)

#Heinz Field and Airport - Sept 2020-Sept 2021 Dataframe
henz.sept <- henz %>%
  filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
  rename(henz.wind = wspd, henz.temp = temp, henz.baro = baro) %>%
  select(dt, henz.wind, henz.temp, henz.baro) %>%
  pivot_longer(cols = c (henz.wind, henz.temp, henz.baro), names_to = "variable", values_to = "value") 

pita.sept <- pita %>%
  filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
  rename(pita.wind = wspd, pita.temp = temp, pita.baro = baro) %>%
  select(dt, pita.wind, pita.temp, pita.baro) %>%
  pivot_longer(cols = c (pita.wind, pita.temp, pita.baro), names_to = "variable", values_to = "value")

henz.pita.sept <- rbind(henz.sept, pita.sept)

henz.pita.sept <- pivot_wider(henz.pita.sept, names_from = "variable", values_from = "value")

#Model 1
model1 <- lm(henz.wind~pita.wind+pita.temp+pita.baro, data = henz.pita.sept)
summary(model1)

henz.pita.sept$henz.model <- model1$coefficients[1]+henz.pita.sept$pita.wind*model1$coefficients[2]
library(ggplot2)
ggplot(henz.pita.sept) + 
  geom_point(aes(x=henz.wind,y=henz.model)) +
  #geom_smooth(aes(x=henz.wind,y=henz.model), method="lm") +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Wind Speed (m/s)") +
  ylab("Modeled Wind Speed (m/s)") +
  xlim(c(0,5)) +
  ylim(c(0,5)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))

#Gamma Distribution Model
henz.pita.sept.nonzero <- henz.pita.sept %>%
  filter(pita.wind >0) %>% filter(henz.wind>0)
model1.glm <- glm(henz.wind~pita.wind+pita.baro, family = Gamma(link="identity"), data = henz.pita.sept.nonzero)
summary(model1.glm)
henz.pita.sept.nonzero$model1 <- model1.glm$coefficients[1]+henz.pita.sept.nonzero$pita.wind*model1.glm$coefficients[2]+model1.glm$coefficients[3]*henz.pita.sept.nonzero$pita.baro

ggplot(henz.pita.sept.nonzero) + 
  geom_point(aes(x=henz.wind,y=model1)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Wind Speed (m/s)") +
  ylab("Modeled Wind Speed (m/s)") +
  xlim(c(0,8)) +
  ylim(c(0,8)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))

henz.pita.sept.nonzero.nonna <- henz.pita.sept.nonzero %>%
  filter(is.na(model1)==FALSE) %>%
  filter(is.na(henz.wind)==FALSE)

cor(henz.pita.sept.nonzero.nonna$henz.wind,henz.pita.sept.nonzero.nonna$model1)

#Model test
septembermodel.1 <- model1$coefficients[1]+henz.pita.sept$pita.wind*model1$coefficients[2]

plot(henz.pita.sept$henz.wind, septembermodel.1, xlim = c(0,8), ylim = c(0,8))
lines(c(0,8), c(0,8))

#Model Temp
modeltemp.henz.pita <- lm(henz.temp~pita.temp, data = henz.pita.sept)
summary(modeltemp.henz.pita)

#Model Pressure - Heinz and Pitt Airport Sept
model.baro.henz.pita <- lm(henz.baro~pita.baro, data = henz.pita.sept)
summary(model.baro.henz.pita)

model.test <- lm(henz.wind~pita.wind+pita.baro, data=henz.pita.sept)
summary(model.test)

#Comparing Cathedral of Learning and Falk - Sept 2020-Sept 2021
cath.sept <- cath %>%
  filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
  rename(cath.wind = wspd, cath.temp = temp, cath.baro = baro) %>%
  select(dt, cath.wind, cath.temp, cath.baro) %>%
  pivot_longer(cols = c (cath.wind, cath.temp, cath.baro), names_to = "variable", values_to = "value") 

falk.sept <- falk %>%
  filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
  filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
  rename(falk.wind = wspd, falk.temp = temp, falk.baro = baro) %>%
  select(dt, falk.wind, falk.temp, falk.baro) %>%
  pivot_longer(cols = c (falk.wind, falk.temp, falk.baro), names_to = "variable", values_to = "value")

cath.falk.sept <- rbind(cath.sept, falk.sept)

cath.falk.sept <- pivot_wider(cath.falk.sept, names_from = "variable", values_from = "value")

#Linear Model of Cathedral of Learning and Falk Sept 2020-Sept 2021 - Just Wind
cath.falk.sept.2020.2021 <- lm(cath.wind~falk.wind, data = cath.falk.sept)
summary(cath.falk.sept.2020.2021)

#GLM of Cathedral of Learning and Falk Sept 2020-Sept 2021 - Just Wind - Gamma Distribution Model
cath.falk.sept.nonzero <- cath.falk.sept %>%
  filter(cath.wind >0) %>% filter(falk.wind>0)
cath.falk.sept.2020.2021.glm <- glm(cath.wind~falk.wind, family = Gamma(link="identity"), data = cath.falk.sept.nonzero)
summary(cath.falk.sept.2020.2021.glm)
cath.falk.sept.nonzero$cath.falk.sept.2020.2021.glm <- cath.falk.sept.2020.2021.glm$coefficients[1]+cath.falk.sept.nonzero$cath.wind*cath.falk.sept.2020.2021.glm$coefficients[2]+cath.falk.sept.2020.2021.glm$coefficients

ggplot(cath.falk.sept.nonzero) + 
  geom_point(aes(x=cath.wind,y=cath.falk.sept.2020.2021.glm)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Wind Speed (m/s)") +
  ylab("Modeled Wind Speed (m/s)") +
  xlim(c(0,8)) +
  ylim(c(0,8)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))

cor(cath.falk.sept.nonzero$cath.wind,cath.falk.sept.nonzero$cath.falk.sept.2020.2021.glm)

#MK Cannot Figure Out What is Going on With these Non NA Values
cath.falk.sept.nonzero.nonna <- cath.falk.sept.nonzero %>%
  filter(is.na(cath.falk.sept.2020.2021)==FALSE)%>%
  filter(is.na(falk.wind)==FALSE)

#Linear Model of Cathedral of Learning and Falk Sept 2020-Sept 2021 - Wind and Baro
cath.falk.sept.2020.2021.wind.baro <- lm(cath.wind~falk.wind+falk.baro, data = cath.falk.sept)
summary(cath.falk.sept.2020.2021.wind.baro)

#GLM of Cathedral of Learning and Falk Sept 2020-Sept 2021 - Wind and Baro - Gamma Distribution Model
cath.falk.sept.wind.baro.nonzero <- cath.falk.sept %>%
  filter(cath.wind >0) %>% filter(falk.wind>0) %>% filter(falk.baro>0)
cath.falk.sept.2020.2021.wind.baro.glm <- glm(cath.wind~falk.wind+falk.baro, family = Gamma(link="identity"), data = cath.falk.sept.nonzero)
summary(cath.falk.sept.2020.2021.wind.baro.glm)
#Problems Start Again Here - Perhaps we adding another variable
cath.falk.sept.wind.baro.nonzero$cath.falk.sept.2020.2021.wind.baro.glm <- cath.falk.sept.2020.2021.wind.baro.glm$coefficients[1]+cath.falk.sept.wind.baro.nonzero$cath.wind*cath.falk.sept.2020.2021.wind.baro.glm$coefficients[2]+cath.falk.sept.2020.2021.wind.baro.glm$coefficients

cath.falk.sept.wind.baro.nonzero.nonna <- cath.falk.sept.wind.baro.nonzero %>%
  filter(is.na(cath.falk.sept.2020.2021.wind.baro)==FALSE)%>%
  filter(is.na(falk.wind)==FALSE)

ggplot(cath.falk.sept.wind.baro.nonzero) + 
  geom_point(aes(x=cath.wind,y=cath.falk.sept.2020.2021.wind.baro.glm)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("Actual Wind Speed (m/s)") +
  ylab("Modeled Wind Speed (m/s)") +
  xlim(c(0,8)) +
  ylim(c(0,8)) +
  theme(panel.background = element_rect(fill = "white", colour = "black")) +
  theme(aspect.ratio = 1) +
  theme(axis.text = element_text(face = "plain", size = 12))

cor(cath.falk.sept.nonzero$cath.wind,cath.falk.sept.nonzero$cath.falk.sept.2020.2021.glm)
