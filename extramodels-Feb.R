#Cathedral and Airport - Sept 2020-Sept 2021 Dataframe
cath.sept <- cath %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(cath.wind = wspd, cath.temp = temp, cath.baro = baro) %>%
     select(dt, cath.wind, cath.temp, cath.baro) %>%
     pivot_longer(cols = c (cath.wind, cath.temp, cath.baro), names_to = "variable", values_to = "value") 

cath.pita.sept <- rbind(cath.sept, pita.sept)

cath.pita.sept <- pivot_wider(cath.pita.sept, names_from = "variable", values_from = "value")

#Model 3 
model3 <- lm(cath.wind~pita.wind, data = cath.pita.sept)
summary(model3)

#Env Charter Scool and Airport - Sept 2020-Sept 2021 Dataframe
char.sept <- char %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(char.wind = wspd, char.temp = temp, char.baro = baro) %>%
     select(dt, char.wind, char.temp, char.baro) %>%
     pivot_longer(cols = c (char.wind, char.temp, char.baro), names_to = "variable", values_to = "value") 

char.pita.sept <- rbind(char.sept, pita.sept)

char.pita.sept <- pivot_wider(char.pita.sept, names_from = "variable", values_from = "value")

#Model 4 
model4 <- lm(char.wind~pita.wind, data = char.pita.sept)
summary(model4)

#Penn State and Airport - Sept 2020-Sept 2021 Dataframe
penn.sept <- penn %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(penn.wind = wspd, penn.temp = temp, penn.baro = baro) %>%
     select(dt, penn.wind, penn.temp, penn.baro) %>%
     pivot_longer(cols = c (penn.wind, penn.temp, penn.baro), names_to = "variable", values_to = "value") 

penn.pita.sept <- rbind(penn.sept, pita.sept)

penn.pita.sept <- pivot_wider(penn.pita.sept, names_from = "variable", values_from = "value")

#Model 5 
model5 <- lm(penn.wind~pita.wind, data = penn.pita.sept)
summary(model5)

#Lawrenceville and Airport - Sept 2020-Sept 2021 Dataframe
lawr.sept <- lawr %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(lawr.wind = wspd, lawr.temp = temp, lawr.baro = baro) %>%
     select(dt, lawr.wind, lawr.temp, lawr.baro) %>%
     pivot_longer(cols = c (lawr.wind, lawr.temp, lawr.baro), names_to = "variable", values_to = "value") 

lawr.pita.sept <- rbind(lawr.sept, pita.sept)

lawr.pita.sept <- pivot_wider(lawr.pita.sept, names_from = "variable", values_from = "value")

#Model 6 
model6 <- lm(lawr.wind~pita.wind, data = lawr.pita.sept)
summary(model6)

#Liberty and Airport - Sept 2020-Sept 2021 Dataframe
lib1.sept <- lib1 %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(lib1.wind = wspd, lib1.temp = temp, lib1.baro = baro) %>%
     select(dt, lib1.wind, lib1.temp, lib1.baro) %>%
     pivot_longer(cols = c (lib1.wind, lib1.temp, lib1.baro), names_to = "variable", values_to = "value") 

lib1.pita.sept <- rbind(lib1.sept, pita.sept)

lib1.pita.sept <- pivot_wider(lib1.pita.sept, names_from = "variable", values_from = "value")

#Model 7 
model7 <- lm(lib1.wind~pita.wind, data = lib1.pita.sept)
summary(model7)

#North Braddock and Airport - Sept 2020-Sept 2021 Dataframe
nobr.sept <- nobr %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(nobr.wind = wspd, nobr.temp = temp, nobr.baro = baro) %>%
     select(dt, nobr.wind, nobr.temp, nobr.baro) %>%
     pivot_longer(cols = c (nobr.wind, nobr.temp, nobr.baro), names_to = "variable", values_to = "value") 

nobr.pita.sept <- rbind(nobr.sept, pita.sept)

nobr.pita.sept <- pivot_wider(nobr.pita.sept, names_from = "variable", values_from = "value")

#Model 8 
model8 <- lm(nobr.wind~pita.wind, data = nobr.pita.sept)
summary(model8)

#Parkway 1 and Airport - Sept 2020-Sept 2021 Dataframe
pea1.sept <- pea1 %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(pea1.wind = wspd, pea1.temp = temp, pea1.baro = baro) %>%
     select(dt, pea1.wind, pea1.temp, pea1.baro) %>%
     pivot_longer(cols = c (pea1.wind, pea1.temp, pea1.baro), names_to = "variable", values_to = "value") 

pea1.pita.sept <- rbind(pea1.sept, pita.sept)

pea1.pita.sept <- pivot_wider(pea1.pita.sept, names_from = "variable", values_from = "value")

#Model 9 
model9 <- lm(pea1.wind~pita.wind, data = pea1.pita.sept)
summary(model9)

#Parkway 2 and Airport - Sept 2020-Sept 2021 Dataframe
pea2.sept <- pea2 %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(pea2.wind = wspd, pea2.temp = temp, pea2.baro = baro) %>%
     select(dt, pea2.wind, pea2.temp, pea2.baro) %>%
     pivot_longer(cols = c (pea2.wind, pea2.temp, pea2.baro), names_to = "variable", values_to = "value") 

pea2.pita.sept <- rbind(pea2.sept, pita.sept)

pea2.pita.sept <- pivot_wider(pea2.pita.sept, names_from = "variable", values_from = "value")

#Model XI 
modelxi <- lm(pea2.wind~pita.wind, data = pea2.pita.sept)
summary(modelxi)

#County Airport and Airport - Sept 2020-Sept 2021 Dataframe
alla.sept <- alla %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(alla.wind = wspd, alla.temp = temp, alla.baro = baro) %>%
     select(dt, alla.wind, alla.temp, alla.baro) %>%
     pivot_longer(cols = c (alla.wind, alla.temp, alla.baro), names_to = "variable", values_to = "value") 

alla.pita.sept <- rbind(alla.sept, pita.sept)

alla.pita.sept <- pivot_wider(alla.pita.sept, names_from = "variable", values_from = "value")

#Model X 
modelx <- lm(alla.wind~pita.wind, data = alla.pita.sept)
summary(modelx)

#Model Building - linear
model4=lm(pm25log~date + precip, data = lincoln_daily)
summary(model4)
confint(model4)

#Falk and Airport - Sept 2020-Sept 2021 Dataframe
falk.sept <- falk %>%
     filter(dt>ymd_hms("2020-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-09-01 00:00:00")) %>%
     rename(falk.wind = wspd, falk.temp = temp, falk.baro = baro) %>%
     select(dt, falk.wind, falk.temp, falk.baro) %>%
     pivot_longer(cols = c (falk.wind, falk.temp, falk.baro), names_to = "variable", values_to = "value") 

falk.pita.sept <- rbind(falk.sept, pita.sept)

falk.pita.sept <- pivot_wider(falk.pita.sept, names_from = "variable", values_from = "value")

#Model 2 
model2 <- lm(falk.wind~pita.wind, data = falk.pita.sept)
summary(model2)

#Model Temperature - Falk and Pitt Airport Sept
model.temp.falk.pita <- lm(falk.temp~pita.temp, data = falk.pita.sept)
summary(model.temp.falk.pita)

#Model Pressure - Falk and Pitt Airport Sept
model.baro.falk.pita <- lm(falk.baro~pita.baro, data = falk.pita.sept)
summary(model.baro.falk.pita)

#Falk and Airport - Sept 2021 Dataframe - One Month
falk.sept2021 <- falk %>%
     filter(dt>ymd_hms("2021-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-10-01 00:00:00")) %>%
     rename(falk.wind = wspd, falk.temp = temp, falk.baro = baro) %>%
     select(dt, falk.wind, falk.temp, falk.baro) %>%
     pivot_longer(cols = c (falk.wind, falk.temp, falk.baro), names_to = "variable", values_to = "value") 

pita.sept2021 <- pita %>%
     filter(dt>ymd_hms("2021-09-01 00:00:00")) %>%
     filter(dt<ymd_hms("2021-10-01 00:00:00")) %>%
     rename(pita.wind = wspd, pita.temp = temp, pita.baro = baro) %>%
     select(dt, pita.wind, pita.temp, pita.baro) %>%
     pivot_longer(cols = c (pita.wind, pita.temp, pita.baro), names_to = "variable", values_to = "value")

falk.pita.sept2021 <- rbind(falk.sept2021, pita.sept2021)

falk.pita.sept2021 <- pivot_wider(falk.pita.sept2021, names_from = "variable", values_from = "value")

modelfalk.pita.sept2021 <- lm(falk.wind~pita.wind, data = falk.pita.sept2021)
summary(falk.pita.sept2021)

falk.pita.sept2021$falk.model <- modelfalk.pita.sept2021$coefficients[1]+falk.pita.sept2021$pita.wind*falk.pita.sept2021$coefficients[2]
library(ggplot2)
ggplot(falk.pita.sept2021) + 
     geom_point(aes(x=falk.wind,y=falk.model)) +
     geom_abline(slope = 1, intercept = 0) +
     xlab("Actual Wind Speed (m/s)") +
     ylab("Modeled Wind Speed (m/s)") +
     xlim(c(0,5)) +
     ylim(c(0,5)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12))
