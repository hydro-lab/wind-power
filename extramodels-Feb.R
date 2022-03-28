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
cath.falk.sept.2020.2021 <- lm(cath.wind~ 0 + falk.wind, data = cath.falk.sept)
summary(cath.falk.sept.2020.2021)

#Linear Model of Falk and Cathedral of Learning (other way) Sept 2020-Sept 2021 - Just Wind
falk.cath.sept <- rbind(falk.sept, cath.sept)

falk.cath.sept <- pivot_wider(falk.cath.sept, names_from = "variable", values_from = "value")

falk.cath.sept.2020.2021 <- lm(falk.wind~0 + cath.wind, data = falk.cath.sept)
summary(falk.cath.sept.2020.2021)

falk.cath.sept$falk.cath.sept.lm <- falk.cath.sept.2020.2021$coefficients[1]*falk.cath.sept$cath.wind
plot(falk.cath.sept$falk.cath.sept.lm, falk.cath.sept$falk.wind)

#GLM of Cathedral of Learning and Falk Sept 2020-Sept 2021 - Just Wind - Gamma Distribution Model
cath.falk.sept.nonzero <- cath.falk.sept %>%
        filter(cath.wind >0) %>% filter(falk.wind>0)
cath.falk.sept.2020.2021.glm <- glm(cath.wind~falk.wind, family = Gamma(link="identity"), data = cath.falk.sept.nonzero)
summary(cath.falk.sept.2020.2021.glm)
cath.falk.sept.nonzero$cath.falk.sept.2020.2021.glm <- cath.falk.sept.2020.2021.glm$coefficients[1]+cath.falk.sept.nonzero$falk.wind*cath.falk.sept.2020.2021.glm$coefficients[2]

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

#Inverse
cath.falk.sept.nonzero <- cath.falk.sept %>%
        filter(cath.wind >0) %>% filter(falk.wind>0)
cath.falk.sept.2020.2021.glm <- glm(cath.wind~falk.wind, family = Gamma(link="inverse"), data = cath.falk.sept.nonzero)
summary(cath.falk.sept.2020.2021.glm)
cath.falk.sept.nonzero$cath.falk.sept.2020.2021.glm <- cath.falk.sept.2020.2021.glm$coefficients[1]-cath.falk.sept.nonzero$falk.wind*cath.falk.sept.2020.2021.glm$coefficients[2]

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

#GLM of Falk and Cathedral of Learning (other way) Sept 2020-Sept 2021 - Just Wind - Gamma Distribution Model
falk.cath.sept.nonzero <- falk.cath.sept %>%
        filter(falk.wind >0) %>% filter(cath.wind>0)
falk.cath.sept.2020.2021.glm <- glm(falk.wind~cath.wind, family = Gamma(link="identity"), data = falk.cath.sept.nonzero)
summary(falk.cath.sept.2020.2021.glm)
falk.cath.sept.nonzero$falk.cath.sept.2020.2021.glm <- falk.cath.sept.2020.2021.glm$coefficients[1]+falk.cath.sept.nonzero$falk.wind*falk.cath.sept.2020.2021.glm$coefficients[2]+falk.cath.sept.2020.2021.glm$coefficients

ggplot(falk.cath.sept.nonzero) + 
        geom_point(aes(x=falk.wind,y=falk.cath.sept.2020.2021.glm)) +
        geom_abline(slope = 1, intercept = 0) +
        xlab("Actual Wind Speed (m/s)") +
        ylab("Modeled Wind Speed (m/s)") +
        xlim(c(0,8)) +
        ylim(c(0,8)) +
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(aspect.ratio = 1) +
        theme(axis.text = element_text(face = "plain", size = 12))

cor(falk.cath.sept.nonzero$falk.wind,falk.cath.sept.nonzero$falk.cath.sept.2020.2021.glm)

#Inverse
falk.cath.sept.nonzero <- falk.cath.sept %>%
        filter(falk.wind >0) %>% filter(cath.wind>0)
falk.cath.sept.2020.2021.glm <- glm(falk.wind~cath.wind, family = Gamma(link="inverse"), data = falk.cath.sept.nonzero)
summary(falk.cath.sept.2020.2021.glm)
falk.cath.sept.nonzero$falk.cath.sept.2020.2021.glm <- falk.cath.sept.2020.2021.glm$coefficients[1]+falk.cath.sept.nonzero$falk.wind*falk.cath.sept.2020.2021.glm$coefficients[2]+falk.cath.sept.2020.2021.glm$coefficients

ggplot(falk.cath.sept.nonzero) + 
        geom_point(aes(x=falk.wind,y=falk.cath.sept.2020.2021.glm)) +
        geom_abline(slope = 1, intercept = 0) +
        xlab("Actual Wind Speed (m/s)") +
        ylab("Modeled Wind Speed (m/s)") +
        xlim(c(0,8)) +
        ylim(c(0,8)) +
        theme(panel.background = element_rect(fill = "white", colour = "black")) +
        theme(aspect.ratio = 1) +
        theme(axis.text = element_text(face = "plain", size = 12))

cor(falk.cath.sept.nonzero$falk.wind,falk.cath.sept.nonzero$falk.cath.sept.2020.2021.glm)


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


