#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)
library(latex2exp)

natgas <- read.csv("https://duq.box.com/shared/static/hhn87zjfa4qdj78x9q7ld10fmcc31gqy.csv")
vawt <- read.csv("https://duq.box.com/shared/static/qylrrmgh9wf28yfciv5r7rlsue1fdxoh.csv")
hawt <- read.csv("https://duq.box.com/shared/static/zzvgseedc6hq0dwhbo2qg1v9tt47kjf2.csv")

boxplot(natgas,
        main = "Global Warming Potential from Natural Gas Power Plants",
        ylab = "kg CO2-eq/kWh")

boxplot(vawt,
        main = "Global Warming Potential from Vertical Axis Wind Turbines",
        ylab = "kg CO2-eq/kWh")

boxplot(hawt,
        main = "Global Warming Potential from Horizontal Axis Wind Turbines",
        ylab = "kg CO2-eq/kWh")

natgas <- natgas %>%
     rename(Value=Gas) %>%
     mutate(Measurement="Natural Gas")
hawt <- hawt %>%
     rename(Value=HAWT) %>%
     mutate(Measurement="HAWT")
vawt <- vawt %>%
     rename(Value=VAWT) %>%
     mutate(Measurement="VAWT")
dat <- rbind(natgas,hawt,vawt)
dat$Measurement <- factor(dat$Measurement, levels = c("HAWT", "VAWT", "Natural Gas"))

ggplot(dat) +
     geom_boxplot(aes(Measurement,Value)) +
     xlab("Source") +
     ylab(TeX('Global Warming Potential (kg $CO_2-\\textit{eq}$/kWh)') ) +
     ylim(c(0,1)) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 14), 
           axis.title = element_text(face = "plain", size = 16))


