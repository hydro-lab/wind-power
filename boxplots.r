#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

natgas <- read.csv("https://duq.box.com/shared/static/hhn87zjfa4qdj78x9q7ld10fmcc31gqy.csv")

boxplot(natgas,
        main = "Global Warming Potential from Natural Gas Power Plants",
        ylab = "kg CO2-eq/kWh")

vawt <- read.csv("https://duq.box.com/shared/static/qylrrmgh9wf28yfciv5r7rlsue1fdxoh.csv")

boxplot(vawt,
        main = "Global Warming Potential from Vertical Axis Wind Turbines",
        ylab = "kg CO2-eq/kWh")

hawt <- read.csv("https://duq.box.com/shared/static/zzvgseedc6hq0dwhbo2qg1v9tt47kjf2.csv")

boxplot(hawt,
        main = "Global Warming Potential from Horizontal Axis Wind Turbines",
        ylab = "kg CO2-eq/kWh")
