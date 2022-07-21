#reading in different packages to do certain tasks
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(lubridate)
library(ggplot2)
library(EnvStats)
library(e1071)

# plot data and Weibull distrubution
m <- (c(1:500)) / 100  #creating a bunch of numbers to run through the distribution (from 0.1 to 6)
shape <- 8
scal <- 2 #using the standard scale factor
n <- (shape/scal) * (m/scal)^(shape-1) * exp(-((m/scal)^shape)) #Weibull equation
wei <- data.frame(m,n) #showing the weibull distribution with outlined parameters

geo_mean <- gamma((shape+1)/shape) #geometric mean using gamma 

ggplot() +
     geom_line(data=wei, aes(x=m,y=n))
     #geom_vline(xintercept = geo_mean)
