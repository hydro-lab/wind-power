library(readr)
library(dplyr)
library(lubridate)

#Collected 24 May 2022
may.wind <- read.csv("https://duq.box.com/shared/static/hmths6ofu40ox2n9shvrdydawd57e7v2.dat")

may.wind <- rnorm(1000)
hist(may.wind)
mean(may.wind)
sd(may.wind)
kurtosis(may.wind)
skewness(may.wind)
