# Wind data from Pittsburgh International Airport.
# Example of rose plot of wind direction

library(tidyverse) # includes ggplot and readr commands

x <- read_csv("/Users/davidkahler/Documents/Air_Quality/PGH_weather.csv")
# wind direction data are at x$`Wind Dir`
br <- 10*(c(0:36))
h <- hist(x$`Wind Dir`, breaks = br)

# Make rose plot, based on:
# https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
# https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
angle <- h$mids
count <- h$counts
y <- data.frame(angle, count)
ggplot(y, aes(x = angle, y = count)) +
      geom_col(fill = "steelblue", color = "steelblue") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank())

# April only
a <- x[which(x$Month == 4), names(x) %in% c("Year", "Month", "Day", "Atemp", "RH", "Wind Dir", "Precip")]
g <- hist(a$`Wind Dir`, breaks = br)
ga <- g$mids
gc <- g$counts
b <- data.frame(ga, gc)
ggplot(b, aes(x = ga, y = gc)) +
      geom_col(fill = "steelblue", color = "steelblue") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank())

