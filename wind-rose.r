# Wind data from Pittsburgh International Airport.
# Example of rose plot of wind direction

library(tidyverse) # includes ggplot and readr commands

# Pittsburgh International Airport (PIT)
x <- read_csv("https://duq.box.com/shared/static/i9qlh63qdzf5hqvf40dphkbwkh93o2h0.csv")

# PNC Park (PNC)
x <- read_csv("https://duq.box.com/shared/static/2cs6xi81xtcmq4mmi46t0v0ev4f2mehs.csv")

# wind direction data are at:
# PIT: x$`Wind Dir`
# PNC: x$`Wind Vane`
br <- 10*(c(0:36)) # This array constructs the bins in degrees
h <- hist(x$`Wind Dir`, breaks = br)
h <- hist(x$`Wind Vane`, breaks = br)

# Make rose plot, based on:
# https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
# https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
angle <- h$mids
count <- h$counts
y <- data.frame(angle, count)
ggplot(y, aes(x = angle, y = count)) + 
      labs(caption = "Pittsburgh International Airport") + 
      geom_col(fill = "steelblue", color = "steelblue") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) 


# caption = "Pittsburgh International Airport" + 
# caption = "PNC Park" + 
