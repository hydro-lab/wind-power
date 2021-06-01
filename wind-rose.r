## Wind data from Pittsburgh International Airport.
## Example of rose plot of wind direction

library(tidyverse) # includes ggplot and readr commands
library(RColorBrewer) # used for wind rose with color code by speed

## Pittsburgh International Airport (PIT)
pit <- read_csv("https://duq.box.com/shared/static/i9qlh63qdzf5hqvf40dphkbwkh93o2h0.csv")
pit.dir <- pit$`Wind Dir` # degrees
pit.spd <- 0.51444444444 * pit$`Wind Speed` # converted to m/s from knots, per http://www.climate.psu.edu/data/current/help.php

## Heinz Field (HFP)
hfp <- read_csv("https://duq.box.com/shared/static/2cs6xi81xtcmq4mmi46t0v0ev4f2mehs.csv")
hfp.dir <- hfp$`Wind Vane` # degrees
hfp.spd <- 0.44704 * hfp$Anemometer # converted to m/s from mph, per https://allegheny.weatherstem.com/pitt

## OLD METHOD - no speed binning
# br <- 10*(c(0:36)) # This array constructs the bins in degrees
# h <- hist(hfp.dir, breaks = br)
## Make rose plot, based on:
## https://stackoverflow.com/questions/39024758/how-to-use-r-package-circular-to-make-rose-plot-of-histogram-data-on-360/39025913
## https://stackoverflow.com/questions/50163352/plot-wind-rose-in-r
# angle <- h$mids
# count <- h$counts
# y <- data.frame(angle, count)
# ggplot(y, aes(x = angle, y = count)) + 
#       labs(caption = "Heinz Field") + 
#       geom_col(fill = "steelblue", color = "steelblue") +
#       coord_polar(theta = "x", start = 0) +
#       scale_x_continuous(breaks = seq(0, 360, 45)) +
#       theme_linedraw() +
#       theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) 

## NEW METHOD - with speed binning
## sort data:
d <- pit.dir
s <- pit.spd
speed.bins <- ceiling(max(s))
speed.bins <- 6
wind <- array(0, dim = c(36,speed.bins))
for (i in 1:(length(s))) {
      # speed.index <- ceiling(s[i])
      ## for use when speed categories exceed colormap (>9)
      if (s[i] <= 2) {
            speed.index <- 1
      } else if (s[i] <= 4) {
            speed.index <- 2
      } else if (s[i] <= 6) {
            speed.index <- 3
      } else if (s[i] <= 8) {
            speed.index <- 4
      } else if (s[i] <= 10) {
            speed.index <- 5
      } else {
            speed.index <- 6
      }
      wind[ceiling(d[i]/10),speed.index] <- wind[ceiling(d[i]/10),speed.index] + 1
}
## Now, form long array rather than wide:
wind.long <- array(NA, dim = 36*speed.bins)
for (i in 1:speed.bins) {
      for (j in 1:36) {
            wind.long[(36*(i-1))+j] <- wind[j,i]
      }
}
speeds <- c(rep("0-2",36), rep("2-4",36), rep("4-6",36), rep("6-8",36), rep("8-10",36), rep("above 10",36)) # be sure to fill in as many as the wind bins in "wind" allocation
directions <- rep(5+10*(c(0:35)), speed.bins)
rose <- data.frame(directions, speeds, wind.long)

ggplot(rose, aes(fill = fct_rev(speeds), x = directions, y = wind.long)) + 
      labs(caption = "Heinz Field") + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_brewer("Speed (m/s)", palette = "Blues") +
      coord_polar(theta = "x", start = 0) +
      scale_x_continuous(breaks = seq(0, 360, 45)) +
      theme_linedraw() +
      theme(axis.title = element_blank(), panel.ontop = TRUE, panel.background = element_blank()) 

# caption = "Pittsburgh International Airport" + 
# caption = "Heinz Field" + 
