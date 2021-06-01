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
d <- hfp.dir
s <- hfp.spd
wind <- array(0, dim = c(36,ceiling(max(s))))
for (i in 1:(length(s))) {
      wind[ceiling(d[i]/10),ceiling(s[i])] <- wind[ceiling(d[i]/10),ceiling(s[i])] + 1
}
## Now, form long array rather than wide:
wind.long <- array(NA, dim = 36*ceiling(max(s)))
for (i in 1:ceiling(max(s))) {
      for (j in 1:36) {
            wind.long[(36*(i-1))+j] <- wind[j,i]
      }
}
speeds <- c(rep("0-1",36), rep("1-2",36), rep("2-3",36), rep("3-4",36), rep("4-5",36), rep("5-6",36), rep("6-7",36)) # be sure to fill in as many as the wind bins in "wind" allocation
directions <- rep(5+10*(c(0:35)), ceiling(max(s)))
rose <- data.frame(directions, speeds, wind.long)
names(rose)[2] <- "`Speed (m/s)`"

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
