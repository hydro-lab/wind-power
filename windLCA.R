library(dplyr)
library(ggplot2)

type <- c("Wind",  "Wind",     "Wind",       "Wind",                  "Gas",   "Gas",      "Gas",        "Gas")
cost <- c("Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon", "Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon")
cost = factor(cost, levels = c("Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon"))
valu <- c( 0.9,     0.0145,     0.0853,       0.00596,                 0.1273,  0.0983,     0.1529,       0.0363)
high <- c( 0.9,     1.24,       0.639,        0.0748,                  0.1273,  8.45,       1.146,        0.456)
lows <- c( 0.9,     0.00755,    0.0081,       0.000117,                0.1273,  0.00515,    0.0145,       0.00071)
dat <- data.frame(type, cost, valu, high, lows)

ggplot(dat, aes(fill=cost, y=valu, x=type)) + 
     geom_bar(position="stack", stat="identity") +
     xlab("") +
     ylab("Cost ($)") +
     guides(fill=guide_legend(title="Impact Categories")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 18), 
           axis.title = element_text(face = "plain", size = 18)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 16), 
           legend.title = element_text(face = "plain", size = 16)) 

ggplot(dat, aes(fill=cost, y=valu, x=type)) + 
     geom_bar(position="dodge", stat="identity") +
     geom_errorbar(aes(ymin = lows, ymax = high), width = 0.3, position = position_dodge(0.9)) +
     coord_cartesian(ylim = c(0,1.5)) +
     xlab("") +
     ylab("Cost ($)") +
     guides(fill=guide_legend(title="Impact Categories")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 18), 
           axis.title = element_text(face = "plain", size = 18)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 16), 
           legend.title = element_text(face = "plain", size = 16)) 
