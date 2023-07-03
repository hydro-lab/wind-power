library(dplyr)
library(ggplot2)

type <- c("Wind",  "Wind",     "Wind",       "Wind",                  "Gas",   "Gas",      "Gas",        "Gas")
cost <- c("Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon", "Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon")
cost = factor(cost, levels = c("Price", "CO2 DALY", "PM2.5 DALY", "Social Cost of Carbon"))
valu <- c( 1.25,     0.0145,     0.0853,       0.00596,                 0.1273,  0.0983,     0.1529,       0.0363)
high <- c( 1.25,     1.24,       0.639,        0.0748,                  0.1273,  8.45,       1.146,        0.456)
lows <- c( 1.25,     0.00755,    0.0081,       0.000117,                0.1273,  0.00515,    0.0145,       0.00071)
dat <- data.frame(type, cost, valu, high, lows)

low <- ggplot(dat, aes(fill=cost, y=lows, x=type)) + 
     geom_bar(position="stack", stat="identity", show.legend = FALSE) +
     xlab("") +
     ylab("Cost ($/kWh)") +
     ylim(c(0,1.5)) +
     guides(fill=guide_legend(title="Impact Categories")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 10), 
           axis.title = element_text(face = "plain", size = 10)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 10), 
           legend.title = element_text(face = "plain", size = 10)) 
glow <- ggplotGrob(low)

med <- ggplot(dat, aes(fill=cost, y=valu, x=type)) + 
     geom_bar(position="stack", stat="identity", show.legend = FALSE) +
     xlab("") +
     ylab("") +
     ylim(c(0,1.5)) +
     guides(fill=guide_legend(title="Impact Categories")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 10), 
           axis.title = element_text(face = "plain", size = 10)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 10), 
           legend.title = element_text(face = "plain", size = 10)) 
gmed <- ggplotGrob(med)

hih <- ggplot(dat, aes(fill=cost, y=high, x=type)) + 
     geom_bar(position="stack", stat="identity") +
     xlab("") +
     ylab("") +
     # ylim(c(0,12)) +
     scale_y_continuous(breaks = seq(0,12,5)) +
     guides(fill=guide_legend(title="Impact Categories")) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 10), 
           axis.title = element_text(face = "plain", size = 10)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 10), 
           legend.title = element_text(face = "plain", size = 10)) 
ghih <- ggplotGrob(hih)

grid::grid.newpage()
tot <- grid::grid.draw(cbind(glow,gmed,ghih))

ggplot(dat, aes(fill=cost, y=valu, x=type)) + 
     geom_bar(position="dodge", stat="identity") +
     geom_errorbar(aes(ymin = lows, ymax = high), width = 0.3, position = position_dodge(0.9)) +
     coord_cartesian(ylim = c(0,1.5)) +
     xlab("") +
     ylab("Cost ($)") +
     guides(fill=guide_legend(title="Impact Categories")) +
     annotate("text", x="Gas", y=1.4, label = "           max=8.45", size = 5) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1.6, 
           panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 18), 
           axis.title = element_text(face = "plain", size = 18)) +
     theme(legend.position="right", 
           legend.key = element_blank(), 
           legend.text = element_text(face = "plain", size = 16), 
           legend.title = element_text(face = "plain", size = 16)) 
