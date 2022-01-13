#Model Building - linear
model4=lm(pm25log~date + precip, data = lincoln_daily)
summary(model4)
confint(model4)