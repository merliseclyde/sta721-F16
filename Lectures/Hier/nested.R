workstation = read.table(hh("datasets/workstation.dat"), header=T)
workstation$method = factor(workstation$method)
workstation$station = factor(workstation$station)

ws.aov.fixed= aov(devices ~ method + method:station, data=workstation)
plot(ws.aov.fixed)

summary(aov(devices ~ method / station, data=workstation))
summary(aov(devices ~ method + station %in% method, data=workstation))
summary(aov(devices ~ method + method:station, data=workstation))



# if stations are instead a random effect?
summary(aov(devices ~ method + Error(station %in% method), data=workstation))


