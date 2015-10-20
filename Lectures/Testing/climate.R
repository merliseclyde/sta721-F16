climate = read.table("http://www.stat.duke.edu/courses/Fall10/sta290/datasets/climate.dat",  header=T)
names(climate)
summary(climate)

climate$T.M  = factor(climate$T.M, labels= c("T", "M"))
climate$proxy = factor(climate$proxy, labels = c("Mg/Ca", "Alkenone", "Faunal", "Sr/Ca", "Del180", "IceCore", "Pollen", "Noble Gas"))

# pairs plot
plot(climate)

attach(climate)
pdf("temp-lat.pdf")

plot(deltaT ~ latitude, data=climate, col=1, pch = as.numeric(T.M)+23, bg = as.numeric(proxy), ylim=c(min(deltaT) - 2*max(sdev), max(deltaT) + 2*max(sdev)),
     ylab="Delta T")
title("Change in Temperature by Proxy")
abline(h=0, lty=2 )

segments(latitude, deltaT-2*sdev, latitude, deltaT+2*sdev, lty=3)
points(latitude, deltaT, col=1, pch = as.numeric(T.M)+23, bg = as.numeric(proxy))
#legend(-20,2, levels(proxy), pch=c(rep(24,5),rep(25,3)), col = as.numeric(proxy))
dev.off()
# co(nditional) plots 
pdf("temp-lat-proaxy.pdf")
coplot(deltaT ~ latitude | proxy, data=climate)
dev.off()

pdf("temp-lat-TM.pdf")
coplot(deltaT ~ latitude | T.M, data=climate)
def.off()

climate.lm = lm(deltaT ~ proxy *(poly(latitude,2)), weights=(1/sdev^2), data=climate)
plot(climate.lm)
anova(climate.lm)

climate1.lm = lm(deltaT ~ poly(latitude,2) + proxy, weights=1/sdev^2,data=climate)
climate2.lm = lm(deltaT ~ proxy, weights=1/sdev^2,data=climate) 
climate3.lm = lm(deltaT ~ T.M, weights=(1/sdev^2), data=climate)

anova(climate3.lm,climate2.lm,climate1.lm, climate.lm)

install.packages("car", dependencies=T)
library(car)
pdf("avplot.pdf")
avPlots(climate1.lm, terms=~.)
dev.off()

pdf("pred-temp-lat.pdf")
climate.lat.lm = lm(deltaT ~ latitude, data=climate, weights=1/sdev^2)
pred.deltaT = fitted(climate.lat.lm)

plot(deltaT ~ latitude, data=climate, col=1, pch = as.numeric(T.M)+23, bg = as.numeric(proxy), ylim=c(min(deltaT) - 2*max(sdev), max(deltaT) + 2*max(sdev)),
     ylab="Delta T")
lines(latitude[order(latitude)], pred.deltaT[order(latitude)], lwd=2)
title("Change in Temperature by Proxy")
abline(h=0, lty=2 )

segments(latitude, deltaT-2*sdev, latitude, deltaT+2*sdev, lty=3)
points(latitude, deltaT, col=1, pch = as.numeric(T.M)+23, bg = as.numeric(proxy))
#legend(-20,2, levels(proxy), pch=c(rep(24,5),rep(25,3)), col = as.numeric(proxy))
dev.off()


pdf("lat-proxy.pdf")
plot(latitude,proxy, pch=as.numeric(T.M)+23, bg=as.numeric(proxy))
dev.off()
      
 pdf("resid.pdf")
 par(mfrow=c(2,2))
 plot(climate2.lm)
 dev.off()

climate.final = lm(deltaT ~ T.M + proxy -1,  weights=(1/sdev^2), data=climate)
summary(climate.final)
anova(climate.final)

climate.smp = lm(formula = deltaT ~ T.M + I(proxy == "Sr/Ca") - 1, data = climate, weights = (1/sdev^2))
summary(climate.smp)
anova(climate.smp, climate.final)

pdf("box.pdf",width=8,height=5)
 plot(deltaT ~ proxy, col=c(rep("blue",5), rep("green",3)))
dev.off()

