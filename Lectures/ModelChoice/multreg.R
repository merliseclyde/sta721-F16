# see EDA.R Exercise 4.2
library(HH)
pollution = read.table(hh("datasets/usair.dat"))
colnames(pollution) = c("SO2","temp","firms","popn", "wind", "precip", "rain")

pairs(pollution,diag.panel=panel.hist )
pairs(log(SO2) ~ log(temp) + log(firms) + log(popn) + log(wind) + log(precip)+ log(rain), data=pollution, diag.panel=panel.hist)

pairs(log(SO2) ~ temp + firms + popn + wind + precip+ rain, data=pollution, diag.panel=panel.hist)
round(cor(pollution), digits=2)
cor(log(pollution))
poll.lm = lm(SO2 ~ ., data=pollution)
postscript("poll-res.ps")
par(mfrow=c(2,2))
plot(poll.lm, ask=F)
dev.off()
library(MASS)
postscript(boxcox.ps)
boxcox(poll.lm)
dev.off()
# log transform SO2

poll.lm2 = lm(log(SO2) ~ ., data=pollution)
postscript("poll-res2.ps")
par(mfrow=c(2,2))
plot(poll.lm2, ask=F)
dev.off()
plot(hatvalues(poll.lm2), type="h", ylab="Leverage", xlab="Case Index")
abline(h=2*(6+1)/41)
text(25, .36, expression(paste("high leverage point if  ", h[i]> 2*(p+1)/n)))

postscript("poll-trans-pairs.ps")
pairs(log(SO2) ~ temp + log(firms) + log(popn) + wind + precip + rain, data=pollution,  diag.panel=panel.hist)
dev.off()

poll.lm3 = lm(log(SO2) ~ temp + log(firms) + log(popn) + wind + precip + rain, data=pollution)

attach(pollution)
round(cor(data.frame(log(SO2),temp,log(firms),log(popn),wind, precip, rain)), digits=2)

plot(hatvalues(poll.lm3), type="h", ylab="Leverage", xlab="Case Index")
abline(h=2*(6+1)/41)
text(25, .36, expression(paste("high leverage point if  ", h[i]> 2*(p+1)/n)))

par(mfrow=c(2,2))
plot(poll.lm3, ask=F)
library(car)
help(av.plots)
par(mfrow=c(2,3))
av.plots(poll.lm3, ask=F, one.page=T, iden=F)


summary(poll.lm3)

anova(poll.lm3)

vif(log(SO2)~temp + log(firms) + log(popn) + wind + precip + rain, data=pollution)

