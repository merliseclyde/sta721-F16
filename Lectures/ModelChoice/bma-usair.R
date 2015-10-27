# see EDA.R Exercise 4.2
library(HH)
pollution = read.table(hh("datasets/usair.dat"))
colnames(pollution) = c("SO2","temp","firms","popn", "wind", "precip", "rain")

pairs(pollution,diag.panel=panel.hist )
poll.lm = lm(log(SO2) ~ temp + log(firms) + log(popn) + wind + precip+ rain, data=pollution)
par(mfrow=c(2,2))
plot(poll.lm, ask=F)
library(car)
help(av.plots)
par(mfrow=c(2,3))
av.plots(poll.lm, ask=F, one.page=T, iden=F)
vif(log(SO2)~temp + log(firms) + log(popn) + wind + precip + rain, data=pollution)

library(BAS)
poll.bma = bas.lm(log(SO2) ~ temp + log(firms) + log(popn) + wind + precip+ rain, data=pollution, prior="g-prior", alpha=41, n.models=2^7, update=50, initprobs="Uniform")

postscript("poll-bma-sum.ps")
par(mfrow=c(2,2))
plot(poll.bma, ask=F)
dev.off()

par(mfrow=c(1,1))
postscript("poll-image.ps")
image(poll.bma)
dev.off()

beta = coef(poll.bma)
postscript("poll-beta.ps")
par(mfrow=c(2,3))
plot(beta, subset=2:7,ask=F)
dev.off()

# pollution & mortality data

ex1217 = read.table("../datasets/sleuthdata/ex1217.csv", sep=",", header=T)
ex1217$logHC = log(HC)
ex1217$logNOX = log(NOX)
ex1217$logSO2 = log(SO2)
mortality = data.frame(ex1217[,c(2:14, 18:20)])

mort.bma = bas.lm(MORTALITY ~ ., data=mortality, prior="g-prior", n.models=2^15, update=100, initprobs="eplogp")
