# frequentists analysis
library(MASS)
library(car)
data(stackloss)
# variables   Air.Flow Water.Temp Acid.Conc. stack.loss

pairs(stackloss) #, diag.panel=panel.hist,panel=panel.smooth)
stack.lm <- lm(stack.loss ~ ., data=stackloss)
avPlots(stack.lm)
par(mfrow=c(2,2))
plot(stack.lm)

###
n = nrow(stackloss)
stack.out = cbind(stackloss, diag(n))
is.data.frame(stack.out)

library(BAS)

BAS.stack = bas.lm(stack.loss ~ ., method="MCMC", prior="hyper-g-laplace", a=3, modelprior=beta.binomial(1, 1), data=stack.out, n.models=2^20)



BAS.stack = bas.lm(stack.loss ~ ., method="MCMC", prior="hyper-g-n", a=3, modelprior=beta.binomial(1, 1), data=stack.out, n.models=2^20, MCMC.it=2^20)
BAS.stack.11 = bas.lm(stack.loss ~ ., method="MCMC", prior="hyper-g-n", a=3, modelprior=beta.binomial(1, 1), data=stack.out, n.models=2^18, init="Uniform", update=50)
# note using BAS in the above problem leads to NA's as models that are not full rank are sampled, but arguments to gamma function are not using rank, but dimension  Need to fix!   MCMC does not seem to encounter that problem

pdf("BAS-stackloss.pdf")
par(mfrow=c(2,2))
plot(BAS.stack, ask=F)
dev.off()

pdf("image-stackloss.pdf")
image(BAS.stack)
dev.off()
pdf("BAS-diagnostic.pdf")
plot(BAS.stack$probne0, BAS.stack$probs.MCMC)
title("Renormalized likelihood Estimates vs MCMC frequencies")
dev.off()

install.packages("BMA")
library(BMA)
help(package=BMA)
help(MC3.REG)
attach(stackloss)
stack.MC3= MC3.REG(stack.loss, stackloss[,-4]
  ,num.its=10000, outliers=TRUE, M0.out=rep(FALSE, 21), outs.list=1:21, M0.var=rep(TRUE, 3))

summary(stack.MC3)
plot(Water.Temp, stack.loss)
identify(Water.Temp, stack.loss)

# other diagnostics
im = influence.measures(stack.lm)
plot(rstudent(stack.lm) ~ hatvalues(stack.lm), ylab="Externally Studentized Residual", xlab="Leverage")
identify(rstudent(stack.lm) ~hatvalues(stack.lm) )
# Prob that observation with largest studentized residual is an outlier
2*(1- pt(max(abs(rstudent(stack.lm))), stack.lm$df - 1))
# Bonferonni 
.05/21
max(abs(rstudent(stack.lm)))
qt(1 - .025/21, 16)
