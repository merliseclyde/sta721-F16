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


### Bayesian outlier Analysis 
stack.lm <- lm(stack.loss ~ ., qr=T, data=stackloss)
library(mvtnorm)    
source("bayes-outliers.R")
k = qnorm(.5 + .5*.95^(1/21))
Bout <- Bayes.outlier.prob(stack.lm,k=k)

plot(Bout$prob.outlier, ylab="Posterior Probability of Outlier", xlab="Case", type="h")

indices = outer(1:21, 1:21, FUN=paste)
cbind(indices[Bout$prob.pair.outlier > .0027^2],
      round(Bout$prob.pair.outlier[Bout$prob.pair.outlier > .0027^2],
            digits=6))

# dear energetic student:
# please write a function to replicate the graph in CB Figure 2.

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
