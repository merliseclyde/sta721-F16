# frequentists analysis
library(MASS)
library(car)
data(stackloss)
# variables   Air.Flow Water.Temp Acid.Conc. stack.loss

pdf("stackloss.pdf")
pairs(stackloss) #, diag.panel=panel.hist,panel=panel.smooth)
dev.off()

pdf("stackloss-avp.pdf")
stack.lm <- lm(stack.loss ~ ., data=stackloss)
avPlots(stack.lm)
dev.off()

pdf("stackloss-resid.pdf")
par(mfrow=c(2,2))
plot(stack.lm)
dev.off()

attach(stackloss)
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


stack21.lm = lm(stack.loss ~ ., data=stackloss, subset = -c(21))

par(mfrow=c(2,2))
plot(stack21.lm)
2*(1- pt(max(abs(rstudent(stack21.lm))), stack21.lm$df - 1))


stack21.4.lm = lm(stack.loss ~ ., data=stackloss, subset = -c(21,4))
2*(1- pt(max(abs(rstudent(stack21.lm))), stack21.lm$df - 1))



attach(stackloss)
plot(Water.Temp, stack.loss)
identify(Water.Temp, stack.loss)
points(Water.Temp, stack.loss, col=(1:21) != 21)

plot(Air.Flow, stack.loss)
identify(Air.Flow, stack.loss)


plot(Acid.Conc., stack.loss)

identify(Acid.Conc., stack.loss)
points(Acid.Conc.[21], stack.loss[21], col=2)
