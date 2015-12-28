library(MASS)
data(Animals) # load the data set animals from MASS
attach(Animals)
pdf("brain.pdf")
plot(brain ~ body, data=Animals, xlab="Body Weight (kg)", ylab="Brain Weight (g)", main="Original Units")
dev.off()
identify(Animals$body, Animals$brain, rownames(Animals))



out = identify(body, brain, rownames(Animals))
# save plot via file menu



brains.lm = lm(brain ~ body, data=Animals)
pdf("brains-resid.pdf")
par(mfrow=c(2,2))
plot(brains.lm)
dev.off()

row.names(Animals) == "Brachiosaurus"  # case 26

plot(lm(brain ~ body, data=Animals, subset= c(-26)))
plot(lm(brain ~ body, data=Animals, subset= c(-26, -6)))
plot(lm(brain ~ body, data=Animals, subset= c(-26, -6, -16)))

# no more dinosaurs, but no elephants are  the problems
pdf("brains-BC.pdf")
boxcox(brains.lm)
dev.off()

pdf("brain-resid-logY.pdf")
par(mfrow=c(2,2))
plot(lm(log(brain) ~ body, data=Animals))
dev.off()

pdf("brains-tran.pdf")
par(mfrow=c(2,2))
logbrain.lm = lm(log(brain) ~ log(body), data=Animals)
plot(logbrain.lm)
dev.off()

#use R^2 to compare the two (as long as same response)

pt(-max(abs(rstudent(logbrain.lm)))), 26)/2

#compare to Bonferroni adjusted alpha
.05/28

#outlier test fails - masking!

pdf("brain-log.pdf")
plot(brain ~ body, data=Animals, xlab="Body Weight (kg)", ylab="Brain Weight (g)", log="xy", main="Logarithmic Scale")
text(body[out], brain[out], row.names(Animals)[out])
dev.off()


logbrains.nodino.lm = lm(log(brain) ~ log(body) + I(row.names(Animals) == "Triceratops") +  I(row.names(Animals) == "Brachiosaurus") + I(row.names(Animals) == "Dipliodocus"), data=Animals)

library(xtable)
xtable(
anova(logbrains.nodino.lm, logbrain.lm)
)
xtable(summary(logbrains.nodino.lm))


library(BAS)
brains.bas = bas.lm(log(brain) ~ log(body) + diag(28), data=Animals, prior="hyper-g-n", a=3, modelprior=beta.binomial(1,28), method="MCMC", n.models=2^17, MCMC.it=2^18)
# check for convergence
pdf("bas-animals-conv.pdf")
plot(brains.bas$probne0, brains.bas$probs.MCMC)
dev.off()
pdf("brains-image.pdf")
image(brains.bas)
dev.off()

rownames(Animals)[c(6, 14, 16, 26)]  
