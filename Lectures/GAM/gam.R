library(ISLR)  # for Wage data
library(mgcv)
data(Wage)
summary(Wage)
plot(wage ~ year + age + sex + race +education, data=Wage)
pdf("resid-slr.pdf")
par(mfrow=c(2,2))
wage.slr = lm(wage ~ age, data=Wage)
plot(wage.slr, ask=F)
dev.off()

summary(lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage))
wage.poly = lm(wage ~ poly(age,4),data=Wage)
summary(wage.poly)

order= order(Wage$age)
poly.pred = predict(wage.poly, int="con")
pdf("poly.pdf")
plot(wage ~ age, data=Wage, main="4th Order Polynomial")
lines(Wage$age[order], poly.pred[order,"fit"], col=2)
lines(Wage$age[order], poly.pred[order,"upr"], col=2)
lines(Wage$age[order], poly.pred[order,"lwr"], col=2)
dev.off()

bs.poly = lm(wage ~ ns(age, df=6), data=Wage)

wage.gam = gam(wage ~ s(age), data=Wage)
summary(wage.gam)
pdf("gam-age.pdf")
plot(wage.gam, rug=T, shade=T)
dev.off()

wage.gam2 = gam(wage ~ s(year,k=7) + s(age), data=Wage)
summary(wage.gam2)
pdf("gam-age-year.pdf", width=11,height=8)
par(mfrow=c(1,2))
plot(wage.gam2, rug=T, ask=F, shade=T, resid=T)
dev.off()

wage.gam3 = gam(wage ~ s(year,k=7) + s(age) + education, data=Wage)
pdf("edu-term.pdf", height=6,width=11)
par(mfrow=c(1,2))
termplot(wage.gam3, se=T, rug=T, ask=F, col.se=2, shade=T)
plot(residuals(wage.gam3) ~ Wage$education); abline(h=0)
dev.off()

wage.gam4 = gam(log(wage) ~ s(year,k=7) + s(age,education, bs="fs"), data=Wage)

pdf("wage-int.pdf",height=6,width=11)
par(mfrow=c(1,2))
plot(wage.gam4, shade=T, resid=T, se=T, rug=T)
dev.off()

pdf("resid-wage4.pdf")
plot(residuals(wage.gam4) ~ Wage$education, xlab="Education", ylab="Residuals"); abline(h=0)
dev.off()

