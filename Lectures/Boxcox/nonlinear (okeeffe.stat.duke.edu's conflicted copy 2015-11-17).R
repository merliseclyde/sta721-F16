x = c(2,4,6,8,10,24,28, 32)
y = c(1.63, 1.01, .73, .55, .41, .01, .06, .02)


conc.lm = lm(I(log(y) - log(30)) ~ x)

vhat = exp(-coef(conc.lm)[1])
khat = -coef(conc.lm)[2]

pdf("nonlinear.pdf", width=11,height=8.5)
par(mfrow=c(1,2))
plot(x, y)
lines(x, (30/vhat)*exp(-khat*x))

plot(fitted(conc.lm),residuals(conc.lm))
abline(h=0)
dev.off()

df = data.frame(y=y, x=x)
logconc.nlm = nls( log(y) ~ log((30/V)*exp(-k*x)), data=df, start=list(V=vhat, k=khat))
summary(logconc.nlm)

conc.nlm = nls( y ~ (30/V)*exp(-k*x), data=df, start=list(V=vhat, k=khat))
summary(conc.nlm)

pdf("nonlinear.pdf", width=11,height=8.5)
par(mfrow=c(1,2))
plot(x, y)
lines(x, exp(predict(logconc.nlm)), col=2, lty=2, lwd=1.5)
lines(x, predict(conc.nlm), col=4, lty=4, lwd=1.5)
legend(15, 1.5, legend=c("Log-Normal", "Normal"), col=c(2,4), lty=c(2,4 ))


plot(exp(predict(logconc.nlm)), y - exp(predict(logconc.nlm)), col=2, xlim=range(0, 1.5), ylim=c(-.10, .30), pch=15)
points(predict(conc.nlm),y - predict(conc.nlm), col=4, pch=16)
abline(h=0)
legend(0.01, .25, legend=c("Log-Normal", "Normal"), col=c(2,4), pch=c(15,16 ))
dev.off()

library(R2jags)
library(R2WinBUGS)


model= function() {

    v ~ dt(0, phi, 1)%_% T(0,)
    k ~ dt(0,phi, 1)%_% T(0,)
#    b ~ dwish(I, 1)
#    R ~ dwish(I, 1)
#    lambda ~ dgamma(1/2, 1/2)
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], phi)
        mu[i] <- D/v * exp(- k * x[i])
    }
    phi <- pow(sigma, -2)
    sigma ~ dt(0.00000E+00, 1, 1) %_% T(0.00000E+00, )
    cl <- v*k
    t.5 < - log(2)/k
}



model.file= "nlnmodel"
write.model(model, model.file)
data = list(y=y, x=x,  n=length(y), D=30)

out = jags(model.file=model.file, data=data, param=c("v","k","cl","t.5", "sigma"), n.iter=30000, n.burnin=5000)

pairs(out$BUGSoutput$sims.matrix)
out   # summary
acf(out$BUGSoutput$sims.matrix[,"v"])
