 X = rnorm(30, 0, 1)
 X = X - mean(X)
 Y = X* 3 + rnorm(30, 0, 1)
 Y = X* 3 + rnorm(30, 0, 1)
 Y = Y - mean(Y)

 SSX = sum(X^2)
 g = 5

ols = lm(Y ~ X -1)

#Call:
#lm(formula = Y ~ X - 1)
#
#Coefficients:
#    X  
3.247  

bols = lm(c(Y, 0) ~ c(X, sqrt(SSX/g)) -1)
                                        #
pdf("gprior.pdf", height=6, width=6)
plot(X, Y)
points(sqrt(SSX/g), 0, pch=15, col=2)

abline(0, ols$coef)
abline(0, bols$coef, lty=3, lwd=3, col=2)
ols$coef*g/(1 + g)

legend(1, -2, legend=c("OLS", "G-prior"), lty=c(1, 3), col=c(1,2))
dev.off()


library("R2jags")
library("R2WinBUGS")

model = function(){
  for (i in 1:n) {
      Y[i] ~ dnorm(X[i]*beta, phi)
  }

  beta ~ dnorm(0, SSX/(n*phi*lambda))
  phi ~ dgamma(.05, .05)

  lambda ~ dgamma(.5, .5)

}

write.model(model, "ZSmodel")
model.file="ZSmodel"
data = list(Y=Y, X=X, n =length(Y), SSX=sum(X^2) )

ZSout = jags(data,inits=NULL, parameters.to.save=c("beta", "lambda", "phi"), 
             model=model.file, n.iter=10000)

ZSout

pdf("ZSprior.pdf", height=6, width=6)
plot(X, Y)
points(sqrt(SSX/g), 0, pch=15, col=2)

abline(0, ols$coef)
abline(0, bols$coef, lty=3, lwd=3, col=2)
abline(0, ZSout$BUGSoutput$mean$beta, lty=2, lwd=3, col=3)
ols$coef*g/(1 + g)

legend(1, -2, legend=c("OLS", "G-prior", "ZS-prior"), lty=c(1, 3, 2), col=c(1,2,3))
dev.off()
