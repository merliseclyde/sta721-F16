bodyfat = read.table("bodyfat.txt", header=T)

# BMA

library(BMA)
n = nrow(bodyfat)
        
bodyfat.MC3= MC3.REG(bodyfat$Bodyfat, as.matrix(bodyfat$Abdomen),num.its=10000,  outliers=TRUE)

summary(bodyfat.MC3)

## lets try in BAS
library(BAS)
bodyfat.wout = cbind(bodyfat[, c("Bodyfat", "Abdomen")], diag(n))

bodyfat.bas = bas.lm(Bodyfat ~ ., data=bodyfat.wout, prior="hyper-g-n", a=3, method="MCMC", n.models=2^18, MCMC.it=2^20, modelprior=beta.binomial(1,1))
pdf("bodyfat-bas.pdf")
image(bodyfat.bas)
dev.off()

plot(bodyfat.bas$probne0, bodyfat.bas$probs.MCMC)
### R interface to JAGS:
library(R2jags)
library(R2WinBUGS)
# Create a data list with inputs for WinBugs/Jags

bf.data = list(Y = bodyfat$Bodyfat, X=bodyfat$Abdomen)
bf.data$n = length(bf.data$Y)
bf.data$Xbar = mean(bf.data$X)
# define a function that returns the Model 

rr.model = function() {
  df <- 9
  
  for (i in 1:n) {
    mu[i] <- alpha0 + alpha1*(X[i] - Xbar)
    lambda[i] ~ dgamma(df/2, df/2)
    prec[i] <- phi*lambda[i]
    Y[i] ~ dnorm(mu[i], prec[i])
  }
  phi ~ dgamma(1.0E-6, 1.0E-6)
  alpha0 ~ dnorm(0, 1.0E-6)
  alpha1 ~ dnorm(0,1.0E-6)
  beta0 <- alpha0 - alpha1*Xbar
  beta1 <- alpha1
  sigma <- pow(phi, -.5)
  mu34 <- beta0 + beta1*2.54*34  #mean for a man w/ a 34 in waist
  y34 ~ dt(mu34,phi, df)   # integrate out lambda_34 
}

# create a function that provides intial values for WinBUGS
rr.inits = function() {
    bf.lm <- lm(bf.data$Y ~ I(bf.data$X - bf.data$Xbar))
    coefs = coef(bf.lm)
    alpha1=coefs[2]
    alpha0 = coefs[1]
    phi = (1/summary(bf.lm)$sigma)^2
 lambda = rep(1, bf.data$n)
return(list(alpha0=alpha0, alpha1 = alpha1, phi=phi, lambda=lambda))
}

# a list of all the parameters to save

parameters = c("beta0", "beta1", "sigma", "mu34", "y34", "lambda[39]")

rr.model.file = paste(getwd(),"rr-model.txt", sep="/")
write.model(rr.model, rr.model.file)


bf.sim = jags(bf.data, inits=rr.inits, parameters, model.file=rr.model.file, n.chains=2, n.iter=20000)

bf.bugs = as.mcmc(bf.sim$BUGSoutput$sims.matrix)  # create an MCMC object 

plot(bf.sim)
summary(bf.sim)  # names of objects in bf.sim
bf.sim  # print gives summary
par(mfrow=c(1,1))
quantile(bf.sim$sims.matrix[,"beta1"], c(.025, .5, .975))
HPDinterval(as.mcmc(bf.bugs[,"beta1"]))
HPDinterval(as.mcmc(bf.bugs[,"mu34"]))
HPDinterval(as.mcmc(bf.bugs[,"y34"]))

hist(bf.bugs[,"beta1"], prob=T, xlab=expression(beta[1]),
     main="Posterior Distribution")
lines(density(bf.bugs[,"beta1"]))
densplot(bf.bugs[,"beta1"])
par(mfrow=c(1,2))
hist(bf.bugs[,"mu34"], prob=T, xlab=expression(mu),
     main="Posterior of Expected Bodyfat\n for Men with 34 inch Waist")
lines(density(bf.bugs[,"mu34"]))

hist(bf.bugs[,"y34"], prob=T, xlab="Abdominal Circumference",
     main="Predictive Distribution of Bodyfat\n for Men with 34 inch Waist ")
lines(density(bf.bugs[,"y34"]))

pdf("lambda39.pdf")
hist(bf.bugs[,"lambda[39]"], prob=T, xlab=expression(lambda[39]),
     main="Posterior Distribution")
lines(density(bf.bugs[,"lambda[39]"]))
# add prior density
lines(seq(0.01, 1.2, length=100), dgamma(seq(0.01, 1.2, length=100), 9/2,rate=9/2))
dev.off()


# compare to Normal results
# Note trick to subtract off value of X so that the intercept is now the mean
# of Y where we want to make a prediction (x = 2.54*34)
# the function I() "inhibits" the usual model formula
# interpretation of "-" and "*"

bodyfat.lm = lm(Bodyfat ~ I(Abdomen - 2.54*34), data=bodyfat)
coef = summary(bodyfat.lm)$coef
c(coef[2,1] - coef[2,2]*qt(.975,250), coef[2,1] + coef[2,2]*qt(.975,250))
# interval for mu
c(coef[1,1] - coef[1,2]*qt(.975,250), coef[1,1] + coef[1,2]*qt(.975,250))
# interval for y
std.pred = sqrt(coef[1,2]^2 + (summary(bodyfat.lm)$sigma)^2)
c(coef[1,1] - std.pred*qt(.975,250), coef[1,1] + std.pred*qt(.975,250))

# compare to Normal results w/out case 39
bodyfat.lm = lm(Bodyfat ~ I(Abdomen - 2.54*34), subset=c(-39), data=bodyfat)
coef = summary(bodyfat.lm)$coef

c(coef[2,1] - coef[2,2]*qt(.975,250), coef[2,1] + coef[2,2]*qt(.975,250))
# interval for mu
c(coef[1,1] - coef[1,2]*qt(.975,250), coef[1,1] + coef[1,2]*qt(.975,250))
# interval for y
std.pred = sqrt(coef[1,2]^2 + (summary(bodyfat.lm)$sigma)^2)
c(coef[1,1] - std.pred*qt(.975,250), coef[1,1] + std.pred*qt(.975,250))

# or use confint and predict function :-)


#Influence function


g = function (x, df) {
(1 + df) * x / (df + x^2)
}
pdf("influence.pdf")
eps=seq(-100,100, length=100000); plot(eps, g(eps, 9), type="l")
abline(v=c(-3,3))
abline(h=0)
dev.off()
