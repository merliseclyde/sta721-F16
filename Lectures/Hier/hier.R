### Code for Running the Random Effect Models using R and R2WinBUGS
### to call WinBUGS
### Read in data sets and create variables as needed for models
 
fish <- read.table("fish", header=T)
boxplot(log(MERCURY) ~ STATION, data=fish)

fish$S = factor(fish$STATION)
fish.dif <- lm(log(MERCURY) ~ S*log(LENGTH), data=fish)
fish.par <- lm(log(MERCURY) ~ S + log(LENGTH), data=fish)
fish.com <- lm(log(MERCURY) ~ 1 + log(LENGTH), data=fish)

anova(fish.com, fish.par, fish.dif)

#library(lme4)

#fish.mer <- lmer(log(MERCURY) ~ 1 + log(LENGTH) + (1 + log(LENGTH) | STATION), fish)
#fish.mer <- lmer(log(MERCURY) ~ 1 + log(LENGTH) + (1|STATION) + (0 + log(LENGTH) | STATION), fish)
# hmm need to check out what is happening with model specification...

fish.fef <- lm(log(MERCURY) ~ log(LENGTH):as.factor(STATION) + as.factor(STATION) -1, data=fish)
attach(fish)
Y <- log(MERCURY)
X <- log(LENGTH)
station <- STATION + 1
river <- RIVER + 1
xbar =  mean(X)
n.station =  sapply(split(Y, station), length)
J = length(n.station)  # number of stations = 16
N = length(Y)




library(R2jags)
library(R2WinBUGS)

fishmodel <- function(){
  for (n in 1:N){
    muj[n] <- alpha[station[n]]+ beta[station[n]]*(X[n] - xbar)
    Y[n] ~ dnorm(muj[n], phi)
  }

  for (j in 1:J) {
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
    beta[j] ~ dnorm(beta.mu,  beta.phi)
  }
  phi ~ dgamma(.001, .001)
  sigma <- 1/sqrt(phi)

  alpha.mu ~ dnorm(0.0, 1.0E-6)%_%T(-5,5)
  alpha.sigma ~ dunif(0, 100)
  alpha.phi <-1/(alpha.sigma*alpha.sigma)
  
  beta.mu ~ dnorm(0.0, 1.0E-6)
  beta.phi <- pow(beta.sigma, -2)
  beta.sigma ~ dunif(0, 100)
}

# write the model code out to a file
model.file = "fishmodel.txt"
write.model(fishmodel, model.file)

## and let's take a look:
file.show(model.file)

data = list(J=J, N=N, Y=Y, X=X, xbar=xbar, station=station)  

inits = function() {
# based on ols  
list(beta=rnorm(J,1.8773, .0000341),
     alpha=rnorm(J,-6.9025+ 1.8773*xbar, .3110550),
     phi=1/.3943144^2,
     alpha.sigma=.3110550,
     beta.sigma=.0000341,
     alpha.mu= -6.9025,
     beta.mu = 1.8773)
}
parameters.to.save = c("alpha", "beta", "alpha.sigma", "beta.sigma", "sigma","alpha.mu", "beta.mu") # parameters to return from the MCMC

load.module("glm")  # use this to improve the glm estimation of SE's from output see post http://sourceforge.net/p/mcmc-jags/discussion/610037/thread/e19faa17

sim = jags(data, inits=NULL, parameters.to.save, model.file=model.file, n.chains=2, n.iter=10000)
sim = jags(data, inits=sim$BUGSoutput$last.values, parameters.to.save, model.file=model.file, n.chains=2, n.iter=10000)


print(sim)

plot(sim)


coef.fish = summary(fish.fef)$coef
beta = coef.fish[-(1:16), 1]
se.beta = coef.fish[-(1:16), 2]
df = fish.fef$df.residual
ci = cbind(beta + qt(.025, df)*se.beta, beta + qt(.975, df)*se.beta)
se.beta = coef.fish[-(1:16), 2]
df = fish.fef$df.residual
ci = cbind(beta + qt(.025, df)*se.beta, beta + qt(.975, df)*se.beta)
pi = t(apply(sim$BUGSoutput$sims.matrix[,19:34], 2,
  function(x){ HPDinterval(as.mcmc(x))}))
exppi = t(apply(sim$BUGSoutput$sims.matrix[,19:34], 2,
  function(x){ HPDinterval(as.mcmc(1.1^(x)))}))
post.beta = apply(sim$BUGSoutput$sims.matrix[,19:34],2, median) 
pi.overall =  HPDinterval(as.mcmc(sim$BUGSoutput$sims.matrix[,"beta.mu"]))
beta.overall = median(sim$BUGSoutput$sims.matrix[,"beta.mu"])
exppi.overall =  HPDinterval(as.mcmc(1.1^(sim$BUGSoutput$sims.matrix[,"beta.mu"])))

pdf("shrinkage.pdf")
matplot(0:15, ci, xlab="STATION", ylab=expression(beta), type="n", main="Shrinkage")
segments(0:15, ci[,1], 0:15, ci[,2], col=2)
points(0:15, beta, pch=23, col=2)

segments((0:15)+.25, pi[,1], (0:15)+.25, pi[,2], col="blue")
points((0:15)+.25, post.beta, pch=24, col="blue")
abline(h=beta.overall)
abline(h=pi.overall[1], lty=2)
abline(h=pi.overall[2], lty=2)
dev.off()

pdf("varcomp.pdf",height=6,width=12,horizontal=T)
par(mfrow=c(1,2))
hist(sim$BUGSoutput$sims.matrix[,"alpha.sigma"], prob=T,
     xlab=expression(sigma[alpha]), breaks=30,
     main="Posterior Distribution")

lines(density(sim$BUGSoutput$sims.matrix[,"alpha.sigma"]))
hist(sim$BUGSoutput$sims.matrix[,"beta.sigma"], prob=T,
     xlab=expression(sigma[beta]), breaks=30,
     main="Posterior Distribution")
lines(density(sim$BUGSoutput$sims.matrix[,"beta.sigma"])) 
dev.off()

pdf("increase.pdf")
matplot(0:15, 100*(exppi -1), xlab="STATION", ylab="Percent", type="n",
        main="Percent Increase in Mercury Concentration\n with a 10% Increase in Length")
segments((0:15), 100*(exppi[,1] -1), (0:15), 100*(exppi[,2] -1), col="blue")
points((0:15), 100*(1.1^(post.beta) -1), pch=24, col="blue")
abline(h=100*(1.1^(beta.overall) -1))
abline(h=100*(exppi.overall[1] -1), lty=2)
abline(h=100*(exppi.overall[2]-1), lty=2)
dev.off()

design = cbind(rep(1, 100), seq(log(25), log(65), length=100) - xbar)
coef = rbind(rnorm(1000, sim$BUGSoutput$sims.matrix[,"alpha.mu"],
                        sim$BUGSoutput$sims.matrix[,"alpha.sigma"]),
             rnorm(1000, sim$BUGSoutput$sims.matrix[,"beta.mu"],
                        sim$BUGSoutput$sims.matrix[,"beta.sigma"]))
  
errors = matrix(rnorm(1000*100, 0, sim$BUGSoutput$sims.matrix[,"sigma"]), nrow=100, ncol=1000, byrow=T)

pred.Mercury = exp(design %*% coef + errors)
pred.int = t(apply(pred.Mercury,1, function(x) {HPDinterval(as.mcmc(x))}))

pdf("pred.pdf")
matplot(exp(design[,2] + xbar)/2.54, pred.int, type="l", col=1, lty=2,
        xlab="Fish Length (inches)",
        ylab="Mercury Concentration (ppm)", main="Predicted Concentrations")
        )
lines(exp(design[,2] + xbar)/2.54, apply(pred.Mercury,1,median), col=2)
points(fish$LENGTH/2.54, fish$MERCURY, pch=22)
dev.off()

prob.lt.1 = apply(pred.Mercury<1,1,mean)
pdf("prob.pdf")
plot(lowess(exp(design[,2] + xbar)/2.54, prob.lt.1, f=.2), type="l", ylim=c(0,1),
     xlab="Fish Length (inches)", ylab="Probability",
     main="Probability Concentration is Less Than 1 ppm")
abline(h=.90)
dev.off()
