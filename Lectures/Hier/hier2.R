### Code for Running the Random Effect Models using R and R2WinBUGS
### to call WinBUGS
### Read in data sets and create variables as needed for models
 
fish <- read.table("../../datasets/fish", header=T)
library(lme4)
lmer(log(MERCURY) ~ log(LENGTH) + (1 | STATION) + (0 + log(LENGTH) | STATION), fish)

attach(fish)
Y <- log(MERCURY)
X <- log(LENGTH)
station <- STATION + 1
river <- RIVER + 1
xbar.station =  sapply(split(X, station), mean)
n.station =  sapply(split(Y, station), length)


boxplot(split(Y, station))
J = length(n.station)  # number of stations = 16
N = length(Y)

library(R2WinBUGS)

# bad model parameterization
fishmodel2 <- function(){
 for (n in 1:N){
    muj[n] <- alpha[station[n]]+ beta[station[n]]*X[station[n]]
    Y[n] ~ dnorm(muj[n], phi)
    }

    for (j in 1:J) {
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
    beta[j] ~ dnorm(beta.mu,  beta.phi)
  }
    phi ~ dgamma(1.0E-6, 1.0E-6)
    sigma <- pow(phi, -.5)

    alpha.mu ~ dnorm(0.0, 1.0E-6)
    alpha.phi <- pow(alpha.sigma, -2)
    alpha.sigma ~ dunif(0, 1000)

    beta.mu ~ dnorm(0.0, 1.0E-6)
    beta.phi <- pow(beta.sigma, -2)
    beta.sigma ~ dunif(0, 1000)
}

# write the model code out to a file
write.model(fishmodel2, "fishmodel2.txt")
model2.file = "/Users/clyde/Documents/sta290/Lectures/randomeffects/fishmodel2.txt"

data2 = list(J=J, N=N, Y=Y, X=X,station=station)  # include only variables in teh model

inits2 = function() {
list(beta=rnorm(J,1.8773, .0000341),
     alpha=rnorm(J,-6.9025, .3110550),
     phi=1/.3943144^2,
     alpha.sigma=.3110550,
     beta.sigma=.0000341,
     alpha.mu=-6.9025,
     beta.mu = 1.8773)
}
parameters.to.save = c("alpha", "beta", "alpha.sigma", "beta.sigma", "sigma","alpha.mu", "beta.mu") 

# needed to run under WINE on MAC OSX/Linux
WINE <- "/Applications/Darwine/Wine.bundle/Contents/bin/wine"
WINEPATH <- "/Applications/Darwine/Wine.bundle/Contents/bin/winepath"
BUGS.DIR = "/users/clyde/.wine/drive_c/Program Files/WinBUGS14/"

sim = bugs(data2, inits2, parameters.to.save, model.file=model2.file, n.chains=2, n.iter=50000, bugs.dir=BUGS.DIR, WINE=WINE, WINEPATH=WINEPATH, debug=T, DIC=F )

print(sim)
plot(sim)

acf(sim$sims.matrix[,"mu"])
acf(sim$sims.matrix[,"rho"])
acf(sim$sims.matrix[,"sigma"])
acf(sim$sims.matrix[,"sigma.mu"])

postscript("hsb4-2.ps")
par(mfrow=c(2,2))
plot(density(sim$sims.matrix[,"mu"]), xlab=expression(mu), ylab="Density", main="")
plot(density(sim$sims.matrix[,"rho"]), xlab=expression(rho), ylab="Density", main="")
plot(density(sim$sims.matrix[,"sigma"]), xlab=expression(sigma^2), ylab="Density", main="")
plot(density(sim$sims.matrix[,"sigma.mu"], adjust=1.1), xlab=expression(sigma[mu]^2), ylab="Density", main="")

dev.off()
