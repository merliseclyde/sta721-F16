clyde@dhcp-151.isds.duke.edu.269                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ation))
J = length(n.station)  # number of stations = 16
N = length(Y)

library(R2WinBUGS)

# better way without centered covariates
fishmodel3 <- function(){

  for (n in 1:N){
    muj[n] <- beta[station[n],1]+ beta[station[n],2]*X[station[n]]
    Y[n] ~ dnorm(muj[n], phi)
  }

  for (j in 1:J) {
    beta[j,1:2 ] ~ dmnorm(beta.mu[ ], beta.prec[ , ])
  }
  phi ~ dgamma(1.0E-6, 1.0E-6)
  sigma <- pow(phi, -.5)

  beta.mu[1:2] ~ dmnorm(zero[ ], prec.mu[ , ])

  beta0.sigma ~ dunif(0, 1000)
  beta1.sigma ~ dunif(0, 1000)

  beta.prec[1,1] <- pow(beta1.sigma, -2)
  beta.prec[2,2] <- pow(beta0.sigma, -2)
  beta.prec[1,2] <- 0
  beta.prec[2,1] <- 0
}

# write the model code out to a file
write.model(fishmodel3, "fishmodel3.txt")
model3.file = "/Users/clyde/Documents/sta290/Lectures/hier/fishmodel3.txt"

data3 = list(J=J, N=N, Y=Y, X=X,station=station, zero=rep(0,2), prec.mu = 1e-6*diag(1,ncol=2,nrow=2)) # include only variables in teh model

inits3 = function() {
list(beta=cbind(rnorm(J,-6.9025, .3110550), rnorm(J,1.8773, .0000341)),
     phi=1/.3943144^2,
     beta0.sigma=.3110550,
     beta1.sigma=.0000341,
     beta.mu=c(-6.9025,1.8773)
)
}
parameters.to.save3 = c("beta", "beta0.sigma", "beta1.sigma", "sigma", "beta.mu") 

# needed to run under WINE on MAC OSX/Linux
WINE <- "/Applications/Darwine/Wine.bundle/Contents/bin/wine"
WINEPATH <- "/Applications/Darwine/Wine.bundle/Contents/bin/winepath"
BUGS.DIR = "/users/clyde/.wine/drive_c/Program Files/WinBUGS14/"

sim = bugs(data3, inits3, parameters.to.save3, model.file=model3.file, n.chains=2, n.iter=50000, bugs.dir=BUGS.DIR, WINE=WINE, WINEPATH=WINEPATH, debug=T, DIC=F )

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
