### Code for Running the Random Effect Models using R and R2WinBUGS
### to call WinBUGS
### Read in data sets and create variables as needed for models
 
fish <- read.table("../../datasets/fish", header=T)
fish.fef <- lm(log(MERCURY) ~ log(LENGTH):as.factor(STATION) + as.factor(STATION) -1, data=fish)


#library(lme4)

#fish.mer <- lmer(log(MERCURY) ~ 1 + log(LENGTH) + (1 + log(LENGTH) | STATION), fish)
#fish.mer <- lmer(log(MERCURY) ~ 1 + log(LENGTH) + (1|STATION) + (0 + log(LENGTH) | STATION), fish)
# hmm need to check out what is happening with model specification...

attach(fish)
Y <- log(MERCURY)
X <- log(LENGTH)
station <- STATION + 1
river <- RIVER + 1
xbar =  mean(X)
n.station =  sapply(split(Y, station), length)


boxplot(split(Y, station))
J = length(n.station)  # number of stations = 16
N = length(Y)

library(R2WinBUGS)

fishmodel <- function(){
  for (n in 1:N){
    muj[n] <- alpha[station[n]] +  beta[station[n]]*X[n]
    Y[n] ~ dnorm(muj[n], phi)
  }

  for (j in 1:J) {
    alpha[j] ~ dnorm(alpha.mu, alpha.phi)
    beta[j]  ~ dnorm(beta.mu,  beta.phi)
  }
  phi ~ dgamma(.001, .001)
  sigma <- 1/sqrt(phi)

  alpha.mu ~ dnorm(0.0, 1.0E-6)
  alpha.sigma ~ dunif(0, 100)
  alpha.phi <-1/(alpha.sigma*alpha.sigma)
  
  beta.mu ~ dnorm(0.0, 1.0E-6)
  beta.phi <- pow(beta.sigma, -2)
  beta.sigma ~ dunif(0, 100)
}

# write the model code out to a file
model.file = paste(getwd(), "fishmodel.txt", sep="/")
write.model(fishmodel, model.file)

## and let's take a look:
file.show("fishmodel.txt")

data = list(J=J, N=N, Y=Y, X=X, station=station)  # include only variables in teh model

inits = function() {
list(beta=rnorm(J,1.8773, .0000341),
     alpha=rnorm(J,-6.9025, .3110550),
     phi=1/.3943144^2,
     alpha.sigma=.3110550,
     beta.sigma=.0000341,
     alpha.mu= -6.9025,
     beta.mu = 1.8773)
}
parameters.to.save = c("alpha", "beta", "alpha.sigma", "beta.sigma", "sigma","alpha.mu", "beta.mu") 

# needed to run under WINE on MAC OSX/Linux
WINE <- "/Applications/Darwine/Wine.bundle/Contents/bin/wine"
WINEPATH <- "/Applications/Darwine/Wine.bundle/Contents/bin/winepath"
BUGS.DIR = "/users/mclyde/.wine/drive_c/Program Files/WinBUGS14/"

sim2 = bugs(data, inits, parameters.to.save, model.file=model.file, n.chains=2, n.iter=50000, bugs.dir=BUGS.DIR, WINE=WINE, WINEPATH=WINEPATH, debug=T, DIC=F )


print(sim2)
plot(sim2)

out2 = read.coda.interactive()
#can use as.mcmc to coerce matrices to mcmc objects


plot(out2, ask=F) # traceplots & density plots

codamenu()

effectiveSize(out2)
# geweke fails ???
gelman.plot(out2)
gelman.diag(out2)
raftery.diag(out2)
heidel.diag(out2)

autocorr.plot(out2)
crosscorr.plot(out2)
