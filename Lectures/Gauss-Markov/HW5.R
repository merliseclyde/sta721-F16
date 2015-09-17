library(lasso2)
data(Prostate)
library(estimability)

# Create dummy variables for the Prostate Data 
Prostate$D6 = as.numeric(Prostate$gleason==6)
Prostate$D7 = as.numeric( Prostate$gleason==7)
Prostate$D8 = as.numeric( Prostate$gleason==8)
Prostate$D9 = as.numeric( Prostate$gleason==9)
Prostate$I = rep(1, nrow(Prostate))

FullModel <- lm( lpsa ~  D6+ D7+ D8 + D9  +I - 1, data = Prostate)
FullModel # We notice that we have an NA for D9 


newdf = data.frame(diag(1, 5))
colnames(newdf) = c("D6", "D7", "D8", "D9", "I")
newdf[6, ] = c(1, -1, 0,0,0)
newdf[7, ] = c(0,0,0,1,1)

epredict(FullModel, newdf)

estimability = function(lmobj,lambda, tol=10^-13) {
  X = model.matrix(lmobj)
  eigen.X = eigen(t(X)%*%X)
  use = eigen.X$values < tol
  LamX = t(eigen.X$vectors[, use]) %*% lambda
  P = eigen.X$vectors[,use]%*% LamX
  P[which(P <1e-10)]=0
  is.nest = rep(1, nrow(P)) %*% abs(P)
  is.nest[ which(is.nest > 0 )] = NA
  beta = coef(lmobj); beta[is.na(beta)] = 0
  est = t(lambda) %*% beta
  est[is.na(is.nest)] = NA
  sigma = summary(lmobj)$sigma
  SE = sqrt( diag(t(LamX) %*% (1/eigen.X$values[use]) %*% LamX))
  return (list(est, is.nest))
}
estimable = function (lmobj, v) {
  X = model.matrix(lmobj)
  svdXt = svd(t(X))
  non0 = svdXt$d > 10^-13
  v - svdXt$u[, non0] %*% (t(svdXt$u[,non0]) %*% v)
}

estimable(FullModel, t(as.matrix(newdf)))
F1 <- lm( lpsa ~  D6+ D7+ D8 +  D9, data = Prostate)
estimable(F1, c(0, 1, 0,0,0))
# function works