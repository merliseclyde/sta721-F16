n = 50
sigma = 2.5
betatrue = c(4,2,0,0,0,-1,0,1.5, 0,0,0,1,0,.5,0,0,0,0,-1,1,4)
#          int|    X1                            | X2     |X3          
# set the random seed so that we can replicate results.
set.seed(42)

fname=rep("df",100)

# create 100 datasets
for (i in 1:100) {
  Z = matrix(rnorm(n*10, 0, 1), ncol=10, nrow=n)
  X1 = cbind(Z, (Z[,1:5] %*% c(.3, .5, .7, .9, 1.1) %*% t(rep(1,5)) +
             matrix(rnorm(n*5, 0, 1), ncol=5, nrow=n)))
  X2 <- matrix(rnorm(n*4,0,1), ncol=4, nrow=n)
  X3 <- X2[,4]+rnorm(n,0,sd=0.1)
  X <- cbind(X1,X2,X3)
  X = sweep(X, 2, apply(X,2, mean))  # subtract off the column means
  Y = 4 + X %*% betatrue[-1] + rnorm(n,0,sigma)  # does not have a column of ones for the intercept
  df = data.frame(Y, X)
  fname[i] = paste("df", as.character(i), sep="")
  save(df, file=fname[i])
}

MSE.OLS  = rep(0,100)

for( i in 1:100) {
  rm(df)
  load(fname[i])
  nk.ols = lm(Y ~ ., data=df)
  coef.ols = coef(nk.ols)
  MSE.OLS[i] = sum((betatrue - coef.ols)^2)
#  print(c(i, MSE.OLS[i]))
}

mean(MSE.OLS)
