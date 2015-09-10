x1 = -4:4
x2 = c(-2, 1, -1, 2, 0, 2, -1, 1, -2)
x3 = 3*x1  -2*x2
x4 = x2 - x1 + 4

Y = 1 + x1 + x2 + x3 + x4 + c(-.5, .5, .5, -.5, 0, .5, -.5, -.5, .5)

dev.set = data.frame(Y, x1, x2, x3, x4)

lm1234 = lm(Y ~ x1 + x2 + x3 + x4, data=dev.set)
coefficients(lm1234)
lm3412 = lm(Y ~ x3 + x4 + x1 + x2, data=dev.set)
coefficients(lm3412)

cbind(dev.set, predict(lm1234), predict(lm3412))

test.set = data.frame(
    x1 = c(3, 6, 6, 0, 0, 1),
    x2 = c(1, 2, 2, 0, 0, 2),
    x3 = c(7,14, 14,0, 0, 3),
    x4 = c(2, 4, 0, 4, 0, 4))

out = cbind(test.set,
    predict(lm1234, new=test.set),
    predict(lm3412, new=test.set))
out
library("estimability" )

outE = cbind(epredict(lm1234, test.set), epredict(lm3412, test.set))
