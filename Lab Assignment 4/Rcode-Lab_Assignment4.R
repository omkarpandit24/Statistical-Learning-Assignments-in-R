# Assignment 4
# NAME: Omkar Pandit
# UT ID: oap338

####################### Question 2 ###########################
#Generating simulated data, and will then use this data to perform forward and backward feature selection, and lasso model.

#Question 2. Part a> Generate a predictor X of length n = 100, as well as a noise vector ?? of length n = 100.
# generate a response vector Y of length n = 100 according to the model Y = ??0 + ??1X + ??2X2 + ??3X3 + ??.
set.seed(1)
x <- rnorm(100)
epslon <- rnorm(100)

b0 <- 4
b1 <- 1.5
b2 <- 1
b3 <- -2.5
y <- b0 + b1 * x + b2 * x^2 + b3 * x^3 + epslon

#Question 2. Part b> Using forward stepwise selection and also using backwards stepwise selection to choose the best model containing the predictors X, X2, . . . , X6.
library(leaps)
data <- data.frame(y = y, x = x)
regfwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data = data, nvmax = 10, method = "forward")
reg.summary.fwd <- summary(regfwd)


par(mfrow = c(2, 2))
plot(reg.summary.fwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary.fwd$cp), reg.summary.fwd$cp[which.min(reg.summary.fwd$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary.fwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.fwd$bic), reg.summary.fwd$bic[which.min(reg.summary.fwd$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary.fwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary.fwd$adjr2), reg.summary.fwd$adjr2[which.max(reg.summary.fwd$adjr2)], col = "red", cex = 2, pch = 20)
mtext("Plots of C_p, BIC and adjusted R^2 for forward stepwise selection", side = 3, line = -2, outer = TRUE)

coef(regfwd, which.max(reg.summary.fwd$adjr2))

#Based on the results found using C_p, BIC, Adjusted R^2 for forward stepwise selection,  
#I will pick 4-variables model from C_p, 3-variables model from BIC, and 4-variables model from adjusted R^2. 
#So finalizing adjusted R^2 as a metric I come up with 4-Variables x, x^2, x^3, x^5 model.

#backward stepwise selection
regbwd <- regsubsets(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data = data, nvmax = 10, method = "backward")
reg.summary.bwd <- summary(regbwd)

par(mfrow = c(2, 2))
plot(reg.summary.bwd$cp, xlab = "Number of variables", ylab = "C_p", type = "l")
points(which.min(reg.summary.bwd$cp), reg.summary.bwd$cp[which.min(reg.summary.bwd$cp)], col = "red", cex = 2, pch = 20)
plot(reg.summary.bwd$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
points(which.min(reg.summary.bwd$bic), reg.summary.bwd$bic[which.min(reg.summary.bwd$bic)], col = "red", cex = 2, pch = 20)
plot(reg.summary.bwd$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
points(which.max(reg.summary.bwd$adjr2), reg.summary.bwd$adjr2[which.max(reg.summary.bwd$adjr2)], col = "red", cex = 2, pch = 20)
mtext("Plots of C_p, BIC and adjusted R^2 for backward stepwise selection", side = 3, line = -2, outer = TRUE)

coef(regbwd, which.max(reg.summary.bwd$adjr2))

#Based on the results found using C_p, BIC, Adjusted R^2 for backward stepwise selection,  
#I will pick 4-variables model from C_p, 3-variables model from BIC, and 4-variables model from adjusted R^2. 
#So finalizing adjusted R^2 as a metric I come up with 4-Variables x, x^2, x^3, x^5 model.

#Hence, both forward stepwise and backward stepwise selction methods select 4 variables i.e. x, x^2, x^3, x^5 model.

#Question 2. Part c> Now fit a lasso model to the simulated data, again using X, X2, . . . , X6 as predictors.
par(mfrow = c(1,1))
library(glmnet)
xmatrix <- model.matrix(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6), data = data)[, -1]
lasso <- cv.glmnet(xmatrix, y, alpha = 1)
plot(lasso)

bestlambda <- lasso$lambda.min

#Now refitting lasso model using minimum possible value for lambda.
fit.lasso <- glmnet(xmatrix, y, alpha = 1)
predict(fit.lasso, s = bestlambda, type = "coefficients")[1:7, ]
#Hence, lasso method selects 4 variables i.e. x, x^2, x^3, x^6 model.
