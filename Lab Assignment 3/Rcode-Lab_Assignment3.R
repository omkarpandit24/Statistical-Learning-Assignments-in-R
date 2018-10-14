# Assignment 3
# NAME: Omkar Pandit
# UT ID: oap338

####################### Question 2 ###########################
#Reading the data and summary statistics

#First load the csv file  
setwd("C:/Omkar/UT AUSTIN SEM 2/SAL/Assignment 2")
redwine.df<-read.csv(paste("redwine.csv",sep=""))
summary(redwine.df)

# summary statistics 

library(psych)
describe(redwine.df)

#Building the new variable final_quality 

redwine.df$final_quality <- with(ifelse(quality>mean(quality), "1", "0"), data=redwine.df)

#Question 2. Part a> Fit a logistic regression model that uses all predictors (except quality) to predict final_quality.

attach(redwine.df)
set.seed(1)
mlogr <- glm(as.factor(final_quality) ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine.df, family="binomial")
summary(mlogr)


#Question 2. Part b> Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps:

#Splitting data 
train <- sample(dim(redwine.df)[1], dim(redwine.df)[1] / 2)

#Logistic Regression on Training Data
mlogreg <- glm(as.factor(final_quality) ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine.df, family="binomial", subset = train)
summary(mlogreg)

#Prediction of final quality
predict <- predict(mlogreg, newdata = redwine.df[-train,], type = "response")
prediction.glm <- rep("0", length(predict))
prediction.glm[predict > 0.5] <- "1"

#Validation set error

mean(prediction.glm != redwine.df[-train, ]$final_quality)

####################### Question 3 ###########################
#Question 3. Part a> Write a function, boot_fn(), that takes as input the redwinedataset as well as an index of the observations, and that outputsthe coefficient estimates for each predictorin the multiplelogistic regression model.

set.seed(1)
attach(redwine.df)

#logistic regression 
mlogrgmodel <- glm(as.factor(final_quality) ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine.df, family="binomial")
summary(mlogrgmodel)


#boot_fn() function
boot_fn <- function(data, index) {
  fit <- glm(as.factor(final_quality) ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine.df, family="binomial", subset = index)
  return (coef(fit))
}

#Question 3. Part b> Use the boot() function together with your boot_fn() function toestimate the standard errors of the logistic regression coefficientsfor each predictor. 
#boot function
library(boot)
boot(redwine.df, boot_fn, 1000)

#Question 3. Part c> Comment on the estimated standard errors obtained using theglm() function and using your bootstrap function.One way to get these value in python is use of statsmodellibrary 
#We can find the standard errors using both the methods
#The estimated standard errors obtained using glm and bootstrap function are almost same
#Consider standard errors of t3 using both methods we get 0.48 and 0.52
#So based on my observations I find these values very similar to each other.