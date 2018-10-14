# Assignment 5
# NAME: Omkar Pandit
# UT ID: oap338

####################### Question 1 ###########################
#Question 1.In this exercise, we will use carseat dataset and seek to predict Sales using regression 
#trees and related approaches, treating the response as a quantitative variable.

setwd("C:/Omkar/UT AUSTIN SEM 2/SAL/Assignment 5")
carseat<-read.csv(paste("carseat.csv",sep=""))
summary(carseat)

#Question 1. Part a> Split the data set into a training set and a test set.
set.seed(1)
train <- sample(1:nrow(carseat), nrow(carseat) / 2)
carseat.train <- carseat[train, ]
carseat.test <- carseat[-train, ]

#Question 1. Part b> Fit a regression tree to the training set. Plot the tree, and interpret the results.
#What test MSE do you obtain?

library(tree)
tree.carseat <- tree(Sales ~ ., data = carseat.train)
summary(tree.carseat)

plot(tree.carseat)
text(tree.carseat, pretty = 0)

ypred <- predict(tree.carseat, newdata = carseat.test)
mean((ypred - carseat.test$Sales)^2)
#We found Test MSE is about 4.15.

#Question 1. Part c> Use cross-validation in order to determine the optimal level of tree complexity.
#Does pruning the tree improve the test MSE?

cross_validation.carseat <- cv.tree(tree.carseat)
plot(cross_validation.carseat$size, cross_validation.carseat$dev, type = "b")
tree.minimum <- which.min(cross_validation.carseat$dev)
points(tree.minimum, cross_validation.carseat$dev[tree.minimum], col = "red", cex = 2, pch = 20)

#In this case, the tree of size 11 is selected by cross-validation. We now prune the tree to obtain the 11-node tree.

prune.carseat <- prune.tree(tree.carseat, best = 11)
plot(prune.carseat)
text(prune.carseat, pretty = 0)

ypred <- predict(prune.carseat, newdata = carseat.test)
mean((ypred - carseat.test$Sales)^2)
#We may see that pruning the tree increases the Test MSE to 4.63.

#Question 1. Part d> Use the bagging approach in order to analyze this data. What test MSE do you obtain?
library(randomForest)
bagging.carseat <- randomForest(Sales ~ ., data = carseat.train, mtry = 10, ntree = 500, importance = TRUE)
ypred.bagging <- predict(bagging.carseat, newdata = carseat.test)
mean((ypred.bagging - carseat.test$Sales)^2)

#We observed that bagging decreases the Test MSE from 4.63 to 2.72.


#Question 1. Part e> Use random forests to analyze this data. What test MSE do you obtain?
randomforest.carseat <- randomForest(Sales ~ ., data = carseat.train, mtry = 3, ntree = 500, importance = TRUE)
ypred.randomforest <- predict(randomforest.carseat, newdata = carseat.test)
mean((ypred.randomforest - carseat.test$Sales)^2)
#We have a Test MSE of 3.59 in case of random forest.


#Question 1. Part f>Also, report the important features in your random forest. One can do this by using 
#importance() function in R or feature_importance_ on a fitted model in sklearn in python.
#Describe the effect of m, the number of variables considered at each split, on the error rate obtained.

#In this case, with m=???p, we have a Test MSE of 3.59.

importance(randomforest.carseat)

#We may conclude that, in this case also, "Price" and "ShelveLoc" are the two most important variables based 
#on node purity values.



####################### Question 2 ###########################
#Question 2. In this problem, you will use support vector approaches in order to predict whether 
#a given red wine is high-quality or low-quality based on the red-wine data set.


setwd("C:/Omkar/UT AUSTIN SEM 2/SAL/Assignment 5")
redwine<-read.csv(paste("redwine.csv",sep=""))
summary(redwine)

#Question 2. Part a>Create a binary variable that takes on a 1 with quality above the mean wine quality, 
#and 0 for quality below the mean.

quality_class <- ifelse(redwine$quality > mean(redwine$quality), 1, 0)
redwine$qualitylevel <- as.factor(quality_class)

#Question 2. Part b>Fit a support vector classifier to the data with various values of penalty (cost in R, C in python),
#in order to predict whether a given red wine is high-quality or low-quality. 
#Report the cross-validation errors associated with different values of this parameter. Comment on your findings.

library(sos)
findFn('tune', maxPages = 1)


costs=data.frame(cost=seq(0.05,100,length.out = 15))               
svm.tune=tune(svm,qualitylevel ~ .,data=redwine,ranges=costs,kernel='linear') 
svm.tune
# A cost of 0.05 seems to performing best in this case.

#Question 2. Part c>Now repeat (b), this time using SVMs with radial and polynomial basis kernels, with three different 
#values of gamma and degree and penalty each (check cost in R, C in python for penalty). What's happening 
#and what might be causing the change in cross validation error. Summary and comment on your choices of 
#parameters and the corresponding cross validation errors in a table.


# 3 different degree values have been used - 2, 3, 4
set.seed(1)
tune.out <- tune(svm, qualitylevel ~ ., data = redwine, kernel = "polynomial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), degree = c(2, 3, 4)))
summary(tune.out)
#For a polynomial kernel, the lowest cross-validation error is obtained for a degree of 3 and a cost of 5.

# Different values of gamma used are - 0.01, 0.1, 1, 5, 10, 100
set.seed(1)
tune.out <- tune(svm, qualitylevel ~ ., data = redwine, kernel = "radial", ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#For a radial kernel, the lowest cross-validation error is obtained for a gamma of 0.01 and a cost of 5.
















































