# Assignment 2
# NAME: Omkar Pandit
# UT ID: oap338

setwd("C:/Omkar/UT AUSTIN SEM 2/SAL/Assignment 2")
redwine.df<-read.csv(paste("redwine.csv",sep=""))
summary(redwine.df)

library(psych)
describe(redwine.df)

####################### Question 2 ###########################

#Question 2. Part a>Produce some numerical and graphical summaries of the Red Wine data. 
#Do there appear to be any patterns?
str(redwine.df)
attach(redwine.df)
#Observations from correlation and summary tables,
#Alcohol affects quality(high correlation) and, alcohol is affected by density which is affected by different 
#chemicals like fixed acidity, residual sugar, chlorides and these chemicals are further corelated
#with other chemicals.
summary_data <- summary(redwine.df)
summary_data
corr <- cor(redwine.df)
corr

#scatter plot
pairs(redwine.df, main = "Scatter plot - Redwine Data")

###Question 3. Part a>Splitting the train and test set into 80% and 20%

#Question 2. Part b>Create a binary variable, final_quality, that contains a 1 if quality 
#contains a value above its mean, and a 0 if quality contains a value below its mean. Use 
#the full data set to perform a logistic regression with final_quality as the response and 
#other variables as predictors (besides the original quality variable). Provide a summary 
#of your obtained results. (summary function in R/statsmodel in python). 
#Do any of the predictors appear to be statistically significant? If so, which ones?

#New binary variable final_quality has been created.
redwine.df$final_quality <- with(ifelse(quality>mean(quality), "1", "0"), data=redwine.df)

# Data has been splitted in 80% and 20% after adding final_quality
set.seed(1)
rows <- sample(x=nrow(redwine.df), size=.80*nrow(redwine.df))
trainset <- redwine.df[rows, ]
testset <- redwine.df[-rows, ]

View(redwine.df)

#Performed logistic regression on the training dataset.
lr.fit <- glm(as.factor(trainset$final_quality) ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=trainset, family="binomial")
summary(lr.fit)

#Among the all available predictors alcohol,sulphates,total.sulfur.dioxide, volatile.acidity showed the 
#lower p values, so, they are more statistically significant and have the bigger impact
#on the dependent variable (final_quality).
lr.probs <- predict(lr.fit, testset, type="response")
lr.pred <- ifelse(lr.probs>0.5, "1", "0")

#Question 2. Part b>Compute the confusion matrix and overall fraction of correct predictions. 
#Explain what the confusion matrix is telling you about the types of mistakes made by logistic regression.

# Confusion matricx is one of the metric used to define the performance of Logistic Regression:
# Observed values are TP = 143, TN = 105, FP = 27 and FN = 45 -->
# This signifies that the logistic regression correctly predicts only 
# only 105 negative values (out of a total of 105+27 total negative values) so 27 were mistake and only 143 true positive values (out of 143+45 total true value) and 45 were mistakes.

Confusion_matrix<-table(testset$final_quality, lr.pred)
Confusion_matrix

###KNN Matrix
#Question 2. Part c>Perform KNN on the full data set, with several values of K, in order to predict final_quality.
#What test errors do you obtain? Which value of K seems to perform the best on this data set?

library(class)

variables <- which(names(trainset)%in%c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol"))

set.seed(1)
acc <- data.frame("k"=1:10, acc=NA)
for(k in 1:10){
  knn.pred <- knn(train=trainset[, variables], test=testset[, variables], cl=trainset$final_quality, k=k)
  
  acc$acc[k]= round(sum(knn.pred!=testset$final_quality)/nrow(testset)*100,2)
}
#The highest accuracy has been achieved for K=1 with a 
#test error of '25.62' seems to perform the best on this data set.

acc


####################### Question 3 ###########################
#Question 3. Part a> Split the data into a training set (80%) and a test set (20%).
# Data has been splitted in 80% and 20% after adding final_quality
#set.seed(1)
#rows <- sample(x=nrow(redwine.df), size=.80*nrow(redwine.df))
#trainset <- redwine.df[rows, ]
#testset <- redwine.df[-rows, ]



#Question 3. Part b>Perform LDA on the training data in order to predict final_quality using
#other variables as predictors. What is the test error of the model obtained?

library(MASS)
ldafit <- lda(final_quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=trainset)
ldapred <- predict(ldafit, testset)
table(testset$quality, ldapred$class)

# test-error obtained for LDA: 23.125%
round(sum(ldapred$class!=testset$final_quality)/nrow(testset)*100,3)

#Question 3. Part c>Perform QDA on the training data in order to predict final_quality using
#other variables as predictors. What is the test error of the model obtained?

qdafit <- qda(final_quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=trainset)
qdapred <- predict(qdafit, testset)
table(testset$final_quality, qdapred$class)

# test-error obtained for QDA: 24.375%
round(sum(qdapred$class!=testset$final_quality)/nrow(testset)*100,3)

