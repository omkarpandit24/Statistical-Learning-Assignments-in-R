# -*- coding: utf-8 -*-
"""
Created on Sun May  6 15:28:01 2018

@author: gaurav
"""

# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.metrics import r2_score
from scipy.stats import spearmanr, pearsonr
from sklearn.metrics import mean_absolute_error
from sklearn.metrics import mean_squared_error
from sklearn.metrics import f1_score 
from sklearn.metrics import explained_variance_score
from sklearn.feature_selection import RFE
from sklearn.preprocessing import Imputer
import seaborn as sns
from sklearn.cross_validation import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.decomposition import PCA
from sklearn.tree import DecisionTreeRegressor
from sklearn.naive_bayes import GaussianNB
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.ensemble import VotingClassifier
from sklearn.preprocessing import MinMaxScaler
from sklearn.neighbors import KNeighborsRegressor

# Importing the dataset
dataset = pd.read_csv('European_firms_data.csv')

### Correlation and correlation matrix

print(dataset.corr())

corr = X1finalfinal.corr()
sns.heatmap(corr, 
            xticklabels=corr.columns.values,
            yticklabels=corr.columns.values)

# Splitting the dataset into the Training set and Test set

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)

# Fitting Random Forest Regression to the dataset

regressor = RandomForestRegressor(n_estimators = 10, random_state = 0)
fit = regressor.fit(X_train, y_train)

# Predicting a new result
y_pred = regressor.predict(X_test)

test_score = r2_score(y_test, y_pred)
print(test_score)


print("Features sorted by their score:")
print(sorted(zip(map(lambda x: round(x, 4), regressor.feature_importances_), X1final.columns), reverse=True))

#Feature selection
X1 = dataset.drop(["Country","Publicly quoted"], axis = 1)

X1=pd.get_dummies(X1,columns=["Company category"])
X1final=X1[['Number of employees', 'Operating revenue (Turnover)', 'Total assets', 'Costs of goods sold', 'Total shareh. funds & liab.', 'Current assets', 'Other current liabilities', 'Operat. rev. per employee', 'Total assets per employee', 'Financial revenue', 'Financial expenses', 'Costs of employees', 'Gross profit', 'Number of years', 'Depreciation', 'Added value', 'Other operating expenses', 'Costs of employees/oper. rev.(%)', 'Company category_LARGE','Company category_MEDIUM SIZED','Company category_SMALL','Company category_VERY LARGE']]
X = X1final.iloc[: , :-4].values
y = X1final.iloc[:, -4:].values

# Splitting the dataset into the Training set and Test set
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)


### Decision Tree Classifier
score_new =[]
component = range(1,7)
for comp in component:
    pca = PCA(n_components=comp).fit(X_train)
    X_train_reduced = pca.transform(X_train)
    X_test_reduced = pca.transform(X_test)

    #create classifier
    Dec_clf = DecisionTreeClassifier()
    Dec_clf.fit(X_train_reduced, y_train)
    y_pred_dec = Dec_clf.predict(X_test_reduced)
    print("Accuracy for", comp, "components is:", accuracy_score(y_pred_dec, y_test))
    score_dtc = accuracy_score(y_pred_dec, y_test)
    score_new.append(score_dtc)
    
plt.plot(component,score_new,"b-")
plt.xlabel('Components')
plt.ylabel('Accuracy score')


### Scaling
mms=MinMaxScaler()
mms.fit(X_train)
X_train_norm=mms.transform(X_train)
X_test_norm=mms.transform(X_test)

### K-Nearest Neighbor

score_knn_new=[]
for comp in component:
    pca = PCA(n_components=comp).fit(X_train_norm)
    X_train_reduced = pca.transform(X_train_norm)
    X_test_reduced = pca.transform(X_test_norm)

    #create classifier
    knn_clf = KNeighborsClassifier()
    knn_clf.fit(X_train_reduced, y_train)
    y_pred = knn_clf.predict(X_test_reduced)
    print("Accuracy for", comp, "components is:", accuracy_score(y_pred, y_test))
    score_knn = accuracy_score(y_pred, y_test)
    score_knn_new.append(score_knn)
    
plt.plot(component,score_knn_new,"r-")
plt.xlabel('Components')
plt.ylabel('Accuracy score')

### Random Forest

RF_Classifier = RandomForestClassifier(n_estimators = 10, random_state = 0)
fit_RF = RF_Classifier.fit(X_train, y_train)

kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(fit_RF, X_test_norm, y_test, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))

### DecisionTree

fold_accuracies_tree = cross_val_score(fit_RF, X_test_norm, y_pred_dec, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies_tree))
print("Average cross-validation score: {:.2f}".format(fold_accuracies_tree.mean()))

### KNeighbors
fold_accuracies_neighbor = cross_val_score(fit_RF, X_test_norm, y_pred, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies_neighbor))
print("Average cross-validation score: {:.2f}".format(fold_accuracies_neighbor.mean()))


