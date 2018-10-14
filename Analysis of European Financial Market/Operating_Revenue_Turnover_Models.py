# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
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
X1final=dataset[['Total shareh. funds & liab.','Taxation','Shareholders funds','Other operating expenses','Other fixed assets','Other current assets','Enterprise value','Current assets: debtors','Current Liabilities: creditors','Cash & cash equivalent','Sales','Total assets','Financial revenue','Costs of goods sold','Current liabilities','Tangible fixed assets','Other shareholders funds','Current assets','Financial expenses','Current assets: stocks','Current Liabilities: loans','Operating revenue (Turnover)']]


# Taking care of missing data

imputer = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0)
imputer = imputer.fit(X1final.iloc[:, :])
X1final.iloc[:, :] = imputer.transform(X1final.iloc[:, :])


X = X1final.iloc[: ,: -1].values
y = X1final.iloc[:, -1].values

# Splitting the dataset into the Training set and Test set

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)


# Fitting Decision Tree  to the dataset

deci_score=[]
for no_of_components in range (3,8):
    pca = PCA(n_components=no_of_components).fit(X_train)
    X_train_reduced = pca.transform(X_train)
    X_test_reduced = pca.transform(X_test)
    
    # Instantiate classifier
    clf1=DecisionTreeRegressor()
    clf1.fit(X_train_reduced,y_train)
    y_pred=clf1.predict(X_test_reduced)
    dec_score = r2_score(y_pred, y_test)
    deci_score.append(dec_score)
    print("Accuracy for", no_of_components, "components is:", dec_score)

components=range(3,8)    
plt.plot(components,deci_score)
plt.xlabel('Components')
plt.ylabel('Accuracy score')

### Cross Val score for Decision Tree

kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(clf1, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))

### Normalizing the data for KNN method 

mms = MinMaxScaler()
X_norm = mms.fit_transform(X)

### KNN Neighbor Method 

KNN= KNeighborsRegressor(n_neighbors=3)

kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(KNN, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))

### Random Forest cross val score

regressor = RandomForestRegressor(n_estimators = 10, random_state = 0)
fit = regressor.fit(X_train, y_train)

kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(regressor, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))
