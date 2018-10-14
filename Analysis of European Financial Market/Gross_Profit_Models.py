# Importing the libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.metrics import accuracy_score
from sklearn.metrics import r2_score

# Importing the dataset
dataset = pd.read_csv('European_firms_data.csv')

#Feature selection
X1final=dataset[['Other operating expenses','Total shareh. funds & liab.','Total assets','Current Liabilities: loans','Other shareholders funds','Cash & cash equivalent','Current assets','Enterprise value','Shareholders funds','Current assets: stocks','Taxation','Costs of goods sold','Current liabilities','Other fixed assets','Tangible fixed assets','Other current assets','Financial expenses','Financial revenue','Current assets: debtors','Current Liabilities: creditors','Net current assets','Working capital','Other fixed assets','Sales','Other operating expenses','Shareholders funds','Tangible fixed assets','Costs of goods sold','Shareholder funds: capital', 'Gross profit']]


# Taking care of missing data
from sklearn.preprocessing import Imputer
imputer = Imputer(missing_values = 'NaN', strategy = 'mean', axis = 0)
imputer = imputer.fit(X1final.iloc[:, :])
X1final.iloc[:, :] = imputer.transform(X1final.iloc[:, :])

# Splitting the dataset into the Input and Output Feature sets
X = X1final.iloc[: ,: -1].values
y = X1final.iloc[:, -1].values

# Splitting the dataset into the Training set and Test set
from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.3, random_state = 0)

# Principal Component Analysis  
from sklearn.decomposition import PCA
from sklearn.tree import DecisionTreeRegressor
from sklearn.naive_bayes import GaussianNB

deci_score=[]
for no_of_components in range (3,8):
    pca = PCA(n_components=no_of_components).fit(X_train)
    X_train_reduced = pca.transform(X_train)
    X_test_reduced = pca.transform(X_test)
    
    # Fitting Decision Tree Regression to the dataset    
    clf1=DecisionTreeRegressor()
    clf1.fit(X_train_reduced,y_train)
    y_pred=clf1.predict(X_test_reduced)
    dec_score = r2_score(y_pred, y_test)
    deci_score.append(dec_score)
    print("Accuracy for", no_of_components, "components is:", dec_score)

get_ipython().run_line_magic('matplotlib', 'inline')
components=range(3,8)    
plt.plot(components,deci_score)
plt.xlabel('Components')
plt.ylabel('Accuracy score')


### Normalizing the data for KNN method 
from sklearn.preprocessing import MinMaxScaler
mms = MinMaxScaler()
X_norm = mms.fit_transform(X)

### KNN Neighbor Method 
from sklearn.neighbors import KNeighborsRegressor
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import KFold
from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import VotingClassifier
from sklearn.model_selection import cross_val_score

KNN= KNeighborsRegressor(n_neighbors=3)

#K-fold cross validation method on KNN regressor
kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(KNN, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))

#K-fold cross validation method on Decision Tree Regressor 
kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(clf1, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))

# Fitting Random Forest Regression to the dataset
from sklearn.ensemble import RandomForestRegressor
regressor = RandomForestRegressor(n_estimators = 10, random_state = 0)
fit = regressor.fit(X_train, y_train)

#K-fold cross validation method on Random Forest Regressor
kfold = KFold(n_splits=10, shuffle=True, random_state=24)
fold_accuracies = cross_val_score(regressor, X_norm, y, cv=kfold)
print("Cross-validation score:\n{}".format(fold_accuracies))
print("Average cross-validation score: {:.2f}".format(fold_accuracies.mean()))
