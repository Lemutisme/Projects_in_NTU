import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
import warnings

train = pd.read_csv('D:/PyCharmProjects/train.csv', parse_dates=['datetime'])
test = pd.read_csv('D:/PyCharmProjects/test.csv', parse_dates=['datetime'])

full = pd.concat([train, test])

full['date'] = full['datetime'].dt.date
full['month'] = full['datetime'].dt.month
full['hour'] = full['datetime'].dt.hour
full['dayname'] = full['datetime'].dt.weekday
full['year'] = full['datetime'].dt.year

full.reset_index(inplace=True)

plt.figure(figsize=(16, 9))
sns.set_theme()
plt.subplot(2, 2, 1)
sns.lineplot(data=full, x='date', y='count', color='green')
plt.subplot(2, 2, 2)
sns.lineplot(data=full, x='hour', y='count', marker='.', color='red')
plt.subplot(2, 2, 3)
sns.lineplot(data=full, x='dayname', y='count', estimator=np.mean)
plt.subplot(2, 2, 4)
sns.barplot(data=full, x='month', y='count', estimator=np.sum, palette=sns.color_palette("Paired"))
plt.show()


fig, (ax2, ax1) = plt.subplots(1, 2, figsize=(10, 6))
full.holiday.value_counts().plot.pie(autopct='%.2f%%', ax=ax1, legend=True)
sns.boxplot(data=full, y='count', x='holiday', ax=ax2, color='green')
ax1.set_title('percent of holiday and non-holiday data')
ax2.set_title('average demands for bike sharing system ')
plt.tight_layout()
plt.show()

fig,(ax1,ax2)=plt.subplots(1,2,figsize=(10,4))
full.groupby('season')['count'].sum().plot.bar(ax=ax2,color='red')
sns.boxplot(x=train.season,y=train['count'],ax=ax1,color='blue')
plt.tight_layout()
ax2.set(title='total demands for each season')
ax2.set_ylabel('total demand')
ax1.set_title('boxplot of each season')
plt.tight_layout()
plt.show()


full.weather.unique()

plt.figure()
plt.subplot()
sns.barplot(data=full,x='weather',y='count',color='green')
plt.show()

full.weather.value_counts()
full.weather.value_counts().plot.pie(shadow=True,autopct='%.2f%%',legend=True)
plt.show()

full.loc[full.weather==4,'weather']=3#reset the No.4 weather conditon to No.3
g = sns.FacetGrid(full,col='weather',height=4)
g.map(sns.histplot,'windspeed',stat='probability',color='yellow')
plt.show()

g = sns.FacetGrid(full,col='weather',height=4)
g.map(sns.histplot,'humidity',stat='probability',color='red')
plt.show()


g = sns.FacetGrid(full,col='weather',height=4)
g.map(sns.histplot,'temp',stat='probability',color='blue')
plt.show()

g = sns.FacetGrid(full,col='season')
g.map(sns.scatterplot,'temp','atemp',color='green')
plt.show()

full[['temp','atemp']].corr()

fig, (ax,ax2,ax3) = plt.subplots(3,1,sharex=True,figsize=(14,9))
sns.lineplot(data=train, x='temp', y='count',ax=ax2,color='red')
sns.histplot(train['temp'],ax=ax,color='green')
sns.histplot(full['temp'],ax=ax3,color='blue')
ax2.set_title('plot of temp and count')
ax.set_title('plot of train temp distribution')
ax3.set_title('plot of full temp distribution')
plt.show()

plt.figure(figsize=(16,9))
plt.subplot(2,2,1)
sns.distplot(full['humidity'],color='yellow')
plt.subplot(2,2,2)
sns.distplot(full['windspeed'],color='blue')
plt.subplot(2,2,3)
sns.lineplot(x='humidity',y='count',data=train,color='purple')
plt.subplot(2,2,4)
sns.lineplot(x='windspeed',y='count',data=train,color='red')
plt.show()


g = sns.FacetGrid(full, col='season', row='year')
g.map(sns.histplot, 'windspeed', stat='probability',color='red')
g.set_ylabels('probability')
plt.show()

import pylab
import calendar
import numpy as np
import pandas as pd
import seaborn as sns
from scipy import stats
from datetime import datetime
import matplotlib.pyplot as plt
import warnings
from sklearn.linear_model import LogisticRegression
from sklearn.svm import LinearSVC, SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.naive_bayes import GaussianNB

# regression
from sklearn.linear_model import LinearRegression, Ridge, Lasso, RidgeCV
from sklearn.ensemble import RandomForestRegressor, BaggingRegressor, GradientBoostingRegressor, AdaBoostRegressor
from sklearn.svm import SVR
from sklearn.neighbors import KNeighborsRegressor

# model selection
from sklearn.model_selection import train_test_split, cross_validate
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV

# evaluation metrics
from sklearn.metrics import mean_squared_log_error, mean_squared_error, r2_score, mean_absolute_error  # for regression
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score  # for classification
from sklearn.preprocessing import StandardScaler

df = pd.read_csv('D:/PyCharmProjects/train.csv')
ddf = pd.read_csv('D:/PyCharmProjects/test.csv')

df.drop(columns=['casual', 'registered'])
# df.info()
# df.describe()
# df.shape

# missing value
df.isnull().sum()

# scaling
num_feature = ['temp', 'atemp', 'humidity', 'windspeed']
scaler = StandardScaler()
df[num_feature] = scaler.fit_transform(df[num_feature].to_numpy())

# datetime
df["hour"] = [t.hour for t in pd.DatetimeIndex(df.datetime)]
# day represents day in a week not in a month
df["day"] = [t.dayofweek for t in pd.DatetimeIndex(df.datetime)]
df["month"] = [t.month for t in pd.DatetimeIndex(df.datetime)]
df['year'] = [t.year for t in pd.DatetimeIndex(df.datetime)]
df.year = df.year.replace({2011: 0, 2012: 1})

ddf["hour"] = [t.hour for t in pd.DatetimeIndex(ddf.datetime)]
# day represents day in a week not in a month
ddf["day"] = [t.dayofweek for t in pd.DatetimeIndex(ddf.datetime)]
ddf["month"] = [t.month for t in pd.DatetimeIndex(ddf.datetime)]
ddf['year'] = [t.year for t in pd.DatetimeIndex(ddf.datetime)]
ddf.year = ddf.year.replace({2011: 0, 2012: 1})

# multi-class
cat_features = ['season', 'weather', 'hour', 'day', 'month']
df = pd.get_dummies(df, columns=cat_features)
ddf = pd.get_dummies(ddf, columns=cat_features)

df = df.drop(columns=['datetime', 'casual', 'registered'])
ddf = ddf.drop(columns='datetime')

X = df.drop('count', axis=1)
y = df['count']
x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)

import time
from sklearn.metrics import mean_squared_log_error
from catboost import CatBoostRegressor
from sklearn.neural_network import MLPRegressor
from sklearn.ensemble import GradientBoostingRegressor
from xgboost import XGBRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import make_scorer
from sklearn.model_selection import cross_val_score

method = make_scorer(mean_squared_log_error)

model_dict = {'RandomForestRegressor': RandomForestRegressor(),
              'DecisionTreeRegressor': DecisionTreeRegressor(),
              'BaggingRegressor': BaggingRegressor(),
              # 'SVR':SVR(),
              'KNeighborsRegressor': KNeighborsRegressor(),
              'MLPRegressor': MLPRegressor(max_iter=200),
              'GradientBoostingRegressor': GradientBoostingRegressor(),
              'XGBRegressor': XGBRegressor(),
              'CatBoostRegressor': CatBoostRegressor(),
              }


def relu(x):
    if x < 0:
        x = 0
    return x


def batch_regression(X_tarin, Y_train, X_test, Y_test, verbose=True):
    df_results = pd.DataFrame(data=np.zeros(shape=(len(model_dict), 3)),
                              columns=['classifier', 'test_score', 'training_time'])
    count = 0
    for key, reg in model_dict.items():
        t_start = time.perf_counter()
        temp = reg.fit(X_tarin, Y_train)
        t_end = time.perf_counter()
        t_diff = t_end - t_start
        pred = temp.predict(X_test)
        pred = [relu(x) for x in pred]
        test_score = np.sqrt(mean_squared_log_error(Y_test, pred))
        # train_score = classifier.score(X_train, Y_train)
        df_results.loc[count, 'classifier'] = key
        df_results.loc[count, 'test_score'] = test_score
        df_results.loc[count, 'training_time'] = t_diff
        if verbose:
            print("trained {c} in {f:.2f} s".format(c=key, f=t_diff))
        count += 1
    return df_results


res = batch_regression(x_train, y_train, x_test, y_test)
print(res.sort_values(by='test_score', ascending=True))

from sklearn.model_selection import KFold
from sklearn.metrics import mean_squared_error


def my_cros(data, method):
    df_results = pd.DataFrame(data=np.zeros(shape=(len(model_dict), 3)),
                              columns=['classifier', 'test_score', 'training_time'])
    count = 0
    for key, reg in model_dict.items():
        templist = []
        kf = KFold(n_splits=5)
        t_start = time.perf_counter()
        for train, test in kf.split(data):
            X = data.drop(columns='count')
            y = data['count']
            x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=42)

            temp = reg.fit(x_train, y_train)

            pred = temp.predict(x_test)
            pred = [relu(x) for x in pred]
            templist.append(np.sqrt(method(y_test, pred)))
            test_score = np.mean(templist)
        t_end = time.perf_counter()
        t_diff = t_end - t_start
        df_results.loc[count, 'classifier'] = key
        df_results.loc[count, 'test_score'] = test_score
        df_results.loc[count, 'training_time'] = t_diff
        print("trained {c} in {f:.2f} s".format(c=key, f=t_diff))
        count += 1
    return df_results


myres = my_cros(df, mean_squared_log_error)
print(myres.sort_values(by='test_score', ascending=True))
myres1 = my_cros(df, mean_squared_error)
print(myres1.sort_values(by='test_score', ascending=True))


def cv_regression(X_tarin, Y_train, verbose=True):
    df_results = pd.DataFrame(data=np.zeros(shape=(len(model_dict), 3)),
                              columns=['classifier', 'test_score', 'training_time'])
    count = 0
    for key, reg in model_dict.items():
        t_start = time.perf_counter()
        scores = cross_val_score(reg, X_tarin, Y_train, scoring=method, cv=5)
        t_end = time.perf_counter()
        t_diff = t_end - t_start
        cross_val_score_mean = scores.mean()
        # train_score = classifier.score(X_train, Y_train)
        df_results.loc[count, 'classifier'] = key
        df_results.loc[count, 'test_score'] = cross_val_score_mean
        df_results.loc[count, 'training_time'] = t_diff
        if verbose:
            print("trained {c} in {f:.2f} s".format(c=key, f=t_diff))
        count += 1
    return df_results


cv_res = cv_regression(X, y)
print(cv_res.sort_values(by='test_score', ascending=True))

reg = RandomForestRegressor()
reg = reg.fit(df.drop(columns='count'), df['count'])
pred = reg.predict(ddf)
sub = pd.DataFrame(pred)
sub.to_csv('sub.csv')

dist_tree = {
    'CatBoostRegressor': CatBoostRegressor(),
    'AdaBoostRegressor': AdaBoostRegressor(),
    'RandomForestRegressor': RandomForestRegressor(),
    'GradientBoostingRegressor': GradientBoostingRegressor(),
    'DecisionTreeRegressor': DecisionTreeRegressor(),
}


def tree_plot_feature(x_train, y_train):
    for name, clf in dist_tree.items():
        clf.fit(x_train, y_train)
        clf.feature_importances_
        feature_importances = pd.DataFrame({'feature_name': x_train.columns,
                                            'importance_value': clf.feature_importances_})
        sns.barplot(data=feature_importances.sort_values(by='importance_value',
                                                         ascending=False).iloc[0:20, :],
                    x='importance_value', y='feature_name', palette='Blues_r_d')
        plt.title('{} Feature Importances '.format(name))
        plt.show()


tree_plot_feature(x_train, y_train)
