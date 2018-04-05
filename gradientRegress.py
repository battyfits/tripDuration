import numpy as np
import pandas as pd
import math
from scipy.stats import gaussian_kde
from scipy.stats import norm
import scipy.stats as stats
from datetime import timedelta
from sklearn.ensemble import AdaBoostRegressor
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.svm import SVR
from sklearn.cross_validation import cross_val_score
from sklearn.tree import DecisionTreeRegressor
from sklearn.metrics import accuracy_score
from sklearn import tree
import matplotlib.pyplot as plt # this is used for the plot the graph
import seaborn as sns # used for plot interactive graph. I like it most for plot
from sklearn.linear_model import LogisticRegression # to apply the Logistic regression
from sklearn.model_selection import train_test_split # to split the data into two parts
from sklearn.model_selection import GridSearchCV# for tuning parameter
from sklearn.ensemble import RandomForestClassifier # for random forest classifier
from sklearn.ensemble import RandomForestRegressor
from sklearn.naive_bayes import GaussianNB
from sklearn.neighbors import KNeighborsClassifier
from sklearn import svm # for Support Vector Machine
from sklearn import metrics # for the check the error and accuracy of the model
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.ensemble import GradientBoostingClassifier  #GBM algorithm
from sklearn import cross_validation, metrics   #Additional scklearn functions
from sklearn.grid_search import GridSearchCV   #Perforing grid search
from matplotlib.pylab import rcParams
AVG_EARTH_RADIUS = 6371
def rmse(y_true, y_pred):
    return np.sqrt(np.mean(np.square(y_pred - y_true), axis=-1))

def bearing_array(lat1, lng1, lat2, lng2):
    AVG_EARTH_RADIUS = 6371  # in km
    lng_delta_rad = np.radians(lng2 - lng1)
    (lat1, lng1, lat2, lng2) = map(np.radians, (lat1, lng1, lat2, lng2))
    y = np.sin(lng_delta_rad) * np.cos(lat2)
    x = np.cos(lat1) * np.sin(lat2) - np.sin(lat1) * np.cos(lat2) * np.cos(lng_delta_rad)

    return np.degrees(np.arctan2(y, x))
def haversine(point1, point2, miles=False):
    """ Calculate the great-circle distance between two points on the Earth surface.
    :input: two 2-tuples, containing the latitude and longitude of each point
    in decimal degrees.
    Example: haversine((45.7597, 4.8422), (48.8567, 2.3508))
    :output: Returns the distance bewteen the two points.
    The default unit is kilometers. Miles can be returned
    if the ``miles`` parameter is set to True.
    """
    # Unpack latitude/longitude
    lat1, lng1 = point1
    lat2, lng2 = point2

    # Convert all latitudes/longitudes from decimal degrees to radians
    lat1, lng1, lat2, lng2 = map(np.radians, (lat1, lng1, lat2, lng2))

    # Calculate haversine
    lat, lng= lat2 - lat1, lng2 - lng1
    d = np.sin(lat * 0.5) ** 2 + np.cos(lat1) * np.cos(lat2) * np.sin(lng * 0.5) ** 2
    h = 2 * AVG_EARTH_RADIUS * np.arcsin(np.sqrt(d))
    return h * 0.621371 if miles else h # in m/km

if __name__ == '__main__':
    plt.rcParams['figure.figsize'] = [10, 6]
    train = pd.read_csv("E:\\4thyear\\BigData\\xgb-train.csv")
    test = pd.read_csv("E:\\4thyear\\BigData\\xgb-test.csv")
    train['pickup_datetime'] = pd.to_datetime(train['pickup_datetime'])
    test['pickup_datetime'] = pd.to_datetime(train['pickup_datetime'])
    train['pickup_weekday'] = train['pickup_datetime'].dt.weekday
    train['pickup_month'] = train['pickup_datetime'].dt.month
    train['pickup_hour'] = train['pickup_datetime'].dt.hour

    test['pickup_weekday'] = test['pickup_datetime'].dt.weekday
    test['pickup_month'] = test['pickup_datetime'].dt.month
    test['pickup_hour'] = test['pickup_datetime'].dt.hour

    # These time periods are based on visuls below
    train['night_trip'] = [True if x < 7 else False for x in train['pickup_hour']]
    train['rush_hour'] = [True if 9 < x < 20 else False for x in train['pickup_hour']]
    train['weekday'] = [True if x < 5 else False for x in train['pickup_weekday']]
    test['night_trip'] = [True if x < 7 else False for x in test['pickup_hour']]
    test['rush_hour'] = [True if 9 < x < 20 else False for x in test['pickup_hour']]
    test['weekday'] = [True if x < 5 else False for x in test['pickup_weekday']]
    log_trip_duration = np.log(train['trip_duration'].values + 1)
    train['log_trip_duration'] = log_trip_duration
    #test.to_csv("E:\\4thyear\\BigData\\test-new.csv")
    #train.to_csv("E:\\4thyear\\BigData\\train-new.csv")
    DO_NOT_USE_FOR_TRAINING = [
        'id', 'pickup_datetime', 'jfk_dist_drop','jfk_dist_pick','lg_dist_drop','lg_dist_pick' ,'dropoff_datetime','speed','store_and_fwd_flag', 'trip_duration', 'pickup_date', 'log_trip_duration','date'
    ]
    new_df = train.drop([col for col in DO_NOT_USE_FOR_TRAINING if col in train], axis=1)
    new_df_test = test.drop([col for col in DO_NOT_USE_FOR_TRAINING if col in test], axis=1)

    #new_df['store_and_fwd_flag'] = 1 * new_df['store_and_fwd_flag'] == True
    #new_df_test['store_and_fwd_flag'] = 1 * new_df['store_and_fwd_flag'] == True
    new_df.columns == new_df_test.columns
    y = np.log(train['trip_duration'].values)
    train_attr = np.array(new_df)
    train_attr.shape
    train_x, val_x, train_y, val_y = train_test_split(train_attr, y, test_size=0.2)
    del train_attr
    print train_x
    TREE_REGRESSORS = [
        # These model are not tunned, default params in using
        DecisionTreeRegressor(), RandomForestRegressor()
    ]
    models = []
    for regressor in TREE_REGRESSORS:
        clf = regressor
        clf = clf.fit(train_x, train_y)
        models.append(clf)
    for model in models:
        # train_y is logged so rmse computes rmsle
        train_rmsle = rmse(train_y, model.predict(train_x))
        val_rmsle = rmse(val_y, model.predict(val_x))
        print('With model: {}\nTrain RMSLE: {}\nVal. RMSLE: {}'.format(model, train_rmsle, val_rmsle))
    test_attr = np.array(new_df_test)
    #train['expoTrain'] = np.exp(train['trip_duration'].values)-1
    model_dt, model_rf = models
    pred_dt = model_dt.predict(test_attr)
    pred_dt = np.exp(pred_dt)-1
    sns.set_style('whitegrid')
    sns.distplot(pred_dt, hist=False, color='red')
    sns.distplot(train['trip_duration'], hist=False, color='green')
    submission = pd.concat([test['id'], pd.DataFrame(pred_dt, columns=['trip_duration'])], axis=1)
    submission.to_csv('submission-dt.csv', index=False)
    pred_rf = model_rf.predict(test_attr)
    pred_rf = np.exp(pred_rf)-1
    submission = pd.concat([test['id'], pd.DataFrame(pred_rf, columns=['trip_duration'])], axis=1)
    submission.to_csv('submission-rf.csv', index=False)