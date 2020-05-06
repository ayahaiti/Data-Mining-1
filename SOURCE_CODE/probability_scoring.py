import pandas as pd
import numpy as np
from matplotlib import pyplot
from sklearn import preprocessing
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_curve, brier_score_loss, log_loss
from sklearn.model_selection import train_test_split

# la probabilité qu'une maison/appartement/duplex.. soit  loué s'il contient au minimum
# 2 lits et ait d'un prix de location inférieur ou égal à 1500 dollars(hypothèse d'un
# logement à charges moyennes selon la moyenne des prix)

# imported the dataset reduced to some tuples from different states
df = pd.read_csv('C:/DESKTOP/Classes/data_mining/dataset.csv')
# dropping null values
df = df.dropna(how='any', axis=0)

# defining the 2 class columns of : number of beds and the price
X = df.drop(['id', 'lat', 'type', 'baths', 'cats_allowed', 'comes_furnished', 'dogs_allowed', 'electric_vehicle_charge',
             'laundry_options', 'smoking_allowed', 'sqfeet', 'wheelchair_access', 'parking_options', 'region', 'state', 'long'], axis=1)
condition = [
    (df['beds'] >= 2) & (df['price'] <= 1500)
]
choice = [1]

# defining new column that contains either 0 or 1( either the house is or will be rented or not)
df['is_rented'] = np.select(condition, choice, default=0)
y = df.is_rented
X = preprocessing.normalize(X)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.30)
model = LogisticRegression(n_jobs=1)
model.fit(X_train, y_train)

# predicting the new probabilities for the test data
probs = model.predict_proba(X_test)
probs = probs[:, 1]
fpr, tpr, thresholds = roc_curve(y_test, probs)
# other methods to apply instead of roc_curve are:
# loss = log_loss(y_test, probs)
# loss = brier_score_loss(y_test, probs)

pyplot.plot([0, 1], [0, 1], linestyle='--')
pyplot.plot(fpr, tpr)
pyplot.show()
