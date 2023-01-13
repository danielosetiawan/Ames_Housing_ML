import seaborn as sns
import numpy as np
import pandas as pd
import warnings
import itertools
import statsmodels.api as sm
from matplotlib import pyplot as plt
from pylab import rcParams
# from sklearn.model_selection import GridSearchCV, train_test_split

warnings.filterwarnings('ignore')
sns.set(rc = {'figure.figsize':(15, 7)})
sns.set_style('white')
pd.set_option('display.max_columns', 500)

housing = pd.read_csv('./../data/Ames_Housing_Price_Data.csv', index_col=0)
DateSold = pd.to_datetime(housing['YrSold'].astype(str) + '-' + 
                        housing['MoSold'].astype(str) + '-1').rename('DateSold')

df = housing[['SalePrice']].set_index(DateSold).groupby('DateSold').mean()

pdq = (1, 1, 1)
PDQM = (1, 2, 1, 12)


def all_neighborhoods():

	mod = sm.tsa.statespace.SARIMAX(df,
	                                order=pdq,
	                                seasonal_order=PDQM,
	                                enforce_stationarity=False,
	                                enforce_invertibility=False)
	result = mod.fit(disp=False)

	pred = result.get_prediction(start=pd.to_datetime('2009-09-01'), dynamic=True)
	pred_ci = pred.conf_int()
	ax = df.plot(label='Observed')
	pred.predicted_mean.plot(ax=ax, label='Prediction', alpha=.7, figsize=(14, 7))
	mse = ((df['2010-01-01':].squeeze() - pred.predicted_mean) ** 2).mean()
	ax.fill_between(pred_ci.index,
	                pred_ci.iloc[:, 0],
	                pred_ci.iloc[:, 1], color='k', alpha=.2)
	ax.set_xlabel('Date')
	ax.set_ylabel('Sale Price')
	plt.legend()
	plt.show()