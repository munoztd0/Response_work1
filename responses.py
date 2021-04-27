import os
import pandas as pd
import numpy as np
from plotnine import *
import sklearn 
#%matplotlib inline

import warnings
warnings.filterwarnings('ignore')


print(os.getcwd().strip('"\''))



data = pd.read_csv("data/data.csv")
data.head()

var_string = list(filter(lambda x: x.endswith('ses1'), data.columns))
for i in range(len(var_string)):
    var_string[i] = var_string[i][: -5]

var_string  #getting the name of the variables


data_long = pd.wide_to_long(data, stubnames=var_string, i=['id', 'intervention'], j='session',
                 sep='_', suffix=r'\w+').reset_index()

data_long['intervention'] = data_long['intervention'].astype(object)

#wide to long format
data_long.head()

data_longer = pd.melt(data_long, id_vars=['id', 'intervention', 'session'], value_vars=var_string,var_name='var', value_name='measure')
data_longer.head()

#density plots
ggplot(data_longer) + geom_density(aes( x='measure')) + \
facet_wrap(['var'], scales = "free") + \
labs(title = "Density per variable")+ \
  ylab("Density") + xlab('Value')

#corrplot
corr = data_long[data_long.columns[3:]].corr()
corr.style.background_gradient(cmap='coolwarm')

#boxplot
ggplot(data_longer)+ \
  geom_boxplot(aes(x='intervention', y='measure'))+ \
    facet_wrap(['var'], scales = "free") + \
  scale_x_discrete(labels=["Placebo", "Control"]) + \
    labs(title = "Boxplot by group")+ \
    ylab("predictors") + xlab('')


#scaling
scaler = preprocessing.StandardScaler() # from sklearn
data_long[var_string] = scaler.fit_transform(data_long[var_string])
data_long.head()




