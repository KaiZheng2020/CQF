# CQF M4 python Lab 1
# import whole library
import sklearn
from sklearn.datasets import fetch_california_housing
from sklearn.model_selection import train_test_split

# loading dataset
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
# data preprocessing 
from sklearn.feature_selection import SelectKBest

# selecting the estimator
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.cluster import k_means

# selecting the metrics 
from sklearn.metrics import accuracy_score
from sklearn.metrics import mean_squared_error

import pandas as pd
import numpy as np
import warnings 
warnings.filterwarnings('ignore')

# 课件中导入的库有的没有用
# =============================================================================
# 
# =============================================================================


from sklearn.datasets import load_boston
from sklearn.metrics import mean_squared_error
# ,r

boston_data=load_boston()
# load_boston referenced a class boston from sklearn, type is utils.Bunch, it is 
# essentially a dic with data, description,feature_names, filename, and target as class members 
# 波士顿房价数据是个典型的自定义数据类，包含很多不同类型的字段，表示相关信息
# print(boston_data.keys())


X=boston_data['data']
# 亦可使用x=boston_data.data
# X数组表示相关数据，需要在一定时候转换成dataframe数据框的形式，并添加行列标签，可在变量列表中查看

y=boston_data['target']
# y是观测值，或者说是要预测的目标，课件中写label不对


# print (max(y), min(y))
# print(boston_data['DESCR'])

boston=pd.DataFrame(boston_data['data'],columns=boston_data['feature_names'])
# # print(boston.head())
# # print(boston.shape)
# # print(boston.isnull().sum())
# # isnull 返回与当前dataframe同样的一个数据框，键值为是否是NA

# print(boston.describe().T)
# dataframe中.T表示数据的转置

# =============================================================================
# 
# =============================================================================


from sklearn.preprocessing import StandardScaler

# create and fit scaler
scaler=StandardScaler()
scaler.fit(X)
# fit Compute the mean and std to be used for later scaling.
# 创建收缩实例


Xt=scaler.transform(X)
# Xmean=X.mean(axis=0).T

# # axis=0 表示沿着纵轴从上到下；axis=1表示沿着横轴从左到右
# # Xvar=X.var(axis=0)

stats1=np.vstack((X.mean(axis=0),X.var(axis=0),Xt.mean(axis=0),Xt.var(axis=0)))
# np.vstack垂直罗列所有参数
stats=stats1.T

feature_names=boston_data['feature_names']
columns=['unscaled mean','unscaled variance','scaled mean','scaled variance']
df=pd.DataFrame(stats,index=feature_names,columns=columns)

# print(df)

from sklearn.linear_model import LinearRegression
# create model and train/fit
model=LinearRegression()
model.fit(X,y)
y_pred=model.predict(X)

# print(y_pred[:10])

model_score=model.score(X,y)
#model.score 返回回归的拟合优度

# print(f'R^2 for the linear regression: {model.score(X,y):0.6}')
#格式显示，大括号内部显示变量及显示小数点位数


# =============================================================================
# 
# =============================================================================

from sklearn.ensemble import GradientBoostingRegressor
model2=GradientBoostingRegressor()
# model2用以区别以上的model
model2.fit(X,y)
y_pred2=model2.predict(X)

# print(y_pred2[:10])
# print(f'R^2 for the gradient boosting regressor: {model2.score(X, y):0.4}')


# =============================================================================
# 
# =============================================================================


from sklearn.pipeline import Pipeline 

# Pipeline可以将许多算法模型串联起来，比如将特征提取、归一化、分类组织在一起形成一个典型的机器学习问题工作流。主要带来两点好处：
# 直接调用fit和predict方法来对pipeline中的所有算法模型进行训练和预测
# 可以结合grid search对参数进行选择
# When encapsulating the entire workflow through a Pipeline object, we avoid manually calling the fitting, transformations, and predictions steps.

pipe=Pipeline([('scaler',StandardScaler()),('regressor',LinearRegression())])

# print (pipe.named_steps)

pipe.fit(X,y)
y_pred=pipe.predict(X)
# print(y_pred[:10])
# print(f'R^2:{model2.score(X,y):0.4}')


# =============================================================================
# 
# =============================================================================





from sklearn.pipeline import FeatureUnion
from sklearn.decomposition import PCA
from sklearn.feature_selection import f_regression, SelectKBest
# 特征联合：特征值的不同种类变换
pca_pipe=Pipeline([('scaler',StandardScaler()),('dim_red',PCA(n_components=4))])
union=FeatureUnion([('pca_pipe',pca_pipe),('selector',SelectKBest(f_regression,k=2))])

pipe=Pipeline([('union',union),('regressor',LinearRegression())])

pipe.fit(X,y)

# print(f'Number of features in the original dataset: {X.shape[-1]}')
# print(f'Number of features in the new dataset: {union.transform(X).shape[-1]}')
# print(f'R-square: {pipe.score(X,y):0.4}')


# =============================================================================
# 
# =============================================================================

# Create features and lable
X1 = np.arange(1,21)
X2 = np.arange(11,31)
y = X1 + 2*np.sqrt(X2)

# Subsume into a dataframe
df = pd.DataFrame({'X1':X1, 'X2':X2, 'y': y}, columns = ['X1', 'X2', 'y'])
# print(df.head())

train = df.iloc[:16]
test = df.iloc[16:]

train_X = train.drop('y', axis=1)
train_y = train.y

test_X = test.drop('y', axis=1)
test_y = test.y

from sklearn.metrics import mean_squared_error
# # let's see if linear regression is able to predict this properly
m1 = LinearRegression()
fit1 = m1.fit(train_X, train_y)
preds = fit1.predict(test_X)

# print(f'{preds}')
# print(f'RMSE: {np.sqrt(mean_squared_error(test_y, preds))}')

from sklearn.base import BaseEstimator, TransformerMixin

# Build custom transformer
class PythonLabTransformer(BaseEstimator, TransformerMixin):
    def __init__(self, feature):
        self.feature = feature
    
    def fit(self, X, y=None):
        return self
    
    def transform(self, X):
        Xt = X.copy()
        Xt[self.feature] = 2 * np.sqrt(Xt[self.feature])
        return Xt

pipe = Pipeline([('pythonlab_trans', PythonLabTransformer('X2')),
                  ('regressor', LinearRegression())])

# print(pipe.named_steps)

pipe.fit(train_X, train_y)
pred = pipe.predict(test_X)

print(f'{pred}')
print(f'RMSE: {np.sqrt(mean_squared_error(test_y, pred))}')

from sklearn.base import RegressorMixin

# Build regressor
class PythonLabRegressor(BaseEstimator, RegressorMixin):
    def __int__(self):
        pass
    
    def fit(self, X, y):
        self.y_mean = np.mean(y)
        return self
    
    def predict(self, X):
        return self.y_mean * np.ones(X.shape[0])
    
pipe = Pipeline([('pythonlab_trans', PythonLabTransformer('X2')),
                  ('pythonlab_regressor', PythonLabRegressor())])

pipe.named_steps
pipe.fit(train_X, train_y)
pred = pipe.predict(test_X)
print(f'{pred}')
print(f"R^2: {pipe.score(train_X, train_y)}")   # score method inherited from base class




