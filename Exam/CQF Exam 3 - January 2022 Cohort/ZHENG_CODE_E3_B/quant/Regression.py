from sklearn.pipeline import Pipeline
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error


class Regression:
    def __init__(self, X, y, test_size):

        # split training and testing dataset
        self.X_train, self.X_test, self.y_train, self.y_test = train_test_split(
            X, y, test_size=test_size, random_state=0)

    def fit_predict(self, estimator, transformer):

        try:
            # subsume estimators and transformer into a pipeline
            self.pipe = Pipeline([
                ('scaler', transformer),
                ('regressor', estimator)
            ])

            # fit/train model
            self.pipe.fit(self.X_train, self.y_train)

            # predict lables
            self.y_pred = self.pipe.predict(self.X_test)

        except Exception as e:
            print(str(e))

    def eval_metrics(self):

        # evaluate metrics
        mse = mean_squared_error(self.y_test, self.y_pred, squared=True)
        rmse = mean_squared_error(self.y_test, self.y_pred, squared=False)
        r2train = self.pipe.score(self.X_train, self.y_train)
        r2test = self.pipe.score(self.X_test, self.y_test)

        return mse, rmse, r2train, r2test
