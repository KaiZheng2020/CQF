import numpy as np
import pandas as pd
import talib


def get_factors(kline_df: pd.DataFrame, sign_ratio=1.0):

    factors = pd.DataFrame()
    factors.index = kline_df.index.copy()

    open = kline_df['open']
    high = kline_df['high']
    low = kline_df['low']
    close = kline_df['close']
    volume = kline_df['volume']

    factors['Price'] = close
    factors['Returns'] = close.pct_change()
    factors['CumReturns'] = (1 + factors['Returns']).cumprod() - 1
    factors['Log_Returns'] = np.log(close).diff()
    factors['Returns_Ratio'] = close.shift(-1) / close

    factors['O-C'] = open - close
    factors['H-L'] = high - low

    # Pass Returns
    factors['PR5'] = factors['Returns'].diff(5)
    factors['PR10'] = factors['Returns'].diff(10)
    factors['PR20'] = factors['Returns'].diff(20)
    factors['PR30'] = factors['Returns'].diff(30)
    factors['PR60'] = factors['Returns'].diff(60)

    # Momentum
    factors['MOM5'] = talib.MOM(close, timeperiod=5)
    factors['MOM10'] = talib.MOM(close, timeperiod=10)
    factors['MOM20'] = talib.MOM(close, timeperiod=20)
    factors['MOM30'] = talib.MOM(close, timeperiod=30)
    factors['MOM60'] = talib.MOM(close, timeperiod=60)

    # SMA
    factors['SMA5'] = talib.SMA(close, timeperiod=5)
    factors['SMA10'] = talib.SMA(close, timeperiod=10)
    factors['SMA20'] = talib.SMA(close, timeperiod=20)
    factors['SMA30'] = talib.SMA(close, timeperiod=30)
    factors['SMA60'] = talib.SMA(close, timeperiod=60)

    alpha = 2/(kline_df.shape[0] + 1)
    factors['EMA'] = close.ewm(alpha=alpha, adjust=False).mean()

    # Label
    factors['Sign'] = np.where(close.shift(-1) > (sign_ratio * close), 1, 0)

    factors.dropna(inplace=True)

    return factors

def get_label(close, sign_ratio=1.0):
    label = np.where(close.shift(-1) > (sign_ratio * close), 1, 0)
    return label
