import numpy as np
import pandas as pd
from numpy import concatenate, exp, maximum, mean, random, sqrt
from scipy.stats import gmean


class Option:
    def __init__(self, s0, strike, mu, sigma, horizon, timesteps, n_sims) -> None:
        # Read parameters
        self.S0 = s0  # initial spot level
        self.E = strike  # Strike E
        self.r = mu  # mu = rf in risk neutral framework
        self.sigma = sigma  # volatility
        self.T = horizon  # time horizion
        self.t = timesteps  # number of time steps
        self.n = n_sims  # number of simulation

        self.monte_carlo_simulate()

    def monte_carlo_simulate(self):

        # Set the random seed for reproducibility
        # Same seed leads to the same set of random values
        random.seed(10000)

        # Define dt
        dt = self.T / self.t  # length of time interval

        # Simulating 'n' asset price paths with 't' timesteps
        S = np.zeros((self.t, self.n))
        S[0] = self.S0

        for i in range(0, self.t - 1):

            w = random.standard_normal(int(self.n / 2))  # psuedo random numbers
            w = concatenate((w, -w), axis=0)

            if (self.n % 2) != 0:
                w = concatenate((w, random.standard_normal(1)), axis=0)

            S[i + 1] = S[i] * (1 + self.r * dt + self.sigma * sqrt(dt) * w)  # vectorized operation per timesteps

        self.S = S

    def monte_carlo_simulate_exp(self):

        # Set the random seed for reproducibility
        # Same seed leads to the same set of random values
        random.seed(10000)

        # Define dt
        dt = self.T / self.t  # length of time interval

        # Simulating 'n' asset price paths with 't' timesteps
        S = np.zeros((self.t, self.n))
        S[0] = self.S0

        for i in range(0, self.t - 1):

            w = random.standard_normal(int(self.n / 2))  # psuedo random numbers
            w = concatenate((w, -w), axis=0)

            if (self.n % 2) != 0:
                w = concatenate((w, random.standard_normal(1)), axis=0)

            S[i + 1] = S[i] * exp((self.r - 0.5 * self.sigma**2) * dt + self.sigma * sqrt(dt) * w)  # alternate form

        self.S = S

    def payoff(self, array_1, array_2):

        call = exp(-self.r * self.T) * mean(maximum(array_1 - array_2, 0))
        put = exp(-self.r * self.T) * mean(maximum(array_2 - array_1, 0))

        return call, put


class AsianOption(Option):
    def arighmetic_average(self):
        return self.S.mean(axis=0)

    def geometric_average(self):
        return gmean(self.S)

    def continuous_arighmetic_average(self, array):
        avg = []
        for i in range(len(array)):
            temp = i
            sum = 0
            while temp >= 0:
                sum = sum + array[temp]
                temp = temp - 1
            avg.append(sum / (i + 1))

        return np.array(avg)

    def continuous_geometric_average(self, array):
        avg = []
        for i in range(len(array)):
            temp = i
            sum = 1
            while temp >= 0:
                sum = sum * (array[temp]**(1 / (i + 1)))
                temp = temp - 1
            avg.append(sum)

        return np.array(avg)

    def discrete_arighmetic_average(self, array, window_size):
        ret = []
        for index in range(len(array)):
            sum = 0
            if index % window_size == 0:
                avg = 0
                for new_temp in range(index, (index + window_size), 1):
                    if new_temp < len(array):
                        sum = sum + array[new_temp]
                        avg = sum / window_size
                    else:
                        avg = sum / (len(array) - index)
            ret.append(avg)
        return np.array(ret)

    def discrete_geometric_average(self, array, window_size):
        ret = []
        array_len = len(array)
        for index in range(array_len):
            mult = 1
            if index % window_size == 0:
                for j in range(index, (index + window_size), 1):
                    avg = 0
                    if j < array_len:
                        mult = mult * (array[j]**(1 / window_size))
                        avg = mult
                    else:
                        mult = 1

                        for last_j in range(array_len, index, -1):
                            if last_j <= array_len:
                                mult = mult * ((array[last_j - 1])**(1 / (array_len - index)))
                                avg = mult
            ret.append(avg)
        return np.array(ret)

    def discrete_geometric_average_gmean(self, array, window_size):
        df = pd.DataFrame(array).rolling(window=window_size, min_periods=window_size).apply(gmean)
        return df

    def payoff_continuous_arighmetic_average(self):

        c_strike_calls, c_strike_puts, c_rate_calls, c_rate_puts = [], [], [], []

        for index in range(self.n):

            array = self.S[:, index]

            continuous_arighmetic_average = self.continuous_arighmetic_average(array)

            S = array
            A = continuous_arighmetic_average
            E = np.array([self.E] * self.t)

            c_strike_call, c_strike_put = self.payoff(S, A)
            c_rate_call, c_rate_put = self.payoff(A, E)

            c_strike_calls.append(c_strike_call)
            c_strike_puts.append(c_strike_put)
            c_rate_calls.append(c_rate_call)
            c_rate_puts.append(c_rate_put)

        return c_strike_calls, c_strike_puts, c_rate_calls, c_rate_puts

    def payoff_mean_continuous_arighmetic_average(self):

        c_strike_calls, c_strike_puts, c_rate_calls, c_rate_puts = self.payoff_continuous_arighmetic_average()

        return np.mean(c_strike_calls), np.mean(c_strike_puts), np.mean(c_rate_calls), np.mean(c_rate_puts)

    def payoff_discrete_arighmetic_average(self, window_size):

        d_strike_calls, d_strike_puts, d_rate_calls, d_rate_puts = [], [], [], []

        for index in range(self.n):

            array = self.S[:, index]

            discrete_arighmetic_average = self.discrete_arighmetic_average(array, window_size)

            S = array
            A = discrete_arighmetic_average
            E = np.array([self.E] * self.t)

            d_strike_call, d_strike_put = self.payoff(S, A)
            d_rate_call, d_rate_put = self.payoff(A, E)

            d_strike_calls.append(d_strike_call)
            d_strike_puts.append(d_strike_put)
            d_rate_calls.append(d_rate_call)
            d_rate_puts.append(d_rate_put)

        return d_strike_calls, d_strike_puts, d_rate_calls, d_rate_puts

    def payoff_mean_discrete_arighmetic_average(self, window_size):

        d_strike_calls, d_strike_puts, d_rate_calls, d_rate_puts = self.payoff_discrete_arighmetic_average(window_size)

        return np.mean(d_strike_calls), np.mean(d_strike_puts), np.mean(d_rate_calls), np.mean(d_rate_puts)


class LookbackOption(Option):
    def continuous_maximum(self, array):
        ret = []
        for index in range(len(array)):
            if index == 0:
                higher = array[index]
            if higher < array[index]:
                higher = array[index]
            else:
                higher = higher
            ret.append(higher)

        return np.array(ret)

    def discrete_maximum(self, array, window_size):
        ret = []
        for index in range(len(array)):
            if index == 0:
                higher = array[index]
            if index % window_size == 0:
                if higher < array[index]:
                    higher = array[index]

            ret.append(higher)

        return np.array(ret)

    def payoff_continuous(self):

        c_fixed_calls, c_fixed_puts, c_floating_calls, c_floating_puts = [], [], [], []

        for index in range(self.n):

            array = self.S[:, index]

            M = self.continuous_maximum(array)
            S = array
            E = np.array([self.E] * self.t)

            c_fixed_call, c_fixed_put = self.payoff(S, M)
            c_floating_call, c_floating_put = self.payoff(M, E)

            c_fixed_calls.append(c_fixed_call)
            c_fixed_puts.append(c_fixed_put)
            c_floating_calls.append(c_floating_call)
            c_floating_puts.append(c_floating_put)

        return c_fixed_calls, c_fixed_puts, c_floating_calls, c_floating_puts

    def payoff_mean_continuous(self):

        c_fixed_calls, c_fixed_puts, c_floating_calls, c_floating_puts = self.payoff_continuous()

        return np.mean(c_fixed_calls), np.mean(c_fixed_puts), np.mean(c_floating_calls), np.mean(c_floating_puts)

    def payoff_discrete(self, window_size):

        d_fixed_calls, d_fixed_puts, d_floating_calls, d_floating_puts = [], [], [], []

        for index in range(self.n):

            array = self.S[:, index]

            M = self.discrete_maximum(array, window_size)
            S = array
            E = np.array([self.E] * self.t)

            d_fixed_call, d_fixed_put = self.payoff(S, M)
            d_floating_call, d_floating_put = self.payoff(M, E)

            d_fixed_calls.append(d_fixed_call)
            d_fixed_puts.append(d_fixed_put)
            d_floating_calls.append(d_floating_call)
            d_floating_puts.append(d_floating_put)

        return d_fixed_calls, d_fixed_puts, d_floating_calls, d_floating_puts

    def payoff_mean_discrete(self, window_size):

        d_fixed_calls, d_fixed_puts, d_floating_calls, d_floating_puts = self.payoff_discrete(window_size)

        return np.mean(d_fixed_calls), np.mean(d_fixed_puts), np.mean(d_floating_calls), np.mean(d_floating_puts)
