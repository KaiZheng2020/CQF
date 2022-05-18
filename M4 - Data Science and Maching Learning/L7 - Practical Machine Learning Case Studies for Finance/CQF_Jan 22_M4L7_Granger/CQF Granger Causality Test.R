library(lmtest)

# Run "CQF Macro Forecasting.R" to set all variables before this code.

setwd("C:/Users/TradeCap/Documents/Claus Huber/Quant Models/CQF/Presentations/2021-05-04, SML/Granger")
x_order <- 2	# For lags up to x_order.

# 1 st period (2/1963 to 11/1967), i = 1:
grangertest(x_reg[ , "BAA"], x_reg[ , "SPY"], order = x_order)
write.csv(grangertest(x_reg[ , "BAA"], x_reg[ , "SPY"], order = 2), "Granger 1.csv")

# => Change time periods.

# 2nd period (12/2015 to 9/2020), i = 635:
grangertest(x_reg[ , "BAA"], x_reg[ , "SPY"], order = x_order)
write.csv(grangertest(x_reg[ , "BAA"], x_reg[ , "SPY"], order = 2), "Granger 2.csv")
