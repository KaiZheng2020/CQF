library(timeSeries)
library(rpart)		# For Decision Trees.
library(rpart.plot)
library(randomForest)
library(xgboost)
library(car)		# for VIF & DW test.

##########
# Variable SPY excludes dividends!! SPXTR is only available since 1988 from Yahoo.
##########

# User settings:
# To run ALL Rolling Regressions: run whole code at once.
# Run the whole code always before studying individual periods.

# To run only 1 period:
# To study individual periods and compare results of different ML methods: set i manually here (1 to 635).
# and then jump to: ENTRY POINT below to run until EXIT POINT.
i <- 65	# Set i manually to periods from 1 (1963-02 to 1967-11) to 635 (2015-11 to 2020-09).

setwd("C:/My Path")
# Define regressors & regressand:
s_reg <- c("US_10Y_1Y", "vol_10Y", "SPY", "vol_SPY", "INDPRO", "PAYEMS", "BAA10YM", "BAA", "CPI_Core")			# c("US_10Y_1Y", "vol_10Y", "SPY", "vol_SPY", "NFP", "BAA10YM", "BAA", "CPI_Core")
s_dep <- "SPY"		# "US_10Y_1Y"		# Regressand.
XB_max_depth <- 2		# Setting max_depth for xgb.boost.
XB_eta <- 0.01		# Learning rate eta for XGBooster.
x_dp <- 60		# We use 60 data points for training incl. cv. The 60th dp is used to forecast the 61st dp (=OOS).
h <- 1		# Set h for h-block CV (Burman et al (1994)).

# End user settings.
##########

s_criteria <- c("IC", "HR", "RMSFE")	# Criteria for performance evalution: IC=Information Coefficient, HR=Hit Ratio, MSFE=Mean Squared Forecast Error.
x_INS <- x_dp - h	- 1	# Calculate size of training set acc. to h-block CV (Burman et al (1994)).
sh1 <- read.csv(file="Macro_Data_US_1963.csv", head=FALSE,sep=",")
sh2 <- read.csv(file="Macro_OOS Periods.csv", head=TRUE,sep=",")

s_date <- as.Date(sh1[ , 1], format="%d/%m/%Y")			# Convert column 1 to date format.
z <- apply(sh1[6:nrow(sh1), 2:ncol(sh1)], 2, as.numeric)	# Convert z to numeric.
x_div_by_100 <- sh1[2, 2:ncol(sh1)]					# Toggle that determines whether variables need to be divided by 100 (yes=1, no=0).
x_diff <- sh1[3, 2:ncol(sh1)]
colnames(x_diff) <- sh1[5, 2:ncol(sh1)]
z <- timeSeries(z, s_date[6:NROW(s_date)])			# Create z as timeSeries.
colnames(z) <- sh1[5, 2:ncol(sh1)]
z_idx <- z		# Save levels of orig. variables.

# Read periods for performance evaluation:
s_date_start <- as.Date(sh2[ , 1], format="%d/%m/%Y")			# Start dates of valuation periods.
s_date_end <- as.Date(sh2[ , 2], format="%d/%m/%Y")			# End dates of periods.

for(i in 1:ncol(z))	# Loop through number of variables.
{
	if(x_div_by_100[i] > 0)		# Divide numbers by 100 where applicable:
	{
		z[ , i] <- apply(z[ , i], 2, function(x) x/100)
	}

	if(x_diff[i] == "D")		# Calculate differences where applicable:
	{
		z[ , i] <- diff(z[ , i], 1)
	}

	if(x_diff[i] == "R")		# Calculate discrete returns where applicable:
	{
		z[2:nrow(z), i] <- returns(z[ , i], method = "discrete")
		z[1, i] <- NA
	}

	if(x_diff[i] == "ER")		# Calculate EXCESS returns where applicable:
	{
		z[2:nrow(z), i] <- returns(z[ , i]) - embed(z[1:(nrow(z) - 1), "US_1Y"]) / 12	# US_1Y
		z[1, i] <- NA
	}

}

z <- na.omit(z)	# Delete all rows with any NAs.

# Create lag structure for forecasting:
y <- z[ , s_dep]						# Isolate regressand for later taking lag.
x_3 <- cbind(lag(y, -1), z[ , s_reg])		# All variables together, s_dep shifted back by 1 period: s_dep(t-1), all other vars with index t.
x_4 <- x_3[1:(nrow(x_3) - 1), ]			# Delete last data point (NA) from lag.
s_dep_2 <- paste(s_dep, "[-1]", sep = "")		# Prepare regressand and its lag.

# We want to forecast s_dep_2(t+1).
x_loop <- nrow(z) - x_dp	# Number of OOS predictions.
s_dd <- c("AR", "LR", "DT", "RF", "XB")		# Define short names of models: linear regression, decision tree, random forests, XGBoost.
x_models <- NROW(s_dd)		# Set number of models.
x <- matrix(NA, nrow = x_loop, ncol = x_models)		# To save in-sample MSE for each model for each training period.
colnames(x) <- s_dd

x_MSE <- timeSeries(x, as.Date(time(z[(nrow(z) - x_loop + 1):nrow(z), ])))		# In-sample MSE.
x_pred <- x_MSE		# To save OOS predictions.
z_fcst <- x_pred		# To preserve dimensions, for constructing forecast chart based on index levels.
x_DW <- x_MSE		# To save Durbin/Watson test statistic.
x_DW_p <- x_DW		# dto. for p-value.

# Prepare variables to save indvidual model parameters:
x.XB_results <- x_MSE
colnames(x.XB_results) <- c("niter", "", "", "", "")

round(cor(x_4), 2)	# Print correlation matrix.


for(i in 1:x_loop)		# x_loop	# Start loop for rolling window:
{

#############
# ENTRY POINT for running ML methods for individual periods.
#############

	x_reg_orig <- x_4[i:(x_dp + i - 1), ]		# Set regressors for INS period (60 dp) & OOS (1 dp).

# Data standardisation:

	idx_INS <- 1:(x_dp - h - 1)		# Set indices for treining (=INS).
	x_reg_INS <- x_reg_orig[idx_INS, ]
	x_reg_INS_mean <- apply(x_reg_INS, 2, mean)		# Save means for later use.
	x_reg_INS_sd <- apply(x_reg_INS, 2, sd)			# Save sd for later use.
	x_reg <- apply(x_reg_INS, 2, function(x) ((x - mean(x)) / sd(x)))	# De-mean and standardise.
	x_reg_OOS <- (x_reg_orig[x_dp, ] - x_reg_INS_mean) / x_reg_INS_sd

#######
# AR model:

	x.AR <- ar.ols(x_reg[ , s_dep], aic = FALSE, order.max = 1, demean = FALSE, intercept = TRUE)	# Simple benchmark model is AR(1).
# If aic = TRUE the model's AR-order is determined by AIC. Here, the order is set to 1, i.e., AR(1).
# For AR, we use s_dep rather than s_dep_2.
	x.AR
	x_MSE[i, "AR"] <- sum(x_reg_INS_sd[s_dep_2] * x.AR$resid, na.rm = TRUE)^2 / NROW(x.AR$resid)	# In-sample MSE, unscaled.

###### Calculate OOS prediction:
	x <- x.AR$x.intercept + x_reg_OOS[ , s_dep] * x.AR$ar[1]	# Save OOS prediction (scaled).
	x_pred[i, "AR"] <- x * x_reg_INS_sd[s_dep_2] + x_reg_INS_mean[s_dep_2]		# Unscale back to original data and save for performance calculation.
	x_DW[i, "AR"] <- durbinWatsonTest(x.AR$resid[2:x_INS])		# Save DW test result (INS).

# Linear regression:
	x.LR <- lm(x_reg[ , s_dep_2] ~ x_reg[ , s_reg])
	summary(x.LR)
	x_dd <- durbinWatsonTest(x.LR)		# Save DW test result.
	x_DW[i, "LR"] <- x_dd$dw
	x_MSE[i, "LR"] <- sum(x_reg_INS_sd[s_dep_2] * residuals(x.LR))^2 / NROW(residuals(x.LR))	# In-sample MSE.
# OOS prediction:

# Set NA coefficients to 0 (can occur with collinear data):
	x_dd <- x.LR$coef[2:NROW(x.LR$coef)]
	x_idx <- which(is.na(x_dd))
	x_dd[x_idx] <- 0

	x <- x.LR$coef[1] + x_reg_OOS[ , s_reg] %*% x_dd	# Save OOS prediction (scaled).
	x_pred[i, "LR"] <- x * x_reg_INS_sd[s_dep_2] + x_reg_INS_mean[s_dep_2]		# Unscale back to original data and save for performance calculation.

# Decision Tree:
	x.DT <- rpart(formula = x_reg[ , s_dep_2] ~ ., data = x_reg[ , s_reg], method = "anova")
	x_DW[i, "DT"] <- durbinWatsonTest(residuals(x.DT))
# summary(x.DT)
# printcp(x.DT)	# Print output x.DT: CP table is the most important part of the RPART, it gives the complexity of the tree model (cp column) training error (rel error) and cross validation error (xerror).
# rpart.plot(x.DT)		# Plot chart x.DT.
	x_MSE[i, "DT"] <- sum(x_reg_INS_sd[s_dep_2] * residuals(x.DT))^2 / NROW(residuals(x.DT))
	x <- predict(x.DT, newdata = x_reg_OOS, type = "vector")	# OOS 1 period forecast.
	x_pred[i, "DT"] <- x * x_reg_INS_sd[s_dep_2] + x_reg_INS_mean[s_dep_2]		# Scale back to original data and save for performance calculation.

# Random Forests:
	df_x <- embed(x_reg[ , s_reg])	# Define features (explaining variables).
	df_y <- as.vector(embed(x_reg[ , s_dep_2]))		# x_INS: response variable (e.g., SPY or BAA10YM).
	x.RF = randomForest(x = df_x, y = df_y, ntree=100, mtry=3, replace=FALSE, importance=TRUE, do.trace = FALSE, keep.forest=TRUE)
	x.RF

# INS prediction:
	x_dd <- predict(x.RF, newdata = df_x, type = "response", predict.all=FALSE, proximity=FALSE, nodes=FALSE)	# INS 60 period forecast.
	x_res <- as.vector(x_reg[1:x_INS, s_dep_2] - x_dd)		# Calculate residuals.
	x_DW[i, "RF"] <- durbinWatsonTest(x_res)		# Durbin-Watson statistic to check for auto-correlated residuals.
	x_MSE[i, "RF"] <- sum(x_reg_INS_sd[s_dep_2] * x_res)^2 / NROW(x_res)	# In-sample MSE.

	varImpPlot(x.RF)		# Variable importance plot.
	x.RF$importance

	df_x_2 <- embed(x_reg_OOS[ , s_reg])	# Set regressors for OOS forecast.
	x <- predict(x.RF, newdata = df_x_2, type = "response") 	# OOS 1 period forecast.
	x_pred[i, "RF"] <- x * x_reg_INS_sd[s_dep_2] + x_reg_INS_mean[s_dep_2]	# Scale back to original data and save for performance calculation.

# XGBooster:
	x.CV <- xgb.cv(data = df_x, label = df_y, nrounds = 100, nfold =5, metrics = list("rmse"), max_depth = XB_max_depth, eta = XB_eta, objective = "reg:squarederror", prediction = TRUE, early_stopping_round = 10)
	x_rounds <- x.CV$best_iteration	# Save the number of iterations for use with xgb.train below.

	dtrain <- xgb.DMatrix(df_x, label = df_y)
	l_param <- list(max_depth = XB_max_depth, eta = XB_eta, verbosity = 2, objective = "reg:squarederror", eval_metric = "rmse")

	x.XB <- xgb.train(param = l_param, dtrain, nrounds = x_rounds)

# How to predict: first use xgb.cv to determine model parameters, then xgb.train to build the final model.

	x <- predict(x.XB, df_x_2)		# OOS prediction.
	x_pred[i, "XB"] <- x * x_reg_INS_sd[s_dep_2] + x_reg_INS_mean[s_dep_2]		# Scale back to original data and save for performance calculation.

# INS prediction:
	x_dd <- predict(x.XB, df_x)		# INS prediction.
	x_res <- as.vector(x_reg[1:x_INS, s_dep_2] - x_dd)		# Calculate residuals.
	x_DW[i, "XB"] <- durbinWatsonTest(x_res)

	x_MSE[i, "XB"] <- sum(x_reg_INS_sd[s_dep_2] * x_res)^2 / NROW(x_res)	# In-sample MSE, scaled.

	print(i)

##############
# EXIT POINT for running ML methods for individual periods.
##############

}		# End loop for rolling window.

################ End loop for Rolling Regressions.

# Compare orig. time series (z) with predictions (x_pred):
z_pred <- cbind(z[ , s_dep], x_pred)	# Define matrix with regressand in the leftmost column and forecast results of the ML methods in the other columns.

# Construct forecast chart based on index levels (loop takes ca. 1 min to run):

for(i in 1:x_loop)
{
	for(j in 1:x_models)
	{	
		s_dd <- as.Date(time(x_pred[i, ]))		# Start date is beginning of OOS period.
		x <- match(s_dd, as.Date(time(z_idx)))	# Find month in time series levels.
		s_model <- colnames(x_pred)[j]		# Get name of the current model.
		z_fcst[i, s_model] <- embed(z_idx[x - 1, s_dep]) * (1 + z_pred[s_dd , s_model])	# Calculate forecasted index level: idx(t) * (1 + fcst_change(t+1)).
	}
}

# z_fcst contains the forecasts unscaled. They can be compared to the original time series.
# => Evaluate predictions, e.g., RMSE etc.:

x_results <- matrix(NA, nrow = nrow(sh2), ncol = x_models * NROW(s_criteria))
s_dd <- NULL
for(j in 1:NROW(s_criteria))
{
	for(kk in 1:x_models)
	{
		s_dd <- c(s_dd, paste(colnames(x_pred)[kk], "_", s_criteria[j], sep =""))	# Create column names.
	}
}
colnames(x_results) <- s_dd	# x_results is now set to store performance measures for all models and criteria.

###################
# Calculate performance measures:

for(j in 1:NROW(s_date_start))
{
	x_pred_2 <- window(z_pred, s_date_start[j], s_date_end[j])		# Define time window for which performance is measured.
	z_fcst_2 <- window(z_fcst, s_date_start[j], s_date_end[j])		# Forecasted index levels.

# Information Coefficient:
	x <- round(cor(x_pred_2), 2)		# ICs.
	x_results[j, 1:x_models] <- x[2:nrow(x), 1]	# Store 1st column of correlation matrix as ICs.

# Hit Ratio:
# Count up/down:
	x_pred_3 <- 1 * (x_pred_2 > 0)

	for(kk in 1:x_models)
	{
# Hit Ratio or Confusion Rate:
		x_results[j, x_models + kk] <- round(sum(1 * (x_pred_3[ , s_dep] == x_pred_3[ , colnames(x_pred)[kk]])) / NROW(x_pred_3), 2)

# RMSFE:
		x_results[j, 2 * x_models + kk] <- sqrt(sum((x_pred_2[ , s_dep] - x_pred_2[ , colnames(x_pred)[kk]]) ^ 2) / nrow(x_pred_2))
	}

}		# End: for(j in 1:NROW(s_date_start))


# Merge sh2 (df with different valuation periods) and results (IC, HR, ...) for the periods:
df_results <- cbind(sh2, x_results)	# Expand data frame to include forecasting results.

df_results	# Display OOS results of periods as specified in "Macro_OOS Periods.csv".
write.csv(df_results, "1.csv")	# Write df_results to CSV file.

# Plot index levels of forecasts:
s <- window(z_idx[ , s_dep], start(z_fcst), end(z_fcst))
s <- cbind(s, z_fcst)
plot(s, main ="SPY vs. Model Forecasts")

x1 <- which(df_results[ , "Comment"] == "Rolling_Start")	# Set start index for plotting index levels.
x2 <- which(df_results[ , "Comment"] == "Rolling_End")	# Set end index for plotting index levels.

s_mod <- "XB"		# Set model ID manually to exhibit IC.
s_mod2 <- "LR"		# 
s_crit <- "IC"		# Define which performance metric to be displayed (IC, HR, RMSFE).

s_dd_1 <- paste("AR", "_", s_crit, sep="")
s_dd_2 <- paste(s_mod, "_", s_crit, sep="")
s_dd_3 <- paste(s_mod2, "_", s_crit, sep="")

# Plot ICs in the course of time:
s_colours <- x_pred[1, ]	# Define vector to set colours.
s_colours[1, ] <- c("black", "red", "green", "blue", "yellow")	# Set colours for chart.

s_dd <- paste(s_dep, ": ", s_crit, ", ", colnames(x_pred[ , "AR"]), " vs. ", colnames(x_pred[ , s_mod]), " & ", colnames(x_pred[ , s_mod2]), sep="")
x <- timeSeries(df_results[x1:x2, s_dd_1], as.Date(df_results[x1:x2, 2], format="%d/%m/%Y"))
y <- timeSeries(df_results[x1:x2, s_dd_2], as.Date(df_results[x1:x2, 2], format="%d/%m/%Y"))
y2 <- timeSeries(df_results[x1:x2, s_dd_3], as.Date(df_results[x1:x2, 2], format="%d/%m/%Y"))

plot(x, main = s_dd, type="l", ylab=s_dep, lty=1, col=s_colours[1, "AR"])	# Plot AR = benchmark IC.
lines(y, col=s_colours[1, s_mod],lty=1)			# Plot model IC in same chart.
lines(y2, col=s_colours[1, s_mod2],lty=1)			# Plot model IC in same chart.
legend(x = "topright", legend = c(colnames(x_pred[ , "AR"]), colnames(x_pred[ , s_mod]), colnames(x_pred[ , s_mod2])), lty = 1, col = c(s_colours[1, "AR"], s_colours[1, s_mod], s_colours[1, s_mod2]), lwd = )

# Format results table to include relative performance vs. AR:
x_m <- NULL		# Define matrix for displaying results.

for(j in 1:nrow(df_results))
{
	x <- t(as.matrix(rep.int(NA, ncol(df_results))))
	colnames(x) <- colnames(df_results)

# IC:
	y <- match("AR_IC", colnames(x))
	x[1, y] <- 1
	x[1, (y + 1):(y + x_models - 1)] <- 0
	x[1, y:(y + x_models - 1)] <- t(round(df_results[j, y:(y + x_models - 1)] - df_results[j, y], 2))

# HR:
	y <- match("AR_HR", colnames(x))
	x[1, y] <- 1
	x[1, (y + 1):(y + x_models - 1)] <- 0
	x[1, y:(y + x_models - 1)] <- t(round(df_results[j, y:(y + x_models - 1)] - df_results[j, y], 2))

# RMSFE:
	y <- match("AR_RMSFE", colnames(x))
	x[1, y] <- 1
	x[1, (y + 1):(y + x_models - 1)] <- 0
	x[1, y:(y + x_models - 1)] <- t(round(df_results[j, y:(y + x_models - 1)] / df_results[j, y], 6))

	x[1, "Comment"] <- "Relative to AR:"

	x_m <- rbind(x_m, rbind(df_results[j, ], x))
}

write.csv(x_m, file = paste("Results ", s_dep, ".csv", sep=""))

####################
# Plot original time series and model forecasts:

s_model <- "XB"		# Change manually to plot orig. TS vs. forecast.
x <- cbind(z_idx[ , s_dep], z_fcst)			# Merge original TS and model forecasts.
x <- window(x, start(z_fcst), end(z_fcst))	# Cut relevant time frame.
plot(x[ , 1], main = paste(s_dep, ", ", s_model, sep=""), type="l", ylab=s_dep, lty=1)	# Plot original time series.
lines(x[ , s_model], col="red",lty=1)			# Plot model forecast in same chart.

# Plot Durbin/Watson (in-sample) test statistic:
plot(x_DW, main = paste(s_dep, ": Durbin/Watson-Statistic", sep=""), col = s_colours[1, ])

# Plot Actual vs. Predictions:
x_pred_4 <- window(z_pred, s_date_start[2], s_date_end[2])
plot(x_pred_4, main = paste(s_dep, ": Actual Returns vs. Predictions", sep=""))

# Calculate Return / Vol:
z_pred_5 <- window(z_pred, s_date_start[1], s_date_end[NROW(s_date_end)])
x_pred_signal <- 1 * (z_pred_5 > 0) + (-1 * (z_pred_5 < 0))
x <- window(z_pred, s_date_start[1], s_date_end[NROW(s_date_end)])
mean(x_pred_signal[ , 2] * x[ , s_dep])
sd(x_pred_signal[ , 2] * x[ , s_dep])
mean(x_pred_signal[ , 3] * x[ , s_dep])
sd(x_pred_signal[ , 3] * x[ , s_dep])

mean(x_pred_signal[ , 4] * x[ , s_dep])
sd(x_pred_signal[ , 4] * x[ , s_dep])

mean(x_pred_signal[ , 5] * x[ , s_dep])
sd(x_pred_signal[ , 5] * x[ , s_dep])

mean(x_pred_signal[ , 6] * x[ , s_dep])
sd(x_pred_signal[ , 6] * x[ , s_dep])

