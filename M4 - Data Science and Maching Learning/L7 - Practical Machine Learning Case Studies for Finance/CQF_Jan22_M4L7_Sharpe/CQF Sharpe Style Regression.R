library(glmnet)
library(timeSeries)

# User settings:
# To run ML for ALL funds: run code until "1st STOP point".
# Run the whole code always before studying individual periods.

# To run only 1 period:
# To study individual periods and compare results of different ML methods: set i manually here (1 to 635).
# and then jump to: ENTRY POINT below to run until EXIT POINT.
i <- 65	# Set i manually to periods from 1 (1963-02 to 1967-11) to 635 (2015-11 to 2020-09).


# Settings:
setwd("C:/Users/TradeCap/Documents/Claus Huber/Quant Models/CQF/Presentations/2021-05-04, SML/Sharpe")
# Read periods for performance evaluation:
s_date_start <- "2015-01-31"			# Start dates of valuation periods.
s_date_end <- "2018-12-31"			# End dates of periods.

sh1 <- read.csv(file="Data Dump All.csv", head=TRUE,sep=",")
# row 2: "F" specifes Feature, "RF" specifies Risk Factor.
idx_funds <- which("F" == sh1[1, ])	# Determine indices of funds.
x_funds <- NROW(idx_funds)		# Determine # funds.
s_funds <- colnames(sh1[1, idx_funds])	# Determine names of funds.

s_date <- as.Date(sh1[2:nrow(sh1), 1], format="%d/%m/%Y")			# Convert column 1 to date format.
z <- apply(sh1[2:nrow(sh1), 2:ncol(sh1)], 2, as.numeric)	# Convert z to numeric.
s_indicator <- sh1[1, ]		# Are the TS funds (F) or risk factors (RF)?
z_idx <- timeSeries(z, s_date)			# Create z as timeSeries.

z <- returns(z_idx, method = "discrete")
s_RF <- c("SPY", "IWM", "QQQ", "EZU", "RWO", "VWO", "DBC", "HYG", "VLUE", "GOVT", "LQD")	# Define the risk factors.
s_colours <- c("black", "red", "green", "blue", "yellow", "grey", "purple", "orange", "cyan", "magenta", "purple")	# Colours for the risk factors.
z_ret <- window(z, s_date_start, s_date_end)

write.csv(cor(z_ret[ , s_RF]), "CM.csv")		# Save correlation matrix of risk factors.

x_funds <- 636
x_coef_LA <- NULL	# To save coefficients.
x_coef_RR <- NULL
x_coef_LR <- NULL
x_coef_EN <- NULL
x_lambda_LA <- NULL
x_lambda_RR <- NULL
x_lambda_EN <- NULL
x_alpha_EN <- NULL

#########
# 1st STOP point.

kk <- 512	# For analysing individual funds, set to fund number, e.g., kk = 2 for PACIX (see "Data Dump All.csv").
# Then go to ENTRY POINT and run line by line to EXIT POINT.

for(kk in 1:x_funds)
{
#########
# ENTRY POINT
#########

# Linear Regression:

	x <- as.matrix(z_ret[ , s_RF])	
	x.LR <- lm(z_ret[ , s_funds[kk]] ~ 0 + x)		# Regression model w/o intercept.
	summary(x.LR)
	
	x_coef_LR <- cbind(x_coef_LR, c(0, coef(x.LR)))

# LASSO:
# Switch off the intercept:

	x.CV <- cv.glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = 1, nfolds = 5, intercept = FALSE)	# lambda = lambda_seq
	plot(x.CV)
	title(paste(s_funds[kk], ": CV Error", sep=""), line = 3)

	x.CV$lambda.min
	x_idx <- which(min(x.CV$cvm) == x.CV$cvm)		# Determine index of min(lambda).
	x.CV$lambda[x_idx]	# min(lambda) => use this for running LASSO on full dataset.

	x.LASSO <- glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = 1, intercept = FALSE)	# min(lambda) => use this for running LASSO on full dataset.

	plot(x.LASSO, main = paste(s_funds[kk], ": LASSO\n\n", sep=""), xvar = "lambda", lwd = 2, col = s_colours, label = FALSE)
	legend(x = "topright", legend = s_RF, col = s_colours, lty = 1, lwd = 2)
	x_coef_LA <- cbind(x_coef_LA, coef(x.LASSO, s = x.CV$lambda[x_idx]))
	x_lambda_LA <- cbind(x_lambda_LA, x.CV$lambda[x_idx])

# Prediction INS:
	x_pred_INS <- predict(x.LASSO, newx = as.matrix(z_ret[ , s_RF]), s = x.CV$lambda[x_idx], type = "response")

# Prediction OOS:
	x_dd <- window(z[ , s_RF], as.Date("2020-01-31"), as.Date("2020-12-31"))
	x_pred_OOS <- predict(x.LASSO, newx = as.matrix(x_dd), s = x.CV$lambda[x_idx], type = "response")

###########################
# Ridge Regression:
# Setting alpha = 0 runs RR:
	x.CV <- cv.glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = 0, nfolds = 5, intercept = FALSE)	# lambda = lambda_seq
	plot(x.CV)
	x.CV$lambda.min
	x_idx <- which(min(x.CV$cvm) == x.CV$cvm)		# Determine index of min(lambda).
	x.CV$lambda[x_idx]	# min(lambda) => use this for running LASSO on full dataset.

	x.RR <- glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = 0, intercept = FALSE)	# min(lambda) => use this for running LASSO on full dataset.

	plot(x.RR, main = paste(s_funds[kk], ": Ridge\n\n", sep=""), xvar = "lambda", lwd = 2, col = s_colours, label = TRUE)
	title("Ridge Regression", line = 3)
	legend(x = "topright", legend = s_RF, col = s_colours, lty = 1, lwd = 2)
	x_coef_RR <- cbind(x_coef_RR, coef(x.RR, s = x.CV$lambda[x_idx]))
	x_lambda_RR <- cbind(x_lambda_RR, x.CV$lambda[x_idx])

# Prediction INS:
	x_pred_INS <- predict(x.LASSO, newx = as.matrix(z_ret[ , s_RF]), s = x.CV$lambda[x_idx], type = "response")

# Prediction OOS:
	x_dd <- window(z[ , s_RF], as.Date("2020-01-31"), as.Date("2020-12-31"))
	x_pred_OOS <- predict(x.LASSO, newx = as.matrix(x_dd), s = x.CV$lambda[x_idx], type = "response")

#########
# Elastic Net via glmnet:
	x_alpha <- seq(0.05, 0.95, by = 0.05)	# Set sequence for grid search for alpha.
	x_EN_results <- matrix(NA, nrow = NROW(x_alpha), ncol = 3)		# rep(NA, NROW(x_alpha))
	colnames(x_EN_results) <- c("idx", "cvm", "lambda")

	for(i in 1:NROW(x_alpha))		# Loop through a sequence of values for alpha.
	{
		x.ENCV <- cv.glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = x_alpha[i], nfolds = 5, intercept = FALSE)	# lambda = lambda_seq
		x_EN_results[i, "idx"] <- which(min(x.ENCV$cvm) == x.ENCV$cvm)		# Determine index of min(lambda).
		x_EN_results[i, "cvm"] <- min(x.ENCV$cvm)
		x_EN_results[i, "lambda"] <- min(x.ENCV$lambda)
	}

	x_idx_EN <- which(min(x_EN_results[ , "cvm"]) == x_EN_results[ , "cvm"])	# Determine model with lowest cvm (CV Error).
	x.EN <- glmnet(as.matrix(z_ret[ , s_RF]), z_ret[ , s_funds[kk]], alpha = x_alpha[x_idx_EN], intercept = FALSE)	# Rebuild Eastic Net with CV parameters.
	plot(x.EN, main = paste(s_funds[kk], ": Elastic Net\n\n", sep=""))
	x_coef_EN <- cbind(x_coef_EN, coef(x.EN, s = x_EN_results[x_idx_EN, "lambda"]))	# Pull coefficients from Elastic Net with lambda & alpha corresponding to model w/ lowest CV Error.

# https://stackoverflow.com/questions/48079660/extract-the-coefficients-for-the-best-tuning-parameters-of-a-glmnet-model-in-car
	x_lambda_EN <- cbind(x_lambda_EN, x_EN_results[x_idx_EN, "lambda"])
	x_alpha_EN <- cbind(x_alpha_EN, x_alpha[x_idx_EN])

	print(kk)

#########
# EXIT POINT
#########

}

# Write coefficients for each funds:
rownames(x_lambda_LA) <- "lambda"
x <- as.matrix(x_coef_LA)
x <- rbind(x, rep(NA, kk), x_lambda_LA)		# For each fund: merge coefficients & lambda parameters.
colnames(x) <- s_funds[1:kk]
write.csv(x, file = "Coeff_LA.csv")

rownames(x_lambda_RR) <- "lambda"
x <- as.matrix(x_coef_RR)
x <- rbind(x, rep(NA, kk), x_lambda_RR)
colnames(x) <- s_funds[1:kk]
write.csv(x, file = "Coeff_RR.csv")

x <- as.matrix(x_coef_LR)
colnames(x) <- s_funds[1:kk]
write.csv(x, file = "Coeff_LR.csv")

rownames(x_lambda_EN) <- "lambda"
rownames(x_alpha_EN) <- "alpha"
x <- as.matrix(x_coef_EN)
x <- rbind(x, rep(NA, kk), x_lambda_EN, x_alpha_EN)		# Merge parameters.
colnames(x) <- s_funds[1:kk]
write.csv(x, file = "Coeff_EN.csv")
