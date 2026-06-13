############################################################
# CLASS CODE 2
# Unconditional and conditional forecasts with a VAR/SVAR
############################################################

# Goal of this code:
# 1. Estimate a reduced-form VAR.
# 2. Use the estimated reduced-form VAR to produce unconditional forecasts.
# 3. Recover the structural impact matrix using recursive identification.
# 4. Impose future EPU paths and compute conditional/structural forecasts.
# 5. Compare unconditional and conditional forecasts.
# 6. Introduce a Bayesian VAR (BVAR) as an alternative forecasting approach,
#    where priors are used to stabilize the estimates and forecasts are based
#    on posterior draws.

############################################################
# 0. Variable definitions used in the VAR
############################################################

# epu_ae_new  = Economic Policy Uncertainty (EPU) index for the euro area, in levels
#               higher values mean more policy-related uncertainty

# spread_ae   = spread between the 10-year Spanish government bond yield
#               and the 10-year German government bond yield

# pib_ae      = euro area real GDP, measured as quarter-on-quarter growth rates
#               seasonally adjusted, so this is a growth rate, not a GDP level

# precios_ae  = euro area HICP inflation, measured as quarter-on-quarter growth rates
#               seasonally adjusted, so this is an inflation rate, not a price level

############################################################
# 1. Start clean and load packages
############################################################

# remove all objects currently stored in the R environment
rm(list = ls())

# packages
# install.packages("vars")       # run once if the vars package is not installed
# install.packages("BVAR")       # run once if the BVAR package is not installed
# install.packages("tidyverse")  # run once if tidyverse is not installed

library("BVAR")
library("vars")
library("readxl")
library("dplyr")
library("tidyverse")

############################################################
# 2. Set working directory and load data
############################################################

# set the working directory equal to the folder where the current R script is saved
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# print the current working directory to check that it was set correctly
getwd()

# load the Excel file "data_condforecast.xlsx" and import the sheet called "ae+scenarios"
my_data <- read_excel("data_condforecast.xlsx", "ae+scenarios")

# convert the qdate column into Date format using day/month/year
my_data[['qdate']] <- as.Date(my_data[['qdate']], format = "%d/%m/%Y")

# display the dataset in the console
print(my_data)

############################################################
# 3. Estimate the reduced-form VAR
############################################################

# keep only observations up to 2019Q4; this creates the estimation sample 
sample <- my_data[my_data$qdate <= as.Date("2019-12-01"), ]

# create a new dataset called myvar keeping only the variables that will enter the VAR
myvar <- subset(sample, select = c(epu_ae_new, spread_ae, pib_ae, precios_ae))

# print the selected VAR variables in the console
print(myvar)

# show basic descriptive statistics for the selected variables
summary(myvar)

# estimate the reduced-form VAR with 1 lag and a constant term
var.est1 <- VAR(myvar, p = 1, type = "const", season = NULL)

# display the estimation results of the VAR
summary(var.est1)

# The reduced-form VAR is:
#
# y_t = c + Phi_1 y_{t-1} + u_t
#
# The unconditional forecast uses this equation recursively,
# setting future reduced-form shocks equal to zero in expectation.

############################################################
# 4. Create forecast-time index
############################################################

# create a simple time index that numbers the observations from 1 to the last row
my_data$index <- 1:nrow(my_data)

# find the row number corresponding to the date 2022-01-01
my_data$index[my_data$qdate == as.Date("2022-01-01")]

# create a forecast-time variable
# here, the observation with index 101 becomes period 1 of the forecast horizon
my_data$time_fcast <- my_data$index - 101 + 1

# replace negative values with NaN, so periods before the forecast start are ignored
my_data$time_fcast[my_data$time_fcast < 0] <- NaN

# print the forecast-time index to check that it was created correctly
print(my_data$time_fcast)

# remove the temporary index variable
my_data <- subset(my_data, select = -index)

# print the updated dataset
print(my_data)

############################################################
# 5. Extract reduced-form VAR coefficients
############################################################

# explore the reduced-form VAR equation for EPU
var.est1$varresult$epu_ae_new  

# store the estimated coefficients of each reduced-form VAR equation
phi1 <- var.est1$varresult$epu_ae_new$coefficients 
phi2 <- var.est1$varresult$spread_ae$coefficients 
phi3 <- var.est1$varresult$pib_ae$coefficients 
phi4 <- var.est1$varresult$precios_ae$coefficients

# print the coefficients of the first equation
print(phi1)

############################################################
# 6. Unconditional forecast
############################################################

##########################
# UNCONDITIONAL FORECAST
##########################

# create a data frame that will store the unconditional forecast
f_unco <- data.frame(my_data$qdate, my_data$time_fcast, my_data$epu_ae_new, my_data$spread_ae, my_data$pib_ae, my_data$precios_ae)

# print the forecast data frame to check that it has been created correctly
print(f_unco)

# store the variable names as character strings
varnames <- c("my_data.epu_ae_new", "my_data.spread_ae", "my_data.pib_ae", "my_data.precios_ae")

# one-step-ahead unconditional forecast, t = 1. Data to use as input are in f_unco$my_data.time_fcast == 0
currentpredictors <- as.matrix(f_unco[which(f_unco$my_data.time_fcast == 0), varnames])

# forecast EPU at t = 1
f_unco$my_data.epu_ae_new[which(f_unco$my_data.time_fcast == 1)] <- currentpredictors %*% as.matrix(phi1[1:4]) + phi1[5]

# forecast spread at t = 1
f_unco$my_data.spread_ae[which(f_unco$my_data.time_fcast == 1)]  <- currentpredictors %*% as.matrix(phi2[1:4]) + phi2[5]

# forecast GDP at t = 1
f_unco$my_data.pib_ae[which(f_unco$my_data.time_fcast == 1)]     <- currentpredictors %*% as.matrix(phi3[1:4]) + phi3[5]

# forecast prices at t = 1
f_unco$my_data.precios_ae[which(f_unco$my_data.time_fcast == 1)] <- currentpredictors %*% as.matrix(phi4[1:4]) + phi4[5]

# multi-step unconditional forecast for horizons t = 2 to 8
for (i in 2:8) {
  currentpredictors <- as.matrix(f_unco[which(f_unco$my_data.time_fcast == i - 1), varnames])
  
  f_unco$my_data.epu_ae_new[which(f_unco$my_data.time_fcast == i)] <- currentpredictors %*% as.matrix(phi1[1:4]) + phi1[5]
  f_unco$my_data.spread_ae[which(f_unco$my_data.time_fcast == i)]  <- currentpredictors %*% as.matrix(phi2[1:4]) + phi2[5]
  f_unco$my_data.pib_ae[which(f_unco$my_data.time_fcast == i)]     <- currentpredictors %*% as.matrix(phi3[1:4]) + phi3[5]
  f_unco$my_data.precios_ae[which(f_unco$my_data.time_fcast == i)] <- currentpredictors %*% as.matrix(phi4[1:4]) + phi4[5]
}

# print the forecasted GDP path
print(f_unco$my_data.pib_ae)

# plot the GDP path over the forecast window
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(f_unco$my_data.pib_ae[which(f_unco$my_data.time_fcast >= 0 | f_unco$my_data.time_fcast <= 8)], type = "l")

# print the full GDP series stored in the forecast data frame
f_unco$my_data.pib_ae

############################################################
# 7. Structural VAR and Cholesky impact matrix
############################################################

##########################
# STRUCTURAL VAR
##########################

# impose recursive Cholesky-type contemporaneous restrictions through matrix A
a.mat <- diag(4)

# free the diagonal normalization terms for estimation
diag(a.mat) <- NA

# allow variable 1 to affect variable 2 contemporaneously
a.mat[2, 1] <- NA

# allow variable 1 to affect variable 3 contemporaneously
a.mat[3, 1] <- NA

# allow variable 1 to affect variable 4 contemporaneously
a.mat[4, 1] <- NA

# allow variable 2 to affect variable 3 contemporaneously
a.mat[3, 2] <- NA

# allow variable 2 to affect variable 4 contemporaneously
a.mat[4, 2] <- NA

# allow variable 3 to affect variable 4 contemporaneously
a.mat[4, 3] <- NA

# print matrix A to check the recursive contemporaneous structure
print(a.mat)

# define matrix B to identify the structural shocks
b.mat <- diag(4)

# free the diagonal elements so the shock-impact scales can be estimated
diag(b.mat) <- NA

# print matrix B to check the identification restrictions
print(b.mat)

# estimate the structural VAR
svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)

# print the estimated structural VAR results
svar.one

# compute the total contemporaneous impact matrix:
# P = A0^{-1} B
solve(svar.one$A) %*% svar.one$B

# extract the contemporaneous effect of a one-unit EPU structural shock on GDP
(solve(svar.one$A) %*% svar.one$B)["pib_ae", "epu_ae_new"]

# estimate the impulse response function from the structural VAR
one.gdp <- irf(svar.one, response = "pib_ae", impulse = "epu_ae_new", n.ahead = 10, ortho = TRUE, boot = TRUE)

# plot the IRF of GDP to an EPU shock
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(one.gdp)

# print the IRF values and confidence intervals
one.gdp

############################################################
# 8. Recover and normalize the impact matrix
############################################################

# store the estimated contemporaneous matrix among endogenous variables
Aest <- svar.one$A 
print(Aest)

# store the estimated matrix of direct structural shock impacts
Best <- svar.one$B 
print(Best)

# compute the inverse of A0
invA <- solve(Aest)

# recover the slides' impact matrix A: A = A0^{-1} B
chol_est <- invA %*% Best

# print A: each column is the contemporaneous effect of one structural shock on all variables
print(chol_est)

# recover the same impact matrix directly from the reduced-form covariance matrix
Sigmaest <- svar.one$Sigma.U / 100
print(Sigmaest)

chol_est <- t(chol(Sigmaest))
S <- chol_est %*% t(chol_est)
print(chol_est)

# normalize the impact matrix so that the first shock raises EPU by 1 on impact
chol_normalized <- chol_est / chol_est[1,1]
chol_normalized

# extract the first-column effects of a unit EPU shock
chol_11 <- chol_normalized[1,1]
chol_21 <- chol_normalized[2,1]
chol_31 <- chol_normalized[3,1]
chol_41 <- chol_normalized[4,1]

# Interpretation:
# chol_11, chol_21, chol_31, chol_41 are the contemporaneous responses of
# EPU, spread, GDP, and prices to a shock that raises EPU by 1 on impact.

############################################################
# 9. Conditional + structural forecasts
############################################################

#################################
# CONDITIONAL + STRUCTURAL FORECAST
#################################

# create one data frame for each scenario
f_base <- data.frame(my_data$qdate, my_data$time_fcast, my_data$epu_ae_new, my_data$spread_ae, my_data$pib_ae, my_data$precios_ae, my_data$baseline)
f_prol <- data.frame(my_data$qdate, my_data$time_fcast, my_data$epu_ae_new, my_data$spread_ae, my_data$pib_ae, my_data$precios_ae, my_data$prolonged)
f_inte <- data.frame(my_data$qdate, my_data$time_fcast, my_data$epu_ae_new, my_data$spread_ae, my_data$pib_ae, my_data$precios_ae, my_data$intense)

# print the scenario data frames to check that the imposed paths have been loaded correctly
print(f_base)
print(f_prol)

############################################################
# 9.1 One-step-ahead conditional forecasts
############################################################

# First compute the t = 1 reduced-form benchmark forecast for each scenario.
# At this stage, the three scenarios are identical because no EPU path has been imposed yet.

# 1. Baseline scenario
f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 1)] <- phi1[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 0)] + phi1[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 0)] + phi1[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 0)] + phi1[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 0)] + phi1[5]
f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 1)] <- phi2[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 0)] + phi2[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 0)] + phi2[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 0)] + phi2[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 0)] + phi2[5]
f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 1)] <- phi3[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 0)] + phi3[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 0)] + phi3[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 0)] + phi3[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 0)] + phi3[5]
f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 1)] <- phi4[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 0)] + phi4[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 0)] + phi4[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 0)] + phi4[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 0)] + phi4[5]

# 2. Prolonged scenario
f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 1)] <- phi1[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 0)] +phi1[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 0)]+phi1[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 0)]+phi1[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 0)]+phi1[5]
f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 1)] <-  phi2[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 0)] +phi2[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 0)]+phi2[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 0)]+phi2[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 0)]+phi2[5]
f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 1)] <-     phi3[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 0)] +phi3[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 0)]+phi3[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 0)]+phi3[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 0)]+phi3[5]
f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 1)] <- phi4[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 0)] +phi4[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 0)]+phi4[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 0)]+phi4[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 0)]+phi4[5]

# 3. Intense scenario
f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 1)] <- phi1[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 0)] +phi1[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 0)]+phi1[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 0)]+phi1[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 0)]+phi1[5]
f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 1)] <-  phi2[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 0)] +phi2[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 0)]+phi2[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 0)]+phi2[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 0)]+phi2[5]
f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 1)] <-     phi3[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 0)] +phi3[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 0)]+phi3[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 0)]+phi3[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 0)]+phi3[5]
f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 1)] <- phi4[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 0)] +phi4[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 0)]+phi4[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 0)]+phi4[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 0)]+phi4[5]

print(f_base)

# At this stage, the t = 1 GDP forecast is the same in all three scenario data frames
f_base$my_data.pib_ae
f_prol$my_data.pib_ae
f_inte$my_data.pib_ae

############################################################
# 9.2 Compute the EPU shock needed to hit the imposed path
############################################################

f_base$shock_base <- NaN
f_prol$shock_prol <- NaN
f_inte$shock_inte <- NaN

# baseline scenario
f_base$shock_base[which(f_base$my_data.time_fcast == 1)] <- f_base$my_data.baseline[which(f_base$my_data.time_fcast == 1)] - f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 1)] 

# prolonged scenario
f_prol$shock_prol[which(f_prol$my_data.time_fcast == 1)] <- f_prol$my_data.prolonged[which(f_prol$my_data.time_fcast == 1)] - f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 1)] 

# intense scenario
f_inte$shock_inte[which(f_inte$my_data.time_fcast == 1)] <- f_inte$my_data.intense[which(f_inte$my_data.time_fcast == 1)] - f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 1)]

############################################################
# 9.3 Add the contemporaneous structural effects
############################################################

# baseline
f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 1)] <- f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == 1)] + (chol_11 * f_base$shock_base[which(f_base$my_data.time_fcast == 1)])
f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 1)]  <- f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == 1)]  + (chol_21 * f_base$shock_base[which(f_base$my_data.time_fcast == 1)])
f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 1)]     <- f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == 1)]     + (chol_31 * f_base$shock_base[which(f_base$my_data.time_fcast == 1)])
f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 1)] <- f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == 1)] + (chol_41 * f_base$shock_base[which(f_base$my_data.time_fcast == 1)])

# prolonged
f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 1)] <- f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == 1)] + (chol_11 * f_prol$shock_prol[which(f_prol$my_data.time_fcast == 1)])
f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 1)]  <- f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == 1)]  + (chol_21 * f_prol$shock_prol[which(f_prol$my_data.time_fcast == 1)])
f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 1)]     <- f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == 1)]     + (chol_31 * f_prol$shock_prol[which(f_prol$my_data.time_fcast == 1)])
f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 1)] <- f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == 1)] + (chol_41 * f_prol$shock_prol[which(f_prol$my_data.time_fcast == 1)])

# intense
f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 1)] <- f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == 1)] + (chol_11 * f_inte$shock_inte[which(f_inte$my_data.time_fcast == 1)])
f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 1)]  <- f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == 1)]  + (chol_21 * f_inte$shock_inte[which(f_inte$my_data.time_fcast == 1)])
f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 1)]     <- f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == 1)]     + (chol_31 * f_inte$shock_inte[which(f_inte$my_data.time_fcast == 1)])
f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 1)] <- f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == 1)] + (chol_41 * f_inte$shock_inte[which(f_inte$my_data.time_fcast == 1)])

# after adding the structural EPU shock, the GDP forecast now differs across scenarios
f_base$my_data.pib_ae
f_prol$my_data.pib_ae
f_inte$my_data.pib_ae

############################################################
# 9.4 Repeat the conditional + structural forecast for t = 2,...,5
############################################################

for (i in 2:5) {
  
  # explicit row indices: current horizon i and previous horizon i-1
  idx_b    <- match(i,   f_base$my_data.time_fcast)
  idx_b_1  <- match(i-1, f_base$my_data.time_fcast)
  idx_p    <- match(i,   f_prol$my_data.time_fcast)
  idx_p_1  <- match(i-1, f_prol$my_data.time_fcast)
  idx_i    <- match(i,   f_inte$my_data.time_fcast)
  idx_i_1  <- match(i-1, f_inte$my_data.time_fcast)
  
  # stop immediately if one of the forecast rows is not found
  stopifnot(!is.na(idx_b), !is.na(idx_b_1), !is.na(idx_p), !is.na(idx_p_1), !is.na(idx_i), !is.na(idx_i_1))
  
  # 1. reduced-form benchmark forecast
  # baseline
  f_base$my_data.epu_ae_new[idx_b] <- phi1[1]*f_base$my_data.epu_ae_new[idx_b_1] + phi1[2]*f_base$my_data.spread_ae[idx_b_1] + phi1[3]*f_base$my_data.pib_ae[idx_b_1] + phi1[4]*f_base$my_data.precios_ae[idx_b_1] + phi1[5]
  f_base$my_data.spread_ae[idx_b]  <- phi2[1]*f_base$my_data.epu_ae_new[idx_b_1] + phi2[2]*f_base$my_data.spread_ae[idx_b_1] + phi2[3]*f_base$my_data.pib_ae[idx_b_1] + phi2[4]*f_base$my_data.precios_ae[idx_b_1] + phi2[5]
  f_base$my_data.pib_ae[idx_b]     <- phi3[1]*f_base$my_data.epu_ae_new[idx_b_1] + phi3[2]*f_base$my_data.spread_ae[idx_b_1] + phi3[3]*f_base$my_data.pib_ae[idx_b_1] + phi3[4]*f_base$my_data.precios_ae[idx_b_1] + phi3[5]
  f_base$my_data.precios_ae[idx_b] <- phi4[1]*f_base$my_data.epu_ae_new[idx_b_1] + phi4[2]*f_base$my_data.spread_ae[idx_b_1] + phi4[3]*f_base$my_data.pib_ae[idx_b_1] + phi4[4]*f_base$my_data.precios_ae[idx_b_1] + phi4[5]
  
  # prolonged
  f_prol$my_data.epu_ae_new[idx_p] <- phi1[1]*f_prol$my_data.epu_ae_new[idx_p_1] + phi1[2]*f_prol$my_data.spread_ae[idx_p_1] + phi1[3]*f_prol$my_data.pib_ae[idx_p_1] + phi1[4]*f_prol$my_data.precios_ae[idx_p_1] + phi1[5]
  f_prol$my_data.spread_ae[idx_p]  <- phi2[1]*f_prol$my_data.epu_ae_new[idx_p_1] + phi2[2]*f_prol$my_data.spread_ae[idx_p_1] + phi2[3]*f_prol$my_data.pib_ae[idx_p_1] + phi2[4]*f_prol$my_data.precios_ae[idx_p_1] + phi2[5]
  f_prol$my_data.pib_ae[idx_p]     <- phi3[1]*f_prol$my_data.epu_ae_new[idx_p_1] + phi3[2]*f_prol$my_data.spread_ae[idx_p_1] + phi3[3]*f_prol$my_data.pib_ae[idx_p_1] + phi3[4]*f_prol$my_data.precios_ae[idx_p_1] + phi3[5]
  f_prol$my_data.precios_ae[idx_p] <- phi4[1]*f_prol$my_data.epu_ae_new[idx_p_1] + phi4[2]*f_prol$my_data.spread_ae[idx_p_1] + phi4[3]*f_prol$my_data.pib_ae[idx_p_1] + phi4[4]*f_prol$my_data.precios_ae[idx_p_1] + phi4[5]
  
  # intense
  f_inte$my_data.epu_ae_new[idx_i] <- phi1[1]*f_inte$my_data.epu_ae_new[idx_i_1] + phi1[2]*f_inte$my_data.spread_ae[idx_i_1] + phi1[3]*f_inte$my_data.pib_ae[idx_i_1] + phi1[4]*f_inte$my_data.precios_ae[idx_i_1] + phi1[5]
  f_inte$my_data.spread_ae[idx_i]  <- phi2[1]*f_inte$my_data.epu_ae_new[idx_i_1] + phi2[2]*f_inte$my_data.spread_ae[idx_i_1] + phi2[3]*f_inte$my_data.pib_ae[idx_i_1] + phi2[4]*f_inte$my_data.precios_ae[idx_i_1] + phi2[5]
  f_inte$my_data.pib_ae[idx_i]     <- phi3[1]*f_inte$my_data.epu_ae_new[idx_i_1] + phi3[2]*f_inte$my_data.spread_ae[idx_i_1] + phi3[3]*f_inte$my_data.pib_ae[idx_i_1] + phi3[4]*f_inte$my_data.precios_ae[idx_i_1] + phi3[5]
  f_inte$my_data.precios_ae[idx_i] <- phi4[1]*f_inte$my_data.epu_ae_new[idx_i_1] + phi4[2]*f_inte$my_data.spread_ae[idx_i_1] + phi4[3]*f_inte$my_data.pib_ae[idx_i_1] + phi4[4]*f_inte$my_data.precios_ae[idx_i_1] + phi4[5]
  
  # 2. scenario EPU gap
  f_base$shock_base[idx_b] <- f_base$my_data.baseline[idx_b]   - f_base$my_data.epu_ae_new[idx_b]
  f_prol$shock_prol[idx_p] <- f_prol$my_data.prolonged[idx_p]  - f_prol$my_data.epu_ae_new[idx_p]
  f_inte$shock_inte[idx_i] <- f_inte$my_data.intense[idx_i]    - f_inte$my_data.epu_ae_new[idx_i]
  
  # 3. conditional + structural adjustment
  f_base$my_data.epu_ae_new[idx_b] <- f_base$my_data.epu_ae_new[idx_b] + chol_11*f_base$shock_base[idx_b]
  f_base$my_data.spread_ae[idx_b]  <- f_base$my_data.spread_ae[idx_b]  + chol_21*f_base$shock_base[idx_b]
  f_base$my_data.pib_ae[idx_b]     <- f_base$my_data.pib_ae[idx_b]     + chol_31*f_base$shock_base[idx_b]
  f_base$my_data.precios_ae[idx_b] <- f_base$my_data.precios_ae[idx_b] + chol_41*f_base$shock_base[idx_b]
  
  f_prol$my_data.epu_ae_new[idx_p] <- f_prol$my_data.epu_ae_new[idx_p] + chol_11*f_prol$shock_prol[idx_p]
  f_prol$my_data.spread_ae[idx_p]  <- f_prol$my_data.spread_ae[idx_p]  + chol_21*f_prol$shock_prol[idx_p]
  f_prol$my_data.pib_ae[idx_p]     <- f_prol$my_data.pib_ae[idx_p]     + chol_31*f_prol$shock_prol[idx_p]
  f_prol$my_data.precios_ae[idx_p] <- f_prol$my_data.precios_ae[idx_p] + chol_41*f_prol$shock_prol[idx_p]
  
  f_inte$my_data.epu_ae_new[idx_i] <- f_inte$my_data.epu_ae_new[idx_i] + chol_11*f_inte$shock_inte[idx_i]
  f_inte$my_data.spread_ae[idx_i]  <- f_inte$my_data.spread_ae[idx_i]  + chol_21*f_inte$shock_inte[idx_i]
  f_inte$my_data.pib_ae[idx_i]     <- f_inte$my_data.pib_ae[idx_i]     + chol_31*f_inte$shock_inte[idx_i]
  f_inte$my_data.precios_ae[idx_i] <- f_inte$my_data.precios_ae[idx_i] + chol_41*f_inte$shock_inte[idx_i]
}

############################################################
# 9.5 From time_fcast = 6 onward, no new EPU path is imposed
############################################################

# From period 6 onward, EPU evolves endogenously according to the reduced-form VAR.
# The forecasts still differ across scenarios because each scenario enters period 6
# with a different state vector inherited from periods 1 to 5.

for (i in 6:8) {
  
  # baseline
  f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == i)] <- phi1[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == i-1)] +phi1[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == i-1)]+phi1[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == i-1)]+phi1[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == i-1)]+phi1[5]
  f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == i)] <-  phi2[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == i-1)] +phi2[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == i-1)]+phi2[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == i-1)]+phi2[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == i-1)]+phi2[5]
  f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == i)] <-     phi3[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == i-1)] +phi3[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == i-1)]+phi3[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == i-1)]+phi3[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == i-1)]+phi3[5]
  f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == i)] <- phi4[1]*f_base$my_data.epu_ae_new[which(f_base$my_data.time_fcast == i-1)] +phi4[2]*f_base$my_data.spread_ae[which(f_base$my_data.time_fcast == i-1)]+phi4[3]*f_base$my_data.pib_ae[which(f_base$my_data.time_fcast == i-1)]+phi4[4]*f_base$my_data.precios_ae[which(f_base$my_data.time_fcast == i-1)]+phi4[5]
  
  # prolonged
  f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == i)] <- phi1[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == i-1)] +phi1[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == i-1)]+phi1[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == i-1)]+phi1[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == i-1)]+phi1[5]
  f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == i)] <-  phi2[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == i-1)] +phi2[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == i-1)]+phi2[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == i-1)]+phi2[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == i-1)]+phi2[5]
  f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == i)] <-     phi3[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == i-1)] +phi3[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == i-1)]+phi3[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == i-1)]+phi3[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == i-1)]+phi3[5]
  f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == i)] <- phi4[1]*f_prol$my_data.epu_ae_new[which(f_prol$my_data.time_fcast == i-1)] +phi4[2]*f_prol$my_data.spread_ae[which(f_prol$my_data.time_fcast == i-1)]+phi4[3]*f_prol$my_data.pib_ae[which(f_prol$my_data.time_fcast == i-1)]+phi4[4]*f_prol$my_data.precios_ae[which(f_prol$my_data.time_fcast == i-1)]+phi4[5]
  
  # intense
  f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == i)] <- phi1[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == i-1)] +phi1[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == i-1)]+phi1[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == i-1)]+phi1[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == i-1)]+phi1[5]
  f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == i)] <-  phi2[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == i-1)] +phi2[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == i-1)]+phi2[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == i-1)]+phi2[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == i-1)]+phi2[5]
  f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == i)] <-     phi3[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == i-1)] +phi3[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == i-1)]+phi3[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == i-1)]+phi3[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == i-1)]+phi3[5]
  f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == i)] <- phi4[1]*f_inte$my_data.epu_ae_new[which(f_inte$my_data.time_fcast == i-1)] +phi4[2]*f_inte$my_data.spread_ae[which(f_inte$my_data.time_fcast == i-1)]+phi4[3]*f_inte$my_data.pib_ae[which(f_inte$my_data.time_fcast == i-1)]+phi4[4]*f_inte$my_data.precios_ae[which(f_inte$my_data.time_fcast == i-1)]+phi4[5]
}

############################################################
# 10. Plot conditional and unconditional forecasts
############################################################

# GDP: conditional forecasts for different scenarios
plot(f_base$my_data.time_fcast, f_base$my_data.pib_ae, type = "l", pch = 19, col = "red", xlim=c(1, 8), ylim=c(-1.4, 0.4), main="GDP Conditional Forecasts", ylab="percentage points", xlab="forecast period (quarters)")
lines(f_prol$my_data.time_fcast, f_prol$my_data.pib_ae, pch = 18, col = "blue", type = "l", lty = 2, xlim=c(1, 8), ylim=c(-1.4, 0.4))
lines(f_inte$my_data.time_fcast, f_inte$my_data.pib_ae, pch = 18, col = "green", type = "l", lty = 3, xlim=c(1, 8), ylim=c(-1.4, 0.4))
legend("topright", legend=c("baseline scenario", "prolonged scenario", "intense scenario"),
       col=c("red", "blue", "green"), lty = 1:3, cex=0.8)

# GDP: conditional forecasts versus unconditional forecast
plot(f_base$my_data.time_fcast, f_base$my_data.pib_ae, type = "l", pch = 19, col = "red", xlim=c(1, 8), ylim=c(-1.4, 0.4), main="GDP Forecasts", ylab="percentage points", xlab="forecast period (quarters)")
lines(f_prol$my_data.time_fcast, f_prol$my_data.pib_ae, pch = 18, col = "blue", type = "l", lty = 2, xlim=c(1, 8), ylim=c(-1.4, 0.4))
lines(f_inte$my_data.time_fcast, f_inte$my_data.pib_ae, pch = 18, col = "green", type = "l", lty = 3, xlim=c(1, 8), ylim=c(-1.4, 0.4))
lines(f_inte$my_data.time_fcast, f_unco$my_data.pib_ae, pch = 18, col = "black", type = "b", lty = 4, xlim=c(1, 8), ylim=c(-1.4, 0.4))
legend("top", legend=c("cond.forecast:baseline scenario", "cond.forecast:prolonged scenario", "cond.forecast:intense scenario", "unconditional forecast"),
       col=c("red", "blue", "green", "black"), lty = 1:4, cex=0.8)

# If predicted pib_ae at t = 1 is -0.4, the model predicts GDP will contract by 0.4% qoq next quarter.

############################################################
# 11. Optional: Bayesian VAR conditional forecast
############################################################

# The section below shows a BVAR alternative.
# It is useful to explain that conditional forecasting can also be implemented
# using Bayesian VAR methods, where forecasts are based on posterior draws.

##########################
# CONDITIONAL FORECASTS WITH BVAR
##########################

bvaroutput <- bvar(
  myvar,
  lags = 1,
  n_draw = 30000L,
  n_burn = 5000L,
  verbose = TRUE,
  priors = bv_priors(hyper = "auto", mn = bv_mn(b = c(1, 1, 0.5, 0.5)))
)

# compute impulse responses from the BVAR
bvarirf <- irf(bvaroutput, bv_irf(horizon = 24L, identification = TRUE), n_thin = 5L)

# plot the IRF of GDP to an EPU shock
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(irf(bvaroutput), vars_response = "pib_ae", vars_impulse = "epu_ae_new")

# compute unconditional forecast from the BVAR
uncondpredict <- predict(bvaroutput)

# conditional forecast: impose a higher future path for EPU
condpredict <- predict(
  bvaroutput,
  cond_path = rep(unlist(myvar[dim(myvar)[1], 1] + 50), times = 5),
  cond_var = 1
)

# posterior mean coefficient matrix
posteriormean <- apply(bvaroutput$beta, c(2, 3), mean)

# posterior mean forecasts
condpred_mean   <- apply(condpredict$fcast, c(2, 3), mean)
uncondpred_mean <- apply(uncondpredict$fcast, c(2, 3), mean)

# plot conditional and unconditional BVAR forecasts
par(mfrow = c(2, 2), mar = c(2.2, 2.2, 2, 1), cex = 0.8)

var_labels <- c("EPU", "Spread", "GDP", "Prices")

for (i in 1:4) {
  yr <- range(c(condpred_mean[, i], uncondpred_mean[, i]), na.rm = TRUE)
  
  plot(1:12, condpred_mean[, i], type = "l", col = "red", lwd = 2, ylim = yr,
       main = var_labels[i], xlab = "Horizon", ylab = "")
  lines(1:12, uncondpred_mean[, i], col = "black", lwd = 2, lty = 2)
  
  legend("topright", legend = c("conditional", "unconditional"),
         col = c("red", "black"), lty = c(1, 2), lwd = 2, bty = "n")
}

# intuition: because EPU is higher in the conditional scenario, the model predicts lower GDP growth,
# a larger spread, and lower prices relative to the unconditional forecast.

############################################################
# End of class code 2
############################################################