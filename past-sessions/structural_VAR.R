############################################################
# CLASS CODE 1
# Reduced-form VAR, Structural VAR, and Cholesky identification
############################################################

# Goal of this code:
# 1. Estimate a reduced-form VAR in R.
# 2. Interpret the reduced-form VAR coefficients.
# 3. Move from the reduced-form VAR to a structural VAR using a recursive ordering.
# 4. Place EPU first in the ordering, so that EPU is contemporaneously exogenous.
# 5. Estimate impulse responses to an EPU shock.

############################################################
# 0. Notation: slides versus R package notation
############################################################

# In the slides, we often write the structural VAR as:
#
#     y_t = c + Phi(L) y_{t-1} + P eps_t
#
# where:
# - y_t is the vector of endogenous variables
# - eps_t are structural shocks
# - Var(eps_t) = I
# - P is the contemporaneous impact matrix of structural shocks on variables
#
# In the R package vars, the SVAR is written in AB form:
#
#     A0 y_t = c + Phi(L) y_{t-1} + B e_t
#
# where:
# - A0 captures contemporaneous relations among variables
# - B captures how structural shocks enter the system
# - e_t are structural shocks with Var(e_t) = I
#
# Multiplying by A0^{-1}, we obtain:
#
#     y_t = A0^{-1} c + A0^{-1} Phi(L) y_{t-1} + A0^{-1} B e_t
#
# Therefore, the impact matrix of structural shocks is:
#
#     P = A0^{-1} B
#
# This is the matrix that tells us the contemporaneous effect of each structural shock
# on each variable in the system.

############################################################
# 1. Start clean and load packages
############################################################

# VAR model 
rm(list = ls()) 

### type in the console if needed:
### install.packages("vars")
### install.packages("readxl")

# packages
library("vars")
library("readxl")

############################################################
# 2. Set working directory and load data
############################################################

# Set working directory
setwd("C:/Users/User/Dropbox/Research_and_Policy/MiUnidad/Teaching/BSE_Álvaro/Code")
getwd()

# load the Excel file into an R object called my_data
my_data <- read_excel("datafinal_varspain.xlsx")

# rename the columns of my_data with simpler variable names
colnames(my_data) <- c("qdate", "pib", "iapc", "spread", "epu", "Vix")

# display the dataset in the console
print(my_data)

############################################################
# 3. Select variables and define the VAR ordering
############################################################

# We now choose the variables used in the VAR.
#
# Ordering:
# 1. epu
# 2. spread
# 3. pib
# 4. iapc
#
# The ordering matters for the recursive / Cholesky identification.
# Since EPU is ordered first, an EPU shock can affect all variables contemporaneously.
# In contrast, the other shocks do not affect EPU contemporaneously.

varepu <- subset(my_data, select = c("epu", "spread", "pib", "iapc"))

# print selected variables
print(varepu)

# show descriptive statistics
summary(varepu)

# plot the selected variables as time series if desired
# plot.ts(varepu)

############################################################
# 4. Estimate the reduced-form VAR
############################################################

# The reduced-form VAR(1) is:
#
#     y_t = c + Phi_1 y_{t-1} + u_t
#
# where:
# - y_t = (epu_t, spread_t, pib_t, iapc_t)'
# - c is a vector of constants
# - Phi_1 is the matrix of lag coefficients
# - u_t are reduced-form residuals
#
# The reduced-form residuals u_t are generally correlated across equations.
# This is why we later need an identification strategy to recover structural shocks.

var.est1 <- VAR(varepu, p = 1, type = "const", season = NULL)

# display reduced-form VAR results
summary(var.est1)

############################################################
# 5. Display reduced-form VAR coefficients
############################################################

# These objects are useful for writing the reduced-form VAR equation by equation.

# variable names in the VAR
var_names <- colnames(varepu)

# constant vector
const <- sapply(var.est1$varresult, function(x) coef(x)["const"])

# lag coefficient matrix Phi_1
Phi1 <- Acoef(var.est1)[[1]]

# reduced-form residual variance-covariance matrix
Sigma_u <- summary(var.est1)$covres

cat("\nConstant vector:\n")
print(round(const, 4))

cat("\nLag coefficient matrix Phi_1:\n")
print(round(Phi1, 4))

cat("\nReduced-form residual variance-covariance matrix Sigma_u:\n")
print(round(Sigma_u, 4))

# Write the reduced-form VAR equation by equation in the console.
# Each equation has the form:
#
#     variable_t = constant + coefficients on lagged variables + reduced-form residual

cat("\nEstimated reduced-form VAR(1):\n")

for (i in seq_along(var_names)) {
  
  lhs <- paste0(var_names[i], "_t")
  eq_line <- paste0(lhs, " = ", round(const[i], 4))
  
  for (j in seq_along(var_names)) {
    
    coef_ij <- round(Phi1[i, j], 4)
    
    if (coef_ij >= 0) {
      eq_line <- paste0(eq_line, " + ", coef_ij, " * ", var_names[j], "_{t-1}")
    } else {
      eq_line <- paste0(eq_line, " - ", abs(coef_ij), " * ", var_names[j], "_{t-1}")
    }
  }
  
  eq_line <- paste0(eq_line, " + u_", var_names[i], ",t")
  cat("\n", eq_line, "\n", sep = "")
}

############################################################
# 6. Move from the reduced-form VAR to the structural VAR
############################################################

# The structural VAR in AB form is:
#
#     A0 y_t = c + Phi_1 y_{t-1} + B e_t
#
# where:
# - A0 describes contemporaneous relations among variables
# - B describes the contemporaneous scale of structural shocks
# - e_t are structural shocks, normalized so that Var(e_t) = I
#
# The impact matrix of structural shocks is:
#
#     P = A0^{-1} B
#
# The recursive / Cholesky identification imposes a lower-triangular structure.
# With the ordering epu, spread, pib, iapc:
#
# - EPU is ordered first and does not respond contemporaneously to the other variables.
# - Spread can respond contemporaneously to EPU.
# - GDP can respond contemporaneously to EPU and spread.
# - Inflation can respond contemporaneously to EPU, spread, and GDP.
#
# This is the identifying assumption.

############################################################
# 7. Specify the contemporaneous restriction matrix A0
############################################################

# create a 4x4 identity matrix as the starting point
a.mat <- diag(4)

# replace the diagonal with NA so these coefficients are freely estimated
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

# print restriction matrix
print(a.mat) 

# Interpretation:
# 0  means: this contemporaneous effect is restricted to zero.
# NA means: this coefficient is freely estimated.
#
# Because the NAs are on and below the diagonal, this is a recursive ordering.

############################################################
# 8. Specify matrix B for structural shocks
############################################################

# create a 4x4 diagonal matrix as the starting point
b.mat <- diag(4)

# replace the diagonal 1s with NA so the shock scales can be estimated
diag(b.mat) <- NA

# print B restriction matrix
print(b.mat)

# Interpretation:
# - B is diagonal.
# - This means structural shocks are assumed to be orthogonal.
# - Each shock enters directly only its own equation in the AB representation.

############################################################
# 9. Estimate the structural VAR
############################################################

svar.one <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, hessian = TRUE)

# var.est1        = reduced-form VAR
# Amat = a.mat    = contemporaneous restrictions
# Bmat = b.mat    = shock restrictions
# max.iter=10000  = maximum number of iterations in optimization
# hessian=TRUE    = compute Hessian for inference

# print estimated SVAR
svar.one

############################################################
# 10. Compute the structural impact matrix P = A0^{-1} B
############################################################

# Extract estimated A0 and B from the SVAR object
A0_est <- svar.one$A
B_est  <- svar.one$B

cat("\nEstimated A0 matrix:\n")
print(round(A0_est, 4))

cat("\nEstimated B matrix:\n")
print(round(B_est, 4))

# Compute impact matrix:
# P = A0^{-1} B
P_est <- solve(A0_est) %*% B_est

rownames(P_est) <- var_names
colnames(P_est) <- paste0("shock_", var_names)

cat("\nStructural impact matrix P = A0^{-1} B:\n")
print(round(P_est, 4))

# The first column of P gives the contemporaneous impact of an EPU shock:
cat("\nImpact effects of a one-unit structural EPU shock:\n")
print(round(P_est[, 1, drop = FALSE], 4))

# Interpretation:
# The first element shows how much EPU itself moves on impact.
# The second element shows the contemporaneous effect on the spread.
# The third element shows the contemporaneous effect on GDP.
# The fourth element shows the contemporaneous effect on inflation.
#
# Because EPU is ordered first, the EPU shock can affect all variables contemporaneously.
# Other shocks cannot affect EPU contemporaneously.

############################################################
# 11. Estimate impulse response to an EPU shock
############################################################

# Estimate the impulse response function from the SVAR.
# Here we trace the response of GDP to an EPU shock.

one.gdp <- irf(svar.one, response = "pib", impulse = "epu", n.ahead = 10, ortho = TRUE, boot = TRUE)

# Arguments:
# svar.one         = estimated structural VAR
# response = "pib" = variable whose response we plot
# impulse = "epu"  = shock applied to the system
# n.ahead = 10     = number of future periods
# ortho = TRUE     = use orthogonalized shocks
# boot = TRUE      = compute bootstrap confidence intervals

############################################################
# 12. Plot impulse response
############################################################

# set graphical options
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)

# plot estimated impulse response
plot(one.gdp)

# print IRF object in console
one.gdp

############################################################
# End of class code 1
############################################################

summary(var.est1)
