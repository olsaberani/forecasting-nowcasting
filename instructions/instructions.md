# Page 1

Homework Instructions
Homework 2 can be completed either individually or in groups of two. Please indicate clearly
in your submission whether the homework is submitted individually or by a group of two.
Homework 2 revolves around the use of the U.S. Economic Policy Uncertainty indicator
in combination with a VAR.
When you submit your homework answers, please include the written answers and plots
in a single PDF. In addition, please submit a separate folder containing the code and data.
Whenever you are asked to plot the data, please include the plot in the PDF that you
submit. The figures must be clearly labelled and must contain a figure note describing what
the reader sees in the figure. In other words, the figure and the figure note should be sufficient
for any reader to understand what is shown in the figure and how the results were produced.
This will be part of the evaluation of Homework 2.
•Deadline:19 June 2026 at 10:00am
Part I
1. Construct the dataset. The fileDataFigure.pngshows how the series should look if
you downloaded them correctly.
a. Download the U.S. Monthly EPU Index frompolicyuncertainty.com.
b. Download the monthly non-farm employment, personal consumption expenditures
price index, and industrial production series fromhttps://fred.stlouisfed.org/.
i. Download the series in levels and seasonally adjusted.
ii. The mnemonics to find the series arePAYEMS,PCEPI, andINDPRO.
iii. Download the series from 1985M1 to 2023M3.
2. Estimate a VAR with one lag using the following ordering of variables:
News Based Policy Uncert Index,PAYEMS,INDPRO,PCEPI.
The first variable is the Economic Policy Uncertainty index.
a. The variablesPAYEMS,INDPRO, andPCEPIenter the model in month-on-month
growth rates, i.e.,
100×
 Xt
Xt−1
−1

.
b. The EPU index enters the model in levels.
c. Estimate the model using only data from 1985M2 to 2019M12.
d. Report the estimated reduced-form VAR(1). Write down the four estimated equa-
tions, including the estimated constant and lag coefficients.
e. Report the estimated reduced-form variance-covariance matrix of the residuals.
1


# Page 2

3. Using the same VAR, estimate impulse response functions for a shock to the Economic
Policy Uncertainty index.
a. Plot the impulse response functions for the month-on-month growth rates of em-
ployment and industrial production, including 95% confidence bands.
b. Include the plots in your answer PDF and describe what you see. How do em-
ployment growth and industrial production growth react to an economic policy
uncertainty shock?
Part II
Imagine you work in a central bank or a consulting firm and you are asked to provide forecasts
for U.S. industrial production and employment.
1. Using the same model as in Part I, estimated on data from 1985 to 2019, produce
unconditional forecasts for 12 horizons ahead using March 2023 as the latest data
point.
a. Plot the forecasts for month-on-month employment growth and month-on-month
industrial production growth.
b. Briefly describe the forecast trajectory of both variables. In particular, discuss
whether the forecasts predict positive or negative growth, and whether growth
increases or decreases over the forecast horizon.
2. Imagine there were national U.S. elections in May 2023. Produce a structural forecast,
assuming that the paths of the U.S. EPU index in April, May, June, July, and August
2023 are the same as in October 2020, November 2020, December 2020, January 2021,
and February 2021, respectively. These months correspond to the period before, during,
and after the last U.S. national elections.
a. Use the ordering of the variables described above, i.e.,
News Based Policy Uncert Index,PAYEMS,INDPRO,PCEPI.
The EPU index is ordered first.
b. For all variables other than the EPU index, the last observed value should be
March 2023.
c. Plot the forecasts for industrial production and employment.
3. Are the forecasts from parts (1) and (2) different? If so, why? What does the structural
forecast mean? Please provide a concise answer.
4. Point out three shortcomings of the analysis conducted in parts (1) to (3). Present
them as bullet points. The shortcomings can be related to any aspect of the exer-
cise, including the model type, the construction of the EPU index, the identification
assumptions, or the forecasting strategy.
2
