# Slide 1

Real-Time Macroeconomic Analysis with Text as Data: VARs
Álvaro Fernández-Gallardo
Banco de España
Barcelona School of Economics
May 26, 2026
Barcelona
All views expressed in these slides are the author’s and do not necessarily represent the views of the Banco de España or the Eurosystem.
1/31


# Slide 2

Road map of the session
1 Vector Autoregression (VAR): introduction
2 Structural VAR: introduction
3 Structural forecasts
4 Appendix: identification details
Main objective
Learn how to use reduced-form VARs and structural VARs to produce forecasts, estimate
impulse response functions, and construct structural scenario forecasts.
2/31


# Slide 3

Road map
1 Vector Autoregression (VAR): introduction
2 Structural VAR: introduction
3 Structural forecasts
4 Appendix
3/31


# Slide 4

What is a vector autoregression?
A vector autoregression (VAR) is adynamic, multivariatemodel.
Dynamic: it models the evolution of variables over time.
Multivariate: it models several variables jointly.
VARs are useful for economists because they allow us to forecast a system of related variables.
Example: how will GDP growth and inflation evolve in the next quarters, taking into account their
interdependence?
Variables used in the code
epu_ae_new: euro area Economic Policy Uncertainty index, in levels.
spread_ae: Spanish-German 10-year sovereign yield spread.
pib_ae: euro area GDP, quarter-on-quarter growth rate.
precios_ae: euro area HICP inflation, quarter-on-quarter growth rate.
4/31


# Slide 5

Reduced-form VAR(1): notation used in the code
The reduced-form VAR(1) estimated in the R code is
yt =c+A 1yt−1 +u t ,
where
yt =


et
st
gt
πt

 =


epu_ae_newt
spread_aet
pib_aet
precios_aet

.
c=


ce
cs
cg
cπ

,A 1 =


aee aes aeg aeπ
ase ass asg asπ
age ags agg agπ
aπe aπs aπg aππ

.
E(ut ) =0,E(u t u′
t ) = Σu.
The coefficientscandA 1 are estimated equation by equation by OLS.
The reduced-form residualsut are forecast errors, but not structural shocks.
5/31


# Slide 6

Reduced-form VAR(1): unconditional forecasting
Given the estimated reduced-form VAR,
yt =c+A 1yt−1 +u t ,
the one-step-ahead forecast is
byT+1|T = bc+ bA1yT ,
because
ET (uT+1) =0.
For later horizons, the forecast is computed recursively:
byT+h|T = bc+ bA1byT+h−1|T ,h=2,3, . . . ,H.
Equivalently,
byT+h|T =
h−1X
j=0
bAj
1bc+ bAh
1yT .
Interpretation
The model uses past observed relationships among EPU, spread, GDP growth, and inflation.
Future reduced-form residuals are set to zero in expectation.
This is the forecast implemented in the code asf_unco.
6/31


# Slide 7

Reduced-form conditional forecasting
Conditional forecasts impose a future path for one or more variables.
Example: given a specific path for interest rates, oil prices, or exchange rates, what is the most
likely path for GDP and inflation?
This is the logic behind technical assumptions in institutional forecasts.
The source of the imposed path is not necessarily identified as a structural shock.
7/31


# Slide 8

Reduced-form conditional forecasting: scenarios
Adverse and severe scenarios use different assumptions about commodity prices, energy
disruptions, and geopolitical tensions.
These are forecasts under different technical assumptions.
They answer questions such as:
Given a path for energy prices, what is the likely path for GDP and inflation?
ECB projections:
https://www.ecb.europa.eu/pub/pdf/other/ecb.projections202203_ecbstaff~44f998dfd7.en.pdf 8/31


# Slide 9

Reduced-form conditional forecasting: why more difficult?
Suppose
yt =
πt
ot

,
whereπ t is inflation andot is the oil price.
The reduced-form VAR is
yt =c+A 1yt−1 +u t .
An unconditional forecast sets future reduced-form residuals to zero:
byT+h|T = bc+ bA1byT+h−1|T .
A conditional forecast imposes a future oil-price path:
oscenario
T+1 ,o scenario
T+2 , . . . ,o scenario
T+H .
Key point
We cannot simply apply the unconditional recursion to all variables.
The forecast must be adjusted so that the oil-price equation hits the imposed path.
This is why conditional forecasts are computationally more intensive.
References: Waggoner and Zha (1999); Banbura, Giannone, and Lenza (2015).
9/31


# Slide 10

Road map
1 Vector Autoregression (VAR): introduction
2 Structural VAR: introduction
3 Structural forecasts
4 Appendix
10/31


# Slide 11

From reduced-form VAR to structural VAR
The reduced-form VAR is
yt =c+A 1yt−1 +u t ,E(u t u′
t ) = Σu.
The structural representation is
yt =c+A 1yt−1 +Pε t ,E(ε t ε′
t ) =I.
Therefore,
ut =Pε t ,Σ u =PP ′.
What changes?
ut : reduced-form forecast errors, generally correlated across equations.
εt : structural shocks, orthogonal and economically interpretable.
P: impact matrix mapping structural shocks into variables.
In the code
P=A −1
0 B,
whereA 0 andBcome from the AB representation estimated bySVAR().
11/31


# Slide 12

Impulse response functions
An impulse response function traces the effect of a structural shock over time.
For an EPU shock, we focus on the first structural shock:
εe
t .
The impact effect is the first column ofP:
IRFe (0) =P ·,1 =


p11
p21
p31
p41

.
At later horizons, the response is propagated by the reduced-form dynamics:
IRFe (h) =A h
1P·,1,h=1,2, . . . ,H.
Interpretation
P·,1 determines the contemporaneous effect of the EPU shock.
A1 determines how the effect propagates over time.
This is what the code estimates withirf(svar.one, impulse = "epu_ae_new").
12/31


# Slide 13

Identification in a structural VAR
The reduced-form covariance matrixΣu is estimated from the VAR.
But many different structural impact matricesPcan satisfy:
Σu =PP ′.
Therefore, we need identifying restrictions to recover structural shocks.
Common identification strategies
Recursive identification / Cholesky restrictions.
Sign restrictions.
External instruments.
Other restrictions motivated by economic theory.
In this class
We use recursive identification: the ordering of variables determines which shocks can affect which
variables contemporaneously.
13/31


# Slide 14

Recursive identification: the EPU VAR
We use the ordering
epu_ae_new,spread_ae,pib_ae,precios_ae.
Recursive identification imposes that the impact matrix is lower triangular:
P=


p11 0 0 0
p21 p22 0 0
p31 p32 p33 0
p41 p42 p43 p44

.
Interpretation
EPU shocks can affect all variables contemporaneously.
Spread shocks can affect spread, GDP, and inflation contemporaneously, but not EPU.
GDP shocks can affect GDP and inflation contemporaneously, but not EPU or spread.
Inflation shocks affect only inflation contemporaneously.
The ordering matters.
14/31


# Slide 15

Cholesky identification in the R code
In the R code, the SVAR is estimated in AB form:
A0yt =c+A 1yt−1 +Bε t .
Multiplying byA −1
0 :
yt =A −1
0 c+A −1
0 A1yt−1 +A −1
0 Bεt .
Therefore, the structural impact matrix is
P=A −1
0 B.
In the code:
P = solve(svar.one$A) %*% svar.one$B.
Equivalent Cholesky representation
Under recursive identification,
Σu =PP ′,
soPcan also be recovered as the lower-triangular Cholesky factor ofΣu.
15/31


# Slide 16

Road map
1 Vector Autoregression (VAR): introduction
2 Structural VAR: introduction
3 Structural forecasts
4 Appendix
16/31


# Slide 17

Conditional reduced-form forecast versus conditional structural forecast
Reduced-form conditional forecast
What is the likely path of output, given that EPU follows a specific path?
We impose a future path for EPU.
The source of the EPU movement is not specified.
Conditional Structural forecast
What is the likely path of output if EPU shocks generate that EPU path?
We impose a future path for EPU.
We also specify that the path is generated by identified EPU shocks.
This uses the structural impact matrixP.
In the homework:impose an EPU path and compute the implied forecasts for the real activity variables.
17/31


# Slide 18

Reminder
Core distinction
Unconditional reduced-form forecast: the economist lets all VAR variables evolve endogenously.
Conditional reduced-form forecast: the economist imposes a future path for EPU and forecasts
the remaining variables conditional on that path, without specifying which shock makes EPU
follow that path.
Conditional structural forecast: the economist imposes a future path for EPU and assumes that
this path is generated by identified EPU shocks.
18/31


# Slide 19

Conditional structural forecast: implementation logic
At each horizon, the code starts from areduced-form benchmark forecast:
byRF,s
T+h|T = bc+ bA1bystruct,s
T+h−1|T .
Forh=1, this is the usual unconditional forecast:
byRF,s
T+1|T = bc+ bA1yT .
For later horizons, the forecast uses the previous scenario-specific structural forecast as the lagged input.
Interpretation
At each step, we first ask: what would the VAR predict before imposing the EPU scenario path?
19/31


# Slide 20

Conditional structural forecast: EPU shock
Let
yt =


EPUt
spreadt
GDPt
πt

,P ·,1 =


p11
p21
p31
p41

.
Herep 11 is the impact of the EPU shock on EPU,p21 on the spread,p31 on GDP growth, andp41 on inflation.
The imposed EPU gap is:
∆EPUs
T+h = EPUscenario,s
T+h −dEPURF,s
T+h|T .
Sincep 11 is the impact effect on EPU:
εEPU,s
T+h =
∆EPUs
T+h
p11
.
Using the normalized first column:
eP·,1 =


1
p21/p11
p31/p11
p41/p11

.
20/31


# Slide 21

Conditional structural forecast: adjustment
Example
Suppose:
EPUscenario,s
T+h =250, dEPURF,s
T+h|T =200.
Then:
∆EPUs
T+h =250−200=50.
With normalizedP, the EPU shock is 50 EPU-index points.
The conditional structural forecast is:
bystruct,s
T+h|T = byRF,s
T+h|T +


1
p21/p11
p31/p11
p41/p11

∆EPUs
T+h .
21/31


# Slide 22

Example: EPU scenarios and output forecasts
Objective
Assess how alternative future paths of economic policy uncertainty affect output through identified EPU shocks.
Model
Four endogenous variables:
yt =


epu_ae_newt
spread_aet
pib_aet
precios_aet

.
VAR(1) estimated on quarterly data.
epu_ae_newordered first in the recursive identification.
Identification
EPU shocks can affect all variables contemporaneously.
Other shocks do not affect EPU contemporaneously.
The structural forecast assumes that deviations of EPU from the baseline forecast are generated by EPU shocks.
Sources: Economic Policy Uncertainty, Eurostat, and Banco de España.
22/31


# Slide 23

Scenario analysis: timing
The model is estimated at quarterly frequency because GDP is quarterly.
EPU is available at monthly frequency.
This matters for scenario design:
the shock may occur within the quarter;
the quarterly EPU value averages months before and after the event;
the first-quarter effect may therefore be smaller than a pure monthly shock suggests.
Lesson
The timing and frequency of the data matter when translating real-time events into quarterly scenarios.
Sources: Economic Policy Uncertainty, Eurostat, and Banco de España.
23/31


# Slide 24

Three scenarios with different paths for EPU
1 Baseline: counterfactual no-invasion path.
2 Intense: large but short-lived increase in EPU.
3 Prolonged: smaller initial increase, but more persistent.
From horizon 6 onward, EPU evolves endogenously under the VAR.
Forecasting logic
For horizons 1–5, impose the EPU scenario path.
Compute the required EPU shock.
Feed that shock through the first column ofeP.
After that, let the VAR propagate the new state
endogenously.
 Date Baseline Intense Prolonged
2022Q1 247.5 284.667 284.667
2022Q2 247.5 359.0 303.25
2022Q3 247.5 303.25 288.85
2022Q4 247.5 247.5 274.45
2023Q1 247.5 247.5 260.25
24/31


# Slide 25

Methodology: scenario implementation
Conditional forecasts use the last observed data point as the initial condition.
The model first computes a reduced-form benchmark forecast.
Then EPU shocks are chosen so that EPU follows the imposed scenario path.
Since the model is linear, differences in EPU levels across scenarios translate into differences in
GDP growth and other variables.
Main idea
Start from the reduced-form forecast, then add the structural effect of the EPU shock needed to hit the
imposed EPU path.
25/31


# Slide 26

Methodology: scenario implementation
The conditional structural forecast for scenariosis
bystruct,s
T+h|T = byRF,s
T+h|T + eP·,1

EPUscenario,s
T+h −dEPURF,s
T+h|T

| {z }
∆EPUs
T+h
where
eP·,1 =


1
p21/p11
p31/p11
p41/p11

.
byRF,s
T+h|T : reduced-form benchmark forecast.
EPUscenario,s
T+h : imposed EPU value in scenarios.
dEPURF,s
T+h|T : reduced-form forecast of EPU.
∆EPUs
T+h : EPU shock in EPU-index units.
eP·,1: normalized impact effects of the EPU shock.
26/31


# Slide 27

Methodology: scenario implementation
27/31


# Slide 28

Road map
1 Vector Autoregression (VAR): introduction
2 Structural VAR: introduction
3 Structural forecasts
4 Appendix
28/31


# Slide 29

Appendix: identification problem
Reduced-form VAR:
yt =c+A 1yt−1 +u t ,E(u t u′
t ) = Σu.
Structural VAR:
yt =c+A 1yt−1 +Pε t ,E(ε t ε′
t ) =I.
Therefore,
ut =Pε t ,Σ u =PP ′.
Identification problem
Σu is estimated from the reduced-form VAR.
Pis not uniquely determined byΣu =PP ′.
We need restrictions to identifyP.
29/31


# Slide 30

Appendix: AB representation in the R package
Thevarspackage estimates the SVAR in AB form:
A0yt =c+A 1yt−1 +Bε t .
Multiplying byA −1
0 , we get
yt =A −1
0 c+A −1
0 A1yt−1 +A −1
0 Bεt .
Hence, the structural impact matrix is
P=A −1
0 B.
Connection with the code
P = solve(svar.one$A) %*% svar.one$B.
Connection with Cholesky
Under recursive identification:
Σu =PP ′,
soPis the lower-triangular Cholesky factor ofΣu.
30/31


# Slide 31

Appendix: Cholesky identification
With four variables,
Σu =


σ11 σ12 σ13 σ14
σ12 σ22 σ23 σ24
σ13 σ23 σ33 σ34
σ14 σ24 σ34 σ44

.
BecauseΣ u is symmetric, it contains 10 distinct elements.
Without restrictions,Phas 16 unknown elements:
P=


p11 p12 p13 p14
p21 p22 p23 p24
p31 p32 p33 p34
p41 p42 p43 p44

.
Cholesky identification imposes 6 zero restrictions:
P=


p11 0 0 0
p21 p22 0 0
p31 p32 p33 0
p41 p42 p43 p44

.
NowPhas 10 free elements, matching the 10 distinct elements inΣu.
31/31
