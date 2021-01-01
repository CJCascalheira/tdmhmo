# Dependencies
library(Hmisc)
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)

# Import data
redcap <- read_csv("full_data_practice/data/results/redcap_subscales.csv")

# Select just the parcels and subscales
redcap_scales <- redcap %>%
  select(contains("factor"), contains("parcel"), mhi_5) %>%
  # Alphabetical variable order
  select(starts_with("gims"), starts_with("isos"), starts_with("mhi"), starts_with("pfq2s"),
         starts_with("sobbs"), starts_with("tis"))
redcap_scales

# MODEL 1 -----------------------------------------------------------------

# Select variables
redcap_scales_1 <- redcap_scales %>%
  select(starts_with(c("gims", "isos", "mhi")))
redcap_scales_1

# Specify CFA
tdmhmo_cfa_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_1 <- cfa(tdmhmo_cfa_1, data = redcap_scales_1, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_1,
  layout = "spring"
)

#################################################################################

# Specify the SEM
tdmhmo_sem_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum
'

# Fit the model to the data
tdmhmo_sem_fit_1 <- sem(
  model = tdmhmo_sem_1, 
  data = redcap_scales_1,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_1, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_1,
  layout = "spring"
)

# MODEL 2 -----------------------------------------------------------------

# Select variables
redcap_scales_2 <- redcap_scales %>%
  select(starts_with(c("gims", "isos", "mhi", "tis")))
redcap_scales_2

# Specify CFA
tdmhmo_cfa_2 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_2 <- cfa(tdmhmo_cfa_2, data = redcap_scales_2, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_2,
  layout = "spring"
)

#################################################################################

# Specify the SEM
tdmhmo_sem_2 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis
'

# Fit the model to the data
tdmhmo_sem_fit_2 <- sem(
  model = tdmhmo_sem_2, 
  data = redcap_scales_2,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_2, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_2,
  layout = "spring"
)

# MODEL 3 -----------------------------------------------------------------

# Select variables
redcap_scales_3 <- redcap_scales %>%
  select(starts_with(c("gims", "isos", "mhi", "tis", "sobbs")))
redcap_scales_3

# Specify CFA
tdmhmo_cfa_3 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_3 <- cfa(tdmhmo_cfa_3, data = redcap_scales_3, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_3, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_3,
  layout = "spring"
)

#################################################################################

# Specify the SEM - 3.1 w/o dehum predicting sobbs or tis
tdmhmo_sem_3_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Covariance between internalization LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs
'

# Fit the model to the data
tdmhmo_sem_fit_3_1 <- sem(
  model = tdmhmo_sem_3_1, 
  data = redcap_scales_3,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_3_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_3_1, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_3_1,
  layout = "tree2"
)

###################

# Specify the SEM - 3.2 w/ dehum predicting sobbs and tis
tdmhmo_sem_3_2 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Covariance between internalization LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
'

# Fit the model to the data
tdmhmo_sem_fit_3_2 <- sem(
  model = tdmhmo_sem_3_2, 
  data = redcap_scales_3,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_3_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_3_2, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_3_2,
  layout = "tree2"
)

# MODEL 4 -----------------------------------------------------------------

# Select variables
redcap_scales_4 <- redcap_scales %>%
  select(starts_with(c("gims", "isos", "mhi", "tis", "sobbs", "pfq2s")))
redcap_scales_4

# Specify CFA - 4.1 w/o error correlation, so therefore standard CFA
tdmhmo_cfa_4_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_4_1 <- cfa(tdmhmo_cfa_4_1, data = redcap_scales_4, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_4_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_4_1,
  layout = "spring"
)

####################

# Specify CFA - 4.2 w/ error correlation part I, so therefore nonstandard CFA
tdmhmo_cfa_4_2 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Error correlations (i.e., residual correlations)
  tis_factor_4 ~~ pfq2s_parcel_2
'

# Fit the CFA
tdmhmo_cfa_fit_4_2 <- cfa(tdmhmo_cfa_4_2, data = redcap_scales_4, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_4_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_4_2,
  layout = "spring"
)

####################

# Specify CFA - 4.3 w/ error correlation part II, nonstandard CFA
tdmhmo_cfa_4_3 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + p2*pfq2s_parcel_2 + p3*pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Error correlations (i.e., residual correlations)
  tis_factor_4 ~~ pfq2s_parcel_2 + pfq2s_parcel_3

# Equality constraints 
  p2 + p3 == 2
'

# Fit the CFA
tdmhmo_cfa_fit_4_3 <- cfa(tdmhmo_cfa_4_3, data = redcap_scales_4, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_4_3, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_4_3,
  layout = "spring"
)

#################################################################################

# Specify the SEM - 4.1 w/o dehum predicting sobbs, tis, or pfq2s
# Error correlation included
tdmhmo_sem_4_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Error correlations (i.e., residual correlations)
  tis_factor_4 ~~ pfq2s_parcel_2

# Covariance between internalization LVs
  tis ~~ sobbs
  pfq2s ~~ tis + sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
'

# Fit the model to the data
tdmhmo_sem_fit_4_1 <- sem(
  model = tdmhmo_sem_4_1, 
  data = redcap_scales_4,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_4_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4_1, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4_1,
  layout = "tree2"
)

###################

# Specify the SEM - 4.2 w/ dehum predicting sobbs, tis, and pfq2s
# Error correlation included
tdmhmo_sem_4_2 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Error correlations (i.e., residual correlations)
  tis_factor_4 ~~ pfq2s_parcel_2

# Covariance between internalization LVs
  tis ~~ sobbs
  pfq2s ~~ tis + sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
  pfq2s ~ DS*dehum
'

# Fit the model to the data
tdmhmo_sem_fit_4_2 <- sem(
  model = tdmhmo_sem_4_2, 
  data = redcap_scales_4,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_4_2, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4_2, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4_2,
  layout = "spring"
)

###################

# Specify the SEM - 4.3 w/ dehum predicted sobbs, tis, and pfq2s
# and pfq2s predicted by tis and sobbs as well
# Error correlation included
tdmhmo_sem_4_3 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Error correlations (i.e., residual correlations)
  tis_factor_4 ~~ pfq2s_parcel_2

# Covariance between internalization LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
  pfq2s ~ DS*dehum + ITS*tis + SOS*sobbs
'

# Fit the model to the data
tdmhmo_sem_fit_4_3 <- sem(
  model = tdmhmo_sem_4_3, 
  data = redcap_scales_4,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_4_3, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4_3, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4_3,
  layout = "spring"
)

###################

# Specify the SEM - 4.4 w/ dehum predicted sobbs, tis, and pfq2s
# and pfq2s predicted by tis and sobbs as well
# No error correlations
tdmhmo_sem_4_4 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Covariance between internalization LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
  pfq2s ~ DS*dehum + ITS*tis + SOS*sobbs
'

# Fit the model to the data
tdmhmo_sem_fit_4_4 <- sem(
  model = tdmhmo_sem_4_4, 
  data = redcap_scales_4,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_4_4, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4_4, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4_4,
  layout = "spring"
)

# FINAL MODEL -------------------------------------------------------------

# Select variables
redcap_scales_4 <- redcap_scales %>%
  select(starts_with(c("gims", "isos", "mhi", "tis", "sobbs", "pfq2s")))
redcap_scales_4

# Specify CFA using CFA Model 4.1 (i.e., standard CFA)
tdmhmo_cfa_4_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_4_1 <- cfa(tdmhmo_cfa_4_1, data = redcap_scales_4, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_4_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit_4_1,
  layout = "spring"
)

###################

# Specify the SEM using SEM Model 4.4
tdmhmo_sem_4_4 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Covariance between internalization LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
  pfq2s ~ DS*dehum + ITS*tis + SOS*sobbs
'

# Fit the model to the data
tdmhmo_sem_fit_4_4 <- sem(
  model = tdmhmo_sem_4_4, 
  data = redcap_scales_4,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit_4_4, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4_4, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4_4,
  layout = "spring"
)
