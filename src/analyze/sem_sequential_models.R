# Dependencies
library(Hmisc)
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)

# Import data
redcap <- read_csv("data/results/redcap_subscales.csv")

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
  gims =~ NA*gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ NA*isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  gims ~~ 1*gims
  isos ~~ 1*isos
  dehum ~~ 1*dehum
'

# Fit the CFA
tdmhmo_cfa_fit_1 <- cfa(tdmhmo_cfa_1, data = redcap_scales_1, estimator = "MLM")

# Summarize the CFA
summary(tdmhmo_cfa_fit_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Specify the SEM
tdmhmo_sem_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5

# Second-order factor
  dehum =~ gims + isos

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
