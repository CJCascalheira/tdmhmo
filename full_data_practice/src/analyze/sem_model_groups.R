# Dependencies
library(Hmisc)
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)

# Import data
redcap <- read_csv("full_data_practice/data/results/redcap_subscales.csv")
cleaned <- read_csv("full_data_practice/data/raw/redcap_cleaned_group.csv")
  
# Select just the parcels and subscales
redcap_scales_0 <- redcap %>%
  select(record_id, contains("factor"), contains("parcel"), mhi_5) %>%
  # Alphabetical variable order
  select(record_id, starts_with("gims"), starts_with("isos"), starts_with("mhi"), starts_with("pfq2s"),
         starts_with("sobbs"), starts_with("tis"))
redcap_scales_0

# Combine data frame with group names
redcap_scales <- cleaned %>%
  select(record_id, group_name) %>%
  left_join(redcap_scales_0) %>%
  # Drop record ID
  select(-record_id)
redcap_scales 

# MEASUREMENT MODEL -------------------------------------------------------

# Specify CFA using CFA Model 4.1 (i.e., standard CFA)
tdmhmo_cfa <- '
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
tdmhmo_cfa_fit <- cfa(tdmhmo_cfa, data = redcap_scales, estimator = "MLM", group = "group_name")

# Summarize the CFA
summary(tdmhmo_cfa_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit,
  layout = "spring"
)

# STRUCTURAL MODEL --------------------------------------------------------

# Specify the SEM using SEM Model 4.4
tdmhmo_sem <- '
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
tdmhmo_sem_fit <- sem(
  model = tdmhmo_sem, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM",
  group = "group_name"
)

# Summarize the SEM
summary(tdmhmo_sem_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit,
  layout = "spring"
)
