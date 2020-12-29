# Dependencies
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)

# Import data
redcap <- read_csv("data/results/redcap_subscales.csv")

# Select just the parcels and subscales
redcap_scales <- redcap %>%
  select(contains("factor"), contains("parcel")) %>%
  # Alphabetical variable order
  select(starts_with("gims"), starts_with("isos"), starts_with("mhi"), starts_with("pfq2s"),
         starts_with("sobbs"), starts_with("tis"))
redcap_scales

# MEASUREMENT MODEL -------------------------------------------------------

# Use CFA to determine if the LVs fit the data and to derive the 
# correlations among LVs.

# CONFIRMATORY FACTOR ANALYSIS --------------------------------------------

# Vector of standard deviations
tdmhmo_sd_vector <- redcap_scales %>%
  gather(key = "measure", value = "score") %>%
  group_by(measure) %>%
  summarize(
    sd = sd(score)
  ) %>%
  select(sd) %>%
  pull()
tdmhmo_sd_vector

# Correlation matrix
tdmhmo_cor_matrix <- cor(redcap_scales)
tdmhmo_cor_matrix

# Create covariance matrix
tdmhmo_cov <- cor2cov(tdmhmo_cor_matrix, tdmhmo_sd_vector)
tdmhmo_cov

# Latent variable model w/o second-order
tdmhmo_model <- '
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2
'

# Latent variable model w/ second-order
tdmhmo_model_1 <- '
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2

# Second-order factor
  dehum =~ gims + isos
'

#######
# Note that, as of 12/28/2020, the cfa() function does the following automatically:
# 1) The first observed indicator of each latent variable is set to 1.0 to scale the LVs
# 2) Residuals variances are added
# 3) LVs are correlated by default and covariances are added
#######

# Fit the data to the model using CFA
tdmhmo_fit <- cfa(
  model = tdmhmo_model,
  # Complete data necessary
  data = redcap_scales,
  sample.cov = tdmhmo_cov,
  sample.nobs = 583,
  std.lv = FALSE,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Second-order model fit
tdmhmo_fit_1 <- cfa(
  model = tdmhmo_model_1,
  # Complete data necessary
  data = redcap_scales,
  sample.cov = tdmhmo_cov,
  sample.nobs = 583,
  std.lv = FALSE,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Different between the two models?
anova(tdmhmo_fit, tdmhmo_fit_1)
compareFit(tdmhmo_fit, tdmhmo_fit_1)

# Summarize the model
summary(tdmhmo_fit_1, standardized = TRUE, fit.measures = TRUE)

# Visualize the measurement model
semPaths(tdmhmo_fit_1)

# SIGNIFICANT INTERCORRELATIONS AMONG LATENT VARIABLES --------------------

# Correlations among the latent variables (LVs)
lv_cor_matrix <- lavInspect(tdmhmo_fit_1, what = "cor.lv")
lv_cor_matrix

# Matrix to data fram
lv_cor_df <- lv_cor_matrix %>%
  as.data.frame() %>%
  as_tibble()
lv_cor_df

# Calculate t-value cutoff for two-tailed significant correlation 
t_cutoff <- qt(0.05/2, df = 581) %>%
  # Take the absolute value
  abs()
t_cutoff 

# Function to calculate critical t-value
t_stat_cor <- function(r) {
  (r * sqrt(583 - 2)) / (sqrt(1 - r^2))
}

# Calculate t-values for all correlations among LVs
lv_t_values <- map_df(lv_cor_df, t_stat_cor)
lv_t_values

# Calculate p-values manually
# GIMS
pt(lv_t_values$gims[2], df = 581, lower.tail = FALSE)
pt(lv_t_values$gims[3], df = 581, lower.tail = FALSE)
pt(lv_t_values$gims[4], df = 581, lower.tail = FALSE)
pt(lv_t_values$gims[5], df = 581, lower.tail = FALSE)
pt(lv_t_values$gims[6], df = 581, lower.tail = TRUE)
# ISOS
pt(lv_t_values$isos[3], df = 581, lower.tail = FALSE)
pt(lv_t_values$isos[4], df = 581, lower.tail = FALSE)
pt(lv_t_values$isos[5], df = 581, lower.tail = FALSE)
pt(lv_t_values$isos[6], df = 581, lower.tail = TRUE)

# Therefore, since all values are significant at the p < .001 thus far and t = 2.2 is the lowest
# t-value, all remaining intercorrelations among LVs are significant at the p < .001 level.

# STRUCTURAL MODEL 1 ------------------------------------------------------

# Specify the SEM
tdmhmo_sem <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2
  
# Second-order factor
  dehum =~ gims + isos

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ DS*dehum + SOS*sobbs + ITS*tis
  sobbs ~ DSO*dehum
  tis ~ DIT*dehum
  
# Indirect effects
  mhi_s := SM * DS
  mhi_so := SOM * DSO
  mhi_it := ITM * DIT
  
# Total effect
  total := DM + (SM * DS) + (SOM * DSO) + (ITM * DIT)
'

# Fit the model to the data
tdmhmo_sem_fit <- sem(
  model = tdmhmo_sem, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit, standardized = TRUE, fit.measures = TRUE)

# Visualize the SEM
semPaths(tdmhmo_sem_fit)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit, what = "cor.lv")

# STRUCTURAL MODEL 2 - DIAGNOSE HEYWOOD CASE ------------------------------

# Retain the second-order factor and respecify for the Heywood case

# Locate the Heywood case
summary(tdmhmo_sem_fit, standardized = TRUE, fit.measures = TRUE,
        # Show the R^2 to identify the Heywood case per Erin Buchanan (see lab notebook)
        rsquare = TRUE)

# What is the variance of the Heywood case?
var(redcap_scales$tis_factor_4)

# Update the model to correct for this variance
tdmhmo_sem_2 <- '
# Define the measurement model

# First-order latent variables
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2
  
# Second-order latent variable
  dehum =~ gims + isos
  
# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ DS*dehum + SOS*sobbs + ITS*tis
  sobbs ~ DSO*dehum
  tis ~ DIT*dehum
  
# Indirect effects
  mhi_s := SM * DS
  mhi_so := SOM * DSO
  mhi_it := ITM * DIT
  
# Total effect
  total := DM + (SM * DS) + (SOM * DSO) + (ITM * DIT)
  
# Constrain the variance for the Heywood case
  tis_factor_4 ~~ 2.484459*tis_factor_4
'

# Fit the model to the data
tdmhmo_sem_fit_2 <- sem(
  model = tdmhmo_sem_2, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_2, standardized = TRUE, fit.measures = TRUE)

# Visualize the model
semPaths(
  object = tdmhmo_sem_fit_2,
  whatLabels = "std",
  layout = "spring"
)

# Diagnose the model
modificationindices(tdmhmo_sem_fit_2, sort. = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_2, what = "cor.lv")

# Constraint of the tis_factor_4 stopped correlation among the LV internalized transphobia
# and all other LVs (see lab notebook). This is obviously not ideal. 

# Adding more indirect pathways to this model did nothing to the model fit
# Thus, it is evident that defined parameters do not contribute to model fit, at least
# not the ones I measured in lavaan. Additionally, doing so violated tracing rules. 

# STRUCTURAL MODEL 3 ------------------------------------------------------

# Remove the second-order dehumanization factor and covary theoretically
# similar internalization constructs (see lab notebook).
# Comment out indirect effect to diagnose the Heywood case
tdmhmo_sem_3 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2

# Covariance among LVs
  tis ~~ sobbs
  gims ~~ isos 

# Define structural relationships

# Direct effects
  mhi ~ GM*gims + ISM*isos + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ GS*gims + ISS*isos + SOS*sobbs + ITS*tis
  sobbs ~ ISSO*isos
  tis ~ GIT*gims
  
# Indirect effects
  mhi_s_is := ISS * SM
  mhi_s_g := GS * SM 
  mhi_so := ISSO * SOM
  mhi_it := GIT * ITM
  
# Total effect
  total := GM + ISM + (GS * SM) + (SM * GS) + (ISSO * SOM) + (GIT * ITM)
'

# Fit the model to the data - sem
tdmhmo_sem_fit_3 <- sem(
  model = tdmhmo_sem_3, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_3, standardized = TRUE, fit.measures = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_3, what = "cor.lv")

# Visualize the model
semPaths(
  object = tdmhmo_sem_fit_3,
  layout = "tree2"
)

# STRUCTURAL MODEL 4 ------------------------------------------------------

# Specify the SEM, returning to second-order factor
tdmhmo_sem_4 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2
  
# Second-order factor
  dehum =~ gims + isos

# Covariance among LVs
  tis ~~ sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ DS*dehum + SOS*sobbs + ITS*tis
  sobbs ~ DSO*dehum
  tis ~ DIT*dehum
  
# Indirect effects
  mhi_s := DS * SM
  mhi_so := DSO * SOM
  mhi_it := DIT * ITM
  
# Total effect
  total := DM + (DS * SM) + (DSO * SOM) + (DIT * ITM)
'

# Fit the model to the data
tdmhmo_sem_fit_4 <- sem(
  model = tdmhmo_sem_4, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_4, standardized = TRUE, fit.measures = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_4, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_4,
  layout = "tree2"
)

# STRUCTURAL MODEL 5 ------------------------------------------------------

# First-order factors only, no shame mediation; allow to covary
tdmhmo_sem_5 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2

# Covariance among LVs
  tis ~~ sobbs
  gims ~~ isos
  pfq2s ~~ tis + sobbs

# Define structural relationships

# Direct effects
  mhi ~ GM*gims + ISM*isos + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ GS*gims + ISS*isos
  sobbs ~ ISSO*isos
  tis ~ GIT*gims
  
# Indirect effects
  mhi_s_is := ISS * SM
  mhi_s_g := GS * SM 
  mhi_so := ISSO * SOM
  mhi_it := GIT * ITM
  
# Total effect
  total := GM + ISM + (GS * SM) + (SM * GS) + (ISSO * SOM) + (GIT * ITM)
'

# Fit the model to the data - sem
tdmhmo_sem_fit_5 <- sem(
  model = tdmhmo_sem_5, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_5, standardized = TRUE, fit.measures = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_5, what = "cor.lv")

# Visualize the model
semPaths(
  object = tdmhmo_sem_fit_5,
  layout = "tree2"
)

# STRUCTURAL MODEL 6 ------------------------------------------------------

# Specify second-order SEM w/o shame mediation; instead, allow to covary
tdmhmo_sem_6 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2
  
# Second-order factor
  dehum =~ gims + isos

# Covariance among LVs
  tis ~~ sobbs
  pfq2s ~~ tis + sobbs

# Define structural relationships

# Direct effects
  mhi ~ DM*dehum + SM*pfq2s + SOM*sobbs + ITM*tis
  pfq2s ~ DS*dehum
  sobbs ~ DSO*dehum
  tis ~ DIT*dehum
  
# Indirect effects
  mhi_s := DS * SM
  mhi_so := DSO * SOM
  mhi_it := DIT * ITM
  
# Total effect
  total := DM + (DS * SM) + (DSO * SOM) + (DIT * ITM)
'

# Fit the model to the data
tdmhmo_sem_fit_6 <- sem(
  model = tdmhmo_sem_6, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_6, standardized = TRUE, fit.measures = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_6, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_6,
  layout = "tree2"
)

###########################################################################

# Structural Model 1 - Heywood Case
tdmhmo_sem_fit

# Structural Model 2 - No Heywood Case
tdmhmo_sem_fit_2

# Structural Model 3 - No Dehumanization Factor
tdmhmo_sem_fit_3

# Structural Model 4 - Dehumanization Factor Retained
tdmhmo_sem_fit_4

# Structural Model 5 - First-Order Factors & Covary All Internalization Factors
tdmhmo_sem_fit_5

# Structural Model 6 - Second-Order Factor & Covary All Internalization Factors
tdmhmo_sem_fit_6