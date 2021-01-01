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
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
'

# Latent variable model w/ second-order
tdmhmo_model_1 <- '
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5

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

# Check for identification per Kline (2015)
tdmhmo_fit_2 <- cfa(
  model = tdmhmo_model_1,
  # Complete data necessary
  data = redcap_scales,
  sample.cov = fitted(tdmhmo_fit_1)$cov,
  sample.nobs = 583,
  std.lv = FALSE,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Based on the estimation convergence, which is identical to tdmhmo_fit_1, the CFA is identified
summary(tdmhmo_fit_2, standardized = TRUE, fit.measures = TRUE)

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

# Significant correlations among LVs
lavPredict(tdmhmo_fit_1) %>%
  # Get correlation matrix with p-values
  rcorr()

# STRUCTURAL MODEL --------------------------------------------------------

# Specify the SEM
tdmhmo_sem <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  
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
#  mhi_s := DS * SM
#  mhi_so := DSO * SOM
#  mhi_it := DIT * ITM
#  mhi_sos := SOS * SM
#  mhi_its := ITS * SM
  
# Total effect
#  total := DM + (DS * SM) + (DSO * SOM) + (DIT * ITM)
#  total_shame := DM + (DS * SM)
#  total_selfobj := DM + (DSO * SOM)
#  total_inttrans := DM + (DIT * ITM)
#  total_shame2 := DM + (DS * SM) + SOM + (SOS * SM) + ITM + (ITS * SM)
'

# Fit the model to the data
tdmhmo_sem_fit <- sem(
  model = tdmhmo_sem, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Get standardized solution for reporting
tdmhmo_std_solution <- standardizedSolution(tdmhmo_sem_fit, type = "std.all", se = TRUE) %>%
  as_tibble() %>%
  # Select only direct and indirect effects
  filter(op %in% c("~", ":="))
tdmhmo_std_solution

# Standardized solution for second-order factor
standardizedSolution(tdmhmo_sem_fit, type = "std.all", se = TRUE) %>%
  as_tibble() %>%
  filter(op == "=~", lhs == "dehum")

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit,
  layout = "tree2"
)

# Export standardized solution for easy reporting
write_csv(tdmhmo_std_solution, "full_data_practice/data/results/standardized_solution.csv")

# Significant correlations among LVs
lavPredict(tdmhmo_sem_fit) %>%
  # Get correlation matrix with p-values
  rcorr()

# Get a correlation matrix of observed indicators as predicted by the model
bivar_cor_model <- lavInspect(tdmhmo_sem_fit, what = "cor.ov") %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>%
  gather(key = "subscale_parcel", value = "score",-rowname) %>%
  # Transform all model-implied observed scores to their absolute value
  mutate(
    score = abs(score)
  ) %>%
  # Arrange bivariate correlation dataframe
  arrange(rowname, subscale_parcel) %>%
  rename(score_model = score)
bivar_cor_model  

# Compute correlation matrix of observed indicators within the sample
bivar_cor_sample <- cor(redcap_scales) %>%
  as.data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>%
  gather(key = "subscale_parcel", value = "score",-rowname) %>%
  # Transform all model-implied observed scores to their absolute value
  mutate(
    score = abs(score)
  ) %>%
  # Arrange bivariate correlation dataframe
  arrange(rowname, subscale_parcel) %>%
  rename(score_sample = score)
bivar_cor_sample

# Determine correlation residuals
left_join(bivar_cor_model, bivar_cor_sample) %>%
  mutate(
    # Difference between observed and predicted value (Kline, 2015)
    resid_cor = score_model - score_sample
  ) %>%
  mutate(
    # Take the absolute value (Kline, 2015)
    resid_cor = abs(resid_cor)
  ) %>%
  # Locate correlation residuals greater than .10 (Kline, 2015)
  filter(resid_cor >= .10) %>% 
  arrange(desc(resid_cor)) 

# RESPECIFIED STRUCTURAL MODEL --------------------------------------------

# Specify the SEM
tdmhmo_sem_1 <- '
# Define the measurement model

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  tis =~ tis_factor_1 + tis_factor_2 + tis_factor_3 + tis_factor_4
  pfq2s =~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3
  mhi =~ mhi_parcel_1 + mhi_parcel_2 + mhi_5
  
# Second-order factor
  dehum =~ gims + isos

# Covariance among LVs
  tis ~~ sobbs

# Residual correlations - works, but not identified per Kline (2015)
  tis_factor_4 ~~ pfq2s_parcel_1 + pfq2s_parcel_2 + pfq2s_parcel_3

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
  mhi_sos := SOS * SM
  mhi_its := ITS * SM
  
# Total effect
  total := DM + (DS * SM) + (DSO * SOM) + (DIT * ITM)
  total_shame := DM + (DS * SM)
  total_selfobj := DM + (DSO * SOM)
  total_inttrans := DM + (DIT * ITM)
  total_shame2 := DM + (DS * SM) + SOM + (SOS * SM) + ITM + (ITS * SM)
'

# Fit the model to the data
tdmhmo_sem_fit_1 <- sem(
  model = tdmhmo_sem_1, 
  data = redcap_scales,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the model
summary(tdmhmo_sem_fit_1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Get standardized solution for reporting
tdmhmo_std_solution_1 <- standardizedSolution(tdmhmo_sem_fit_1, type = "std.all", se = TRUE) %>%
  as_tibble() %>%
  # Select only direct and indirect effects
  filter(op %in% c("~", ":="))
tdmhmo_std_solution_1

# Standardized solution for second-order factor
standardizedSolution(tdmhmo_sem_fit_1, type = "std.all", se = TRUE) %>%
  as_tibble() %>%
  filter(op == "=~", lhs == "dehum")

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit_1, what = "cor.lv")

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit_1,
  layout = "tree2"
)

# Export standardized solution for easy reporting
write_csv(tdmhmo_std_solution_1, "full_data_practice/data/results/standardized_solution.csv")
