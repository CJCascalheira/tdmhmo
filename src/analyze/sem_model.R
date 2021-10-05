# Dependencies
library(Hmisc)
library(tidyverse)
library(lavaan)
library(semPlot)
library(semTools)

# Import data
redcap <- read_csv("data/results/redcap_subscales.csv")
redcap_cfa_results <- read_csv("data/results/redcap_cfa_results.csv")

# Select just the parcels and subscales
redcap_scales <- redcap %>%
  select(contains("factor"), contains("parcel"), mhi_5) %>%
  # Alphabetical variable order
  select(starts_with("gims"), starts_with("isos"), starts_with("mhi"), starts_with("pfq2s"),
         starts_with("sobbs"), starts_with("tis"))
redcap_scales

# MEASUREMENT MODEL -------------------------------------------------------

# Specify CFA using CFA Model 4.1 (i.e., standard CFA)
tdmhmo_cfa <- '
# DEFINE THE MEASUREMENT MODEL

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3 + isos_factor_4
  
  mhi =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5
  
  tis =~ tis_factor_1 + tis_6 + tis_25 + tis_1 + tis_12 + tis_factor_2 + 
  tis_factor_3 + tis_factor_4 + tis_8 + tis_14
  
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  
  pfq2s =~ pfq2s_1 + pfq2s_2 + pfq2s_3 + pfq2s_4 + pfq2s_5 + pfq2s_6 + 
  pfq2s_7 + pfq2s_8 + pfq2s_9 + pfq2s_10

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Allow residual variances to covary
# Covariance between the error variances (Byrne, 2016, p. 104)
  tis_6 ~~ tis_25
  tis_1 ~~ tis_12
  tis_8 ~~ tis_14
  mhi_2 ~~ mhi_3
  
  tis_factor_1 ~~ tis_6
  tis_factor_1 ~~ tis_25
  tis_factor_1 ~~ tis_1
  tis_factor_1 ~~ tis_12
  tis_factor_4 ~~ tis_8
  tis_factor_4 ~~ tis_14
  
  pfq2s_7 ~~ pfq2s_10
  pfq2s_8 ~~ pfq2s_9
  gims_factor_3 ~~ gims_factor_5
'

# No higher-order factor
tdmhmo_cfa_1 <- '
# DEFINE THE MEASUREMENT MODEL

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3 + isos_factor_4
  
  mhi =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5
  
  tis =~ tis_factor_1 + tis_6 + tis_25 + tis_1 + tis_12 + tis_factor_2 + 
  tis_factor_3 + tis_factor_4 + tis_8 + tis_14
  
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  
  pfq2s =~ pfq2s_1 + pfq2s_2 + pfq2s_3 + pfq2s_4 + pfq2s_5 + pfq2s_6 + 
  pfq2s_7 + pfq2s_8 + pfq2s_9 + pfq2s_10

# Allow residual variances to covary
# Covariance between the error variances (Byrne, 2016, p. 104)
  tis_6 ~~ tis_25
  tis_1 ~~ tis_12
  tis_8 ~~ tis_14
  mhi_2 ~~ mhi_3
  
  tis_factor_1 ~~ tis_6
  tis_factor_1 ~~ tis_25
  tis_factor_1 ~~ tis_1
  tis_factor_1 ~~ tis_12
  tis_factor_4 ~~ tis_8
  tis_factor_4 ~~ tis_14
  
  pfq2s_7 ~~ pfq2s_10
  pfq2s_8 ~~ pfq2s_9
  gims_factor_3 ~~ gims_factor_5
'

# Fit the CFA - higher order
tdmhmo_cfa_fit <- cfa(tdmhmo_cfa, data = redcap_cfa_results, estimator = "MLM")

# Summarize the CFA - higher order
summary(tdmhmo_cfa_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Fit the CFA - first-order factors only model
tdmhmo_cfa_fit_1 <- cfa(tdmhmo_cfa_1, data = redcap_cfa_results, estimator = "MLM")

# Different between the two models?
anova(tdmhmo_cfa_fit, tdmhmo_cfa_fit_1)

# Visualize the CFA
semPaths(
  object = tdmhmo_cfa_fit,
  layout = "spring"
)

# STRUCTURAL MODEL --------------------------------------------------------

# Specify the SEM using SEM Model 4.4
tdmhmo_sem <- '
# DEFINE THE MEASUREMENT MODEL

# First-order factors
  gims =~ gims_factor_1 + gims_factor_2 + gims_factor_3 + gims_factor_4 + gims_factor_5
  
  isos =~ isos_factor_1 + isos_factor_2 + isos_factor_3 + isos_factor_4
  
  mhi =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5
  
  tis =~ tis_factor_1 + tis_6 + tis_25 + tis_1 + tis_12 + tis_factor_2 + 
  tis_factor_3 + tis_factor_4 + tis_8 + tis_14
  
  sobbs =~ sobbs_factor_1 + sobbs_factor_2
  
  pfq2s =~ pfq2s_1 + pfq2s_2 + pfq2s_3 + pfq2s_4 + pfq2s_5 + pfq2s_6 + 
  pfq2s_7 + pfq2s_8 + pfq2s_9 + pfq2s_10

# Second-order factor
  dehum =~ NA*gims + isos

# Unit variance identification constraint (Kline, 2015, p. 199)
  dehum ~~ 1*dehum

# Allow residual variances to covary
# Covariance between the error variances (Byrne, 2016, p. 104)
  tis_6 ~~ tis_25
  tis_1 ~~ tis_12
  tis_8 ~~ tis_14
  mhi_2 ~~ mhi_3
  
  tis_factor_1 ~~ tis_6
  tis_factor_1 ~~ tis_25
  tis_factor_1 ~~ tis_1
  tis_factor_1 ~~ tis_12
  tis_factor_4 ~~ tis_8
  tis_factor_4 ~~ tis_14
  
  pfq2s_7 ~~ pfq2s_10
  pfq2s_8 ~~ pfq2s_9
  gims_factor_3 ~~ gims_factor_5

# Covariance between internalization LVs
  tis ~~ sobbs

# DEFINE STRUCTURAL MODEL

# Direct effects
  mhi ~ DM*dehum + ITM*tis + SOM*sobbs + SM*pfq2s
  tis ~ DIT*dehum
  sobbs ~ DSO*dehum
  pfq2s ~ DS*dehum + ITS*tis + SOS*sobbs

# Indirect effects
  d_it_mh := DIT * ITM
  d_so_mh := DSO * SOM
  d_s_mh := DS * SM
  it_s_mh := ITS * SM
  so_s_mh := SOS * SM
  
# Total effects
total_int_tran := ITM + (ITS * SM)

# Estimating these total effects does not make sense
# given the nonsignificant direct paths
#  total_dehum := DM + (DIT * ITM) + (DSO * SOM) + (DS * SM)
#  total_self_obj := SOM + (SOS * SM)
'

# Fit the model to the data
tdmhmo_sem_fit <- sem(
  model = tdmhmo_sem, 
  data = redcap_cfa_results,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLM"
)

# Summarize the SEM
summary(tdmhmo_sem_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Correlations among the LVs
lavInspect(tdmhmo_sem_fit, what = "cor.lv")

# Significant correlations among LVs
# Correlations among LVs different from lavInspect due to estimation versus converged solution
lavPredict(tdmhmo_sem_fit) %>%
  # Get correlation matrix with p-values
  rcorr()

# Visualize the SEM
semPaths(
  object = tdmhmo_sem_fit,
  layout = "spring"
)

# Get standardized solution for standard error reporting
tdmhmo_sem_report <- standardizedsolution(tdmhmo_sem_fit) %>%
  as_tibble() %>%
  filter(op == "~" | op == ":=" | (lhs == "dehum" & op == "=~")) %>%
  # Round out p-values for interpretation
  mutate(pvalue = round(pvalue, 3))
tdmhmo_sem_report

# Write to file for easy reporting
write_csv(tdmhmo_sem_report, "data/results/tdmhmo_sem_report.csv")

# Standard error for covariance (psi) of tis and sobbs
standardizedsolution(tdmhmo_sem_fit) %>%
  as_tibble() %>%
  filter(lhs == "sobbs" & op == "~~")

# SEs and 95% CI for standardized coefficient of indirect relations
standardizedsolution(tdmhmo_sem_fit) %>%
  as_tibble() %>%
  filter(op == ":=") %>%
  mutate(pvalue = round(pvalue, 3))


#* ALT: BI-FACTOR MODEL ---------------------------------------------------

# https://pages.mtu.edu/~shanem/psy5220/daily/Day22/cfa.html#the-bifactor-model
