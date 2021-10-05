# Dependencies
library(Hmisc)
library(tidyverse)
library(lavaan)

# Set the seed
set.seed(1234567)

# Import
redcap_a <- read_csv("data/raw/redcap_cleaned.csv")
redcap_b <- read_csv("data/results/redcap_subscales.csv")

# Keep item-level data
redcap_items <- redcap_a %>%
  select(record_id, isos_1:mhi_2) %>%
  # Recode for reverse score
  mutate(
    tis_1 = recode(tis_1, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_2 = recode(tis_2, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_6 = recode(tis_6, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_7 = recode(tis_7, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_12 = recode(tis_12, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_19 = recode(tis_19, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_25 = recode(tis_25, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    tis_26 = recode(tis_26, `1` = 7, `2` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7`= 1),
    mhi_1 = recode(mhi_1, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    mhi_5 = recode(mhi_5, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
  )

# Select just the parcels and subscales
redcap_scales <- redcap_b %>%
  select(record_id, contains("factor"), contains("parcel"), mhi_5) %>%
  # Alphabetical variable order
  select(record_id, starts_with("gims"), starts_with("isos"), starts_with("mhi"),
         starts_with("pfq2s"), starts_with("sobbs"), starts_with("tis"))
redcap_scales

# When analyzing Likert-scale items as indicators with five or fewer
# response options, then it is best to use robust weighted least squares (WLS),
# such as mean-adjusted least squares (WLSM) and mean- and variance-adjusted
# weighted least squares (WLSMV; Kline, 2015, pp. 323-327). 
# See also Byrne (2012) page 132.

# SEPARATE MEASUREMENT MODELS ---------------------------------------------

#* GIMS -------------------------------------------------------------------

# ESTIMATOR CHOICE
# Response choices are dichotomous, so choose WLSMV (Byrne, 2012; Kline, 2015)

# Construct the model
cfa_gims <- '
# Denial of Gender Identity
 gims_factor_1 =~ gims_1 + gims_2 + gims_3 + gims_4

# Misuse of Pronouns
 gims_factor_2 =~ gims_5 + gims_6

# Invasion of Bodily Privacy
 gims_factor_3 =~ gims_7 + gims_8 + gims_9

# Behavioral Discomfort
 gims_factor_4 =~ gims_10 + gims_11 + gims_12

# Denial of Societal Transphobia
 gims_factor_5 =~ gims_13 + gims_14
'

# Fit the CFA
cfa_gims_fit <- cfa(cfa_gims, data = redcap_items, estimator = "WLSMV")

# Summarize the CFA
summary(cfa_gims_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

#* SOBBS ------------------------------------------------------------------

# ESTIMATOR CHOICE
# Number of response choices <= 5, so use WLSMV (Kline, 2015)

# Construct the model
cfa_sobbs <- '
# Observers Perspective
 sobbs_factor_1 =~ sobbs_2 + sobbs_4 + sobbs_6 + sobbs_7 + sobbs_8 + sobbs_12 + sobbs_14

# Body as Self
 sobbs_factor_2 =~ sobbs_1 + sobbs_3 + sobbs_5 + sobbs_9 + sobbs_10 + sobbs_11 + sobbs_13
'

# Fit the CFA
cfa_sobbs_fit <- cfa(cfa_sobbs, data = redcap_items, estimator = "WLSMV")

# Summarize the CFA
summary(cfa_sobbs_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

#* TIS --------------------------------------------------------------------

# ESTIMATOR CHOICE
# Number of response choices

# Construct the model
cfa_tis <- '
# Pride
  tis_factor_1 =~ tis_1 + tis_2 + tis_6 + tis_7 + tis_12 + tis_19 + tis_25 + tis_26

# Passing
  tis_factor_2 =~ tis_9 + tis_10 + tis_11 + tis_13 + tis_17 + tis_20 + tis_21

# Alienation
  tis_factor_3 =~ tis_5 + tis_22 + tis_24

# Shame
  tis_factor_4 =~ tis_3 + tis_4 + tis_8 + tis_14 + tis_15 + tis_16 + tis_18 + tis_23
'

# Fit the CFA
cfa_tis_fit <- cfa(cfa_tis, data = redcap_items, estimator = "MLM")

# Summarize the CFA
summary(cfa_tis_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Examine residuals
resid(cfa_tis_fit)

# Examine modification indices
modindices(cfa_tis_fit) %>%
  as_tibble() %>%
  arrange(desc(mi))

# Construct the model
cfa_tis1 <- '
# Pride
  tis_factor_1 =~ tis_1 + tis_2 + tis_6 + tis_7 + tis_12 + tis_19 + tis_25 + tis_26

# Passing
  tis_factor_2 =~ tis_9 + tis_10 + tis_11 + tis_13 + tis_17 + tis_20 + tis_21

# Alienation
  tis_factor_3 =~ tis_5 + tis_22 + tis_24

# Shame
  tis_factor_4 =~ tis_3 + tis_4 + tis_8 + tis_14 + tis_15 + tis_16 + tis_18 + tis_23

# Allow residual variances to covary
# Covariance between the error variances (Byrne, 2016, p. 104)
  tis_6 ~~ tis_25
  tis_1 ~~ tis_12
  tis_8 ~~ tis_14
'

# Fit the CFA
cfa_tis_fit1 <- cfa(cfa_tis1, data = redcap_items, estimator = "MLM")

# Summarize the CFA
summary(cfa_tis_fit1, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

#* ISOS -------------------------------------------------------------------

# ESTIMATOR CHOICE
# Number of response choices <= 5, so choose WLSMV (Kline, 2015)

# DAVIDSON ET AL (2013) THREE-FACTOR MODEL

# Construct the model
cfa_isos_davidson <- '
# body evaluation
  isos_factor_1 =~ isos_1 + isos_2 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11
  
# body gazes
  isos_factor_2 =~ isos_3 + isos_4 + isos_5 + isos_8

# unwanted sexual advances
  isos_factor_3 =~ isos_12 + isos_13 + isos_14 + isos_15
'

# Fit the CFA
cfa_isos_davidson_fit <- cfa(cfa_isos_davidson, data = redcap_items, estimator = "WLSMV")

# Summarize the CFA
summary(cfa_isos_davidson_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# KOZEE ET AL (2007) TWO-FACTOR MODEL

# Construct the model
cfa_isos_kozee <- '
# Body Evaluation
  isos_factor_1 =~ isos_1 + isos_2 + isos_3 + isos_4 + isos_5 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11

# Unwanted Explicit Sexual Advances
  isos_factor_2 =~ isos_12 + isos_13 + isos_14 + isos_15
'

# Fit the CFA
cfa_isos_kozee_fit <- cfa(cfa_isos_kozee, data = redcap_items, estimator = "WLSMV")

# Summarize the CFA
summary(cfa_isos_kozee_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Conduct an EFA for ISOS
# https://solomonkurz.netlify.app/post/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/

# 1-factor model
f1 <- '
efa("efa")*f1 =~ isos_1 + isos_2 + isos_3 + isos_4 + isos_5 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11 + isos_12 + isos_13 + isos_14 + isos_15
'

# 2-factor model
f2 <- '
efa("efa")*f1 +
efa("efa")*f2 =~ isos_1 + isos_2 + isos_3 + isos_4 + isos_5 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11 + isos_12 + isos_13 + isos_14 + isos_15
'

# 3-factor model
f3 <- '
efa("efa")*f1 +
efa("efa")*f2 +
efa("efa")*f3 =~ isos_1 + isos_2 + isos_3 + isos_4 + isos_5 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11 + isos_12 + isos_13 + isos_14 + isos_15
'

# 4-factor model
f4 <- '
efa("efa")*f1 +
efa("efa")*f2 +
efa("efa")*f3 +
efa("efa")*f4 =~ isos_1 + isos_2 + isos_3 + isos_4 + isos_5 + isos_6 + isos_7 + isos_8 + isos_9 + isos_10 + isos_11 + isos_12 + isos_13 + isos_14 + isos_15
'

# Fit the models
efa_f1 <- 
  cfa(model = f1,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

efa_f2 <- 
  cfa(model = f2,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

efa_f3 <- 
  cfa(model = f3,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

efa_f4 <- 
  cfa(model = f4,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

# Summarize the fit
summary(efa_f1, fit.measures = TRUE)
summary(efa_f2, fit.measures = TRUE)
summary(efa_f3, fit.measures = TRUE)
summary(efa_f4, fit.measures = TRUE)

# COMPARE THE MODELS

# define the fit measures
fit_measures_robust <- c("chisq.scaled", "df", "pvalue.scaled", 
                         "cfi.scaled", "rmsea.scaled", "srmr")

# collect them for each model
efa_fit_overview <- rbind(
  fitmeasures(efa_f1, fit_measures_robust),
  fitmeasures(efa_f2, fit_measures_robust),
  fitmeasures(efa_f3, fit_measures_robust),
  fitmeasures(efa_f4, fit_measures_robust)) %>% 
  # wrangle
  data.frame() %>% 
  mutate(df = as.integer(df)) %>% 
  mutate_at(vars(cfi.scaled:srmr), ~round(., digits =  3))
efa_fit_overview

#* MHI --------------------------------------------------------------------

# ESTIMATOR CHOICE
# Response options > 5, so used MLM (Kline, 2015)

# Construct the model
cfa_mhi <- '
factor_1 =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5
'

# Fit the CFA
cfa_mhi_fit <- cfa(cfa_mhi, data = redcap_items, estimator = "MLM")

# Summarize the CFA
summary(cfa_mhi_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Examine modification indices
modindices(cfa_mhi_fit) %>%
  as_tibble() %>%
  arrange(desc(mi))

# Construct the model
cfa_mhi <- '
factor_1 =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5

# Allow residual variances to covary
# Covariance between the error variances of these indicator variables 
# (Byrne, 2016, p. 104)
mhi_2 ~~ mhi_3
'

# Fit the CFA
cfa_mhi_fit <- cfa(cfa_mhi, data = redcap_items, estimator = "MLM")

# Summarize the CFA
summary(cfa_mhi_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Construct the model
cfa_mhi <- '
factor_1 =~ mhi_1 + mhi_2 + mhi_3 + mhi_4 + mhi_5

# Allow residual variances to covary
# Covariance between the error variances of these indicator variables 
# (Byrne, 2016, p. 104)
mhi_2 ~~ mhi_3
mhi_1 ~~ mhi_5
'

# Fit the CFA
cfa_mhi_fit <- cfa(cfa_mhi, data = redcap_items, estimator = "MLM")

# Summarize the CFA
summary(cfa_mhi_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

#* PFQ2S ------------------------------------------------------------------

# ESTIMATOR CHOICE
# Number of response choices <= 5, so use WLSMV (Kline, 2015)

# EFA instead of parcels
# https://solomonkurz.netlify.app/post/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/

# 1-factor model
f1 <- '
efa("efa")*f1 =~ pfq2s_1 + pfq2s_2 + pfq2s_3 + pfq2s_4 + pfq2s_5 + pfq2s_6 + pfq2s_7 + pfq2s_8 + pfq2s_9 + pfq2s_10
'

# 2-factor model
f2 <- '
efa("efa")*f1 +
efa("efa")*f2 =~ pfq2s_1 + pfq2s_2 + pfq2s_3 + pfq2s_4 + pfq2s_5 + pfq2s_6 + pfq2s_7 + pfq2s_8 + pfq2s_9 + pfq2s_10
'

# 3-factor model
# Attempted, but lavaan could not find a solution

# Fit the models
efa_f1 <- 
  cfa(model = f1,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

efa_f2 <- 
  cfa(model = f2,
      data = redcap_items,
      rotation = "oblimin",
      estimator = "WLSMV")

# Summarize the fit
summary(efa_f1, fit.measures = TRUE)
summary(efa_f2, fit.measures = TRUE)

# COMPARE THE MODELS

# define the fit measures
fit_measures_robust <- c("chisq.scaled", "df", "pvalue.scaled", 
                         "cfi.scaled", "rmsea.scaled", "srmr")

# collect them for each model
efa_fit_overview <- rbind(
  fitmeasures(efa_f1, fit_measures_robust),
  fitmeasures(efa_f2, fit_measures_robust)) %>% 
  # wrangle
  data.frame() %>% 
  mutate(chisq.scaled  = round(chisq.scaled, digits = 0),
         df            = as.integer(df)) %>% 
  mutate_at(vars(cfi.scaled:srmr), ~round(., digits =  3))
efa_fit_overview

# COMBINE DATA ------------------------------------------------------------

# Select subscales to keep based on above results
redcap_scales_1 <- redcap_scales %>%
  select(record_id, starts_with("gims"), starts_with("sobbs"))
redcap_scales_1

# Select items to keep
redcap_items_1 <- redcap_items %>%
  select(-starts_with("gims"), -starts_with("sobbs"), -starts_with("isos")) %>%
  select(record_id, tis_6, tis_25, tis_1, tis_12, tis_8, tis_14, 
         starts_with("mhi"), starts_with("pfq"))
redcap_items_1

### ISOS ###

# ISOS - Factor 1
isos_factor_1 <- redcap_items %>%
  select(record_id, isos_1, isos_7) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_1 = mean(score)
  )
isos_factor_1

# ISOS - Factor 2
isos_factor_2 <- redcap_items %>%
  select(record_id, isos_2:isos_5, isos_8, isos_10) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_2 = mean(score)
  )
isos_factor_2

# ISOS - Factor 3
isos_factor_3 <- redcap_items %>%
  select(record_id, isos_6, isos_9:isos_11, isos_13, isos_15) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_3 = mean(score)
  )
isos_factor_3

# ISOS - Factor 4
isos_factor_4 <- redcap_items %>%
  select(record_id, isos_12, isos_14) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_4 = mean(score)
  )
isos_factor_4

### TIS ###

# TIS - Factor 1 - Pride
tis_factor_1 <- redcap_items %>%
  select(record_id, tis_2, tis_7, tis_19, tis_26) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_1 = mean(score)
  )
tis_factor_1

# TIS - Factor 2 - Passing
tis_factor_2 <- redcap_items %>%
  select(record_id, tis_9:tis_11, tis_13, tis_17, tis_20, tis_21) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_2 = mean(score)
  )
tis_factor_2

# TIS - Factor 3 - Alienation
tis_factor_3 <- redcap_items %>%
  select(record_id, tis_5, tis_22, tis_24) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_3 = mean(score)
  )
tis_factor_3

# TIS - Factor 4 - Shame
tis_factor_4 <- redcap_items %>%
  select(record_id, tis_3, tis_4, tis_15, tis_16, tis_18, tis_23) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_4 = mean(score)
  )
tis_factor_4

### Create New DF ###
redcap_cfa_results <- left_join(redcap_scales_1, redcap_items_1) %>%
  left_join(isos_factor_1) %>%
  left_join(isos_factor_2) %>%
  left_join(isos_factor_3) %>%
  left_join(isos_factor_4) %>%
  left_join(tis_factor_1) %>%
  left_join(tis_factor_2) %>%
  left_join(tis_factor_3) %>%
  left_join(tis_factor_4)
redcap_cfa_results

# NEW MEASUREMENT MODEL ---------------------------------------------------

# Specify overall CFA based on individual CFA results
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
'

# Fit the CFA - higher order
tdmhmo_cfa_fit <- cfa(tdmhmo_cfa, data = redcap_cfa_results, estimator = "MLM")

# Summarize the CFA - higher order
summary(tdmhmo_cfa_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Examine modification indices
modindices(tdmhmo_cfa_fit) %>%
  as_tibble() %>%
  arrange(desc(mi)) %>%
  filter(op == "~~") %>%
  View()

# Respecify the model
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

# Fit the CFA - higher order
tdmhmo_cfa_fit <- cfa(tdmhmo_cfa, data = redcap_cfa_results, estimator = "MLM")

# Summarize the CFA - higher order
summary(tdmhmo_cfa_fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)

# Export new data frame
write_csv(redcap_cfa_results, "data/results/redcap_cfa_results.csv")
