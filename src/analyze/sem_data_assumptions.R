# Dependencies
library(psych)
library(car)
library(tidyverse)
library(MVN)

# Import
redcap <- read_csv("data/results/redcap_scales.csv")

# Just total scores
redcap_totals <- redcap %>%
  select(starts_with("total"))
redcap_totals

# MULTICOLLINEARITY (WESTON & GORE, 2006 ----------------------------------

# Not an issue because intercorrelations are below .80
pairs.panels(redcap_totals)

# INDEPENDENCE & UNSTANDARDIZATION (KLINE, 2012) --------------------------

# Given the way that the data were collected, there is evidence for 
# independent observations; recruitment of one participants did not
# influence the recruitment of another participants.

# Durbin-Watson test of independence using a single regression model
car::durbinWatsonTest(lm(total_mhi ~ total_isos + total_tis + total_sobbs + total_gims + total_pfq2s, 
                         data = redcap_totals))

# Unstardardization will be met by using a covariance matrix

# MISSING VALUES (KLINE, 2012) --------------------------------------------

# No data should be missing due to mean imputation following a MCAR test
# using a regression-based approach

# Are any data missing?
redcap %>%
  select(-record_id) %>%
  gather(key = "measures", value = "scores") %>%
  filter(is.na(scores))

# MULTIVARIATE NORMAL (KLINE, 2012) ---------------------------------------

# In terms of univariate normality, evidence for support:
# - large sample size

# Visual inspection of graphs
pairs.panels(redcap_totals)

# Prepare vector
c <- c()

# Skewness
for (i in 1:6) {
  
  # Calculate skewness and save in vector
  c[i] <- skew(redcap_totals[i][[1]])
  
}
c

# Prepare vector 
d <- c()

# Kurtosis
for (i in 1:6) {
  
  # Calculate sample kurtosis and save in vector
  d[i] <- kurtosi(redcap_totals[i][[1]])
  
}
d

# QQ plot for ISOS
qqPlot(redcap_totals$total_isos)

# QQ plot for TIS
qqPlot(redcap_totals$total_tis)

# QQ plot for SOBBS
qqPlot(redcap_totals$total_sobbs)

# QQ plot for GIMS
qqPlot(redcap_totals$total_gims)

# QQ plot for PFQ2s
qqPlot(redcap_totals$total_pfq2s)

# QQ plot for MHI
qqPlot(redcap_totals$total_mhi)

#######
# Multivariate Normality Checks
#######

# Mardia's MVN test
mvn_1 <- mvn(data = redcap_totals, mvnTest = "mardia", desc = TRUE, univariateTest = "Lillie")
mvn_1

# Henze-Zirkler's MVN test
mvn(data = redcap_totals, mvnTest = "hz", desc = FALSE, univariateTest = "AD",
    multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

# Identify the record IDs for the multivariate outliers
outlier(redcap_totals, bad = 20)

# Set the Mahalanobis distance for all cases
md <- mahalanobis(redcap_totals, center = colMeans(redcap_totals), cov = cov(redcap_totals))

# Quantile cutoff value, rounded down to pull most extreme 20 multivariate outliers
# Quantile cutoff estimate taken from HZ function call
cutoff <- 13.95

# Identify the outlier
names_outliers_md <- which(md > cutoff)
names_outliers_md

# 20 multivariate outliers identified?
length(names_outliers_md)

# Check outlier identification; Mahalanobis distances slightly different from the mvn call
md[names_outliers_md]

# Remove outliers
no_mv_outlier <- redcap_totals[-names_outliers_md, ]
no_mv_outlier

# Prepare to recode multivariate outliers (see Weston & Gore, 2006)
redcap_desc <- mvn_1$Descriptives %>%
  rownames_to_column() %>%
  as_tibble() %>%
  select(rowname, Mean, Std.Dev) %>%
  mutate(
    sd_low_m = Mean - (Std.Dev * 1.5),
    sd_high_m = Mean + (Std.Dev * 1.5)
  )
redcap_desc

# Recode outliers
recoded_outliers <- redcap_totals[names_outliers_md, ] %>%
  mutate(
    total_isos = if_else(total_isos < redcap_desc$sd_low_m[1], redcap_desc$sd_low_m[1], if_else(
      total_isos > redcap_desc$sd_high_m[1], redcap_desc$sd_high_m[1], total_isos
    )),
    total_tis = if_else(total_tis < redcap_desc$sd_low_m[2], redcap_desc$sd_low_m[2], if_else(
      total_tis > redcap_desc$sd_high_m[2], redcap_desc$sd_high_m[2], total_tis
    )),
    total_sobbs = if_else(total_sobbs < redcap_desc$sd_low_m[3], redcap_desc$sd_low_m[3], if_else(
      total_sobbs > redcap_desc$sd_high_m[3], redcap_desc$sd_high_m[3], total_sobbs
    )),
    total_gims = if_else(total_gims < redcap_desc$sd_low_m[4], redcap_desc$sd_low_m[4], if_else(
      total_gims > redcap_desc$sd_high_m[4], redcap_desc$sd_high_m[4], total_gims
    )),
    total_pfq2s = if_else(total_pfq2s < redcap_desc$sd_low_m[5], redcap_desc$sd_low_m[5], if_else(
      total_pfq2s > redcap_desc$sd_high_m[5], redcap_desc$sd_high_m[5], total_pfq2s
    )),
    total_mhi = if_else(total_mhi < redcap_desc$sd_low_m[6], redcap_desc$sd_low_m[6], if_else(
      total_mhi > redcap_desc$sd_high_m[6], redcap_desc$sd_high_m[6], total_mhi
    ))
  )
recoded_outliers

# Combine data frames
redcap_totals_2 <- bind_rows(no_mv_outlier, recoded_outliers)
redcap_totals_2

# Henze-Zirkler's MVN test - quantile method
mvn(data = redcap_totals_2, mvnTest = "hz", desc = FALSE, univariateTest = "AD",
    multivariatePlot = "qq", multivariateOutlierMethod = "quan", showOutliers = TRUE)

# Henze-Zirkler's MVN test - adjusted quantile method
mvn(data = redcap_totals_2, mvnTest = "hz", desc = FALSE, univariateTest = "AD",
    multivariatePlot = "qq", multivariateOutlierMethod = "adj", showOutliers = TRUE)

# Thus, the assumption of multivariate normality approached tenability, but did not
# reach it. Hence, an estimation method accounting for non-normality must be used. 

# MEASURED WITHOUT ERROR (KLINE, 2012) ------------------------------------

# Assumption met because Cronbach's alphas for all measures were > .80

# ADJUST DATA FOR MEASUREMENT AND STRUCTURAL MODELS -----------------------

# Mean - ISOS, TIS, SOBBS
# Sum - GIMS, PFQ2S, MHI

# ISOS has both a two- (women) and three-factor (men) structure, so since most of the sample is
# transmasculine or trans men, the three-factor solution was used 

# ISOS - Factor 1 - body evaluation
isos_factor_1 <- redcap %>%
  select(record_id, isos_1, isos_2, isos_6:isos_11) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_1 = mean(score)
  )
isos_factor_1

# ISOS - Factor 2 - body gazes
isos_factor_2 <- redcap %>%
  select(record_id, isos_3:isos_5, isos_8) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_2 = mean(score)
  )
isos_factor_2

# ISOS - Factor 3 - unwanted sexual advances 
isos_factor_3 <- redcap %>%
  select(record_id, isos_12:isos_15) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    isos_factor_3 = mean(score)
  )
isos_factor_3

# TIS - Factor 1 - Pride
tis_factor_1 <- redcap %>%
  select(record_id, tis_1, tis_2, tis_6, tis_7, tis_12, tis_19, tis_25, tis_26) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_1 = mean(score)
  )
tis_factor_1

# TIS - Factor 2 - Passing
tis_factor_2 <- redcap %>%
  select(record_id, tis_9:tis_11, tis_13, tis_17, tis_20, tis_21) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_2 = mean(score)
  )
tis_factor_2

# TIS - Factor 3 - Alienation
tis_factor_3 <- redcap %>%
  select(record_id, tis_5, tis_22, tis_24) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_3 = mean(score)
  )
tis_factor_3

# TIS - Factor 4 - Shame
tis_factor_4 <- redcap %>%
  select(record_id, tis_3, tis_4, tis_8, tis_14:tis_16, tis_18, tis_23) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    tis_factor_4 = mean(score)
  )
tis_factor_4

# SOBBS - Factor 1 - Observer's Perspectives
sobbs_factor_1 <- redcap %>%
  select(record_id, sobbs_2, sobbs_4, sobbs_6:sobbs_8, sobbs_12, sobbs_14) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    sobbs_factor_1 = mean(score)
  )
sobbs_factor_1

# SOBBS - Factor 2 - Body as Self
sobbs_factor_2 <- redcap %>%
  select(record_id, sobbs_1, sobbs_3, sobbs_5, sobbs_9:sobbs_11, sobbs_13) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    sobbs_factor_2 = mean(score)
  )
sobbs_factor_2

# GIMS - Factor 1 - Denial of Gender Identity
gims_factor_1 <- redcap %>%
  select(record_id, gims_1, gims_2, gims_3, gims_4) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    gims_factor_1 = sum(score)
  )
gims_factor_1

# GIMS - Factor 2 - Misuse of Pronouns
gims_factor_2 <- redcap %>%
  select(record_id, gims_5, gims_6) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    gims_factor_2 = sum(score)
  )
gims_factor_2

# GIMS - Factor 3 - Invasion of Bodily Privacy
gims_factor_3 <- redcap %>%
  select(record_id, gims_7, gims_8, gims_9) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    gims_factor_3 = sum(score)
  )
gims_factor_3

# GIMS - Factor 4 - Behavioral Discomfort
gims_factor_4 <- redcap %>%
  select(record_id, gims_10, gims_11, gims_12) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    gims_factor_4 = sum(score)
  )
gims_factor_4

# GIMS - Factor 5 - Denial of Societal Transphobia
gims_factor_5 <- redcap %>%
  select(record_id, gims_13, gims_14) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    gims_factor_5 = sum(score)
  )
gims_factor_5

# Random combination of PFQ2S numbers
sample(1:10, replace = FALSE)

# Parcel 1: 5, 6, 8
# Parcel 2: 1, 7, 4
# Parcel 3: 10, 3, 2, 9

# PFQ2S - Parcel 1 - 5, 6, 8
pfq2s_parcel_1 <- redcap %>%
  select(record_id, pfq2s_5, pfq2s_6, pfq2s_8) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    pfq2s_parcel_1 = sum(score)
  )
pfq2s_parcel_1

# PFQ2S - Parcel 2 - 1, 7, 4
pfq2s_parcel_2 <- redcap %>%
  select(record_id, pfq2s_1, pfq2s_7, pfq2s_4) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    pfq2s_parcel_2 = sum(score)
  )
pfq2s_parcel_2

# PFQ2S - Parcel 3 - 10, 3, 2, 9
pfq2s_parcel_3 <- redcap %>%
  select(record_id, pfq2s_10, pfq2s_3, pfq2s_2, pfq2s_9) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    pfq2s_parcel_3 = sum(score)
  )
pfq2s_parcel_3

# Random combination of MHI numbers
sample(1:5, replace = FALSE)

# Parcel 1: 4, 2, 5
# Parcel 2: 3, 1

# MHI - Parcel 1 - 4, 2, 5
mhi_parcel_1 <- redcap %>%
  select(record_id, mhi_4, mhi_2, mhi_5) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    mhi_parcel_1 = sum(score)
  )
mhi_parcel_1

# MHI - Parcel 2 - 3, 1
mhi_parcel_2 <- redcap %>%
  select(record_id, mhi_3, mhi_1) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    mhi_parcel_2 = sum(score)
  )
mhi_parcel_2

#######
# Combine new subscales to data frames
#######

# Total dataframe
redcap_1 <- redcap %>%
  # Add the ISOS subscales
  left_join(isos_factor_1) %>%
  left_join(isos_factor_2) %>%
  left_join(isos_factor_3) %>%
  # Add the TIS subscales
  left_join(tis_factor_1) %>%
  left_join(tis_factor_2) %>%
  left_join(tis_factor_3) %>%
  left_join(tis_factor_4) %>%
  # Add the SOBBS subscales
  left_join(sobbs_factor_1) %>%
  left_join(sobbs_factor_2) %>%
  # Add the GIMS subscales
  left_join(gims_factor_1) %>%
  left_join(gims_factor_2) %>%
  left_join(gims_factor_3) %>%
  left_join(gims_factor_4) %>%
  left_join(gims_factor_5) %>%
  # Add the PFQ2S parcels
  left_join(pfq2s_parcel_1) %>%
  left_join(pfq2s_parcel_2) %>%
  left_join(pfq2s_parcel_3) %>%
  # Add the MHI parcels
  left_join(mhi_parcel_1) %>%
  left_join(mhi_parcel_2)
redcap_1

# Save to file
write_csv(redcap_1, "data/results/redcap_subscales.csv")

# Remove MV outliers
redcap_no_outliers <- redcap_1[-names_outliers_md, ]
redcap_no_outliers

# Save to file
write_csv(redcap_no_outliers, "data/results/no_mv_outliers.csv")
