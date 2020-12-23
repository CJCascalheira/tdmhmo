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

redcap_no_outliers <- redcap[-names_outliers_md, ]
redcap_no_outliers
