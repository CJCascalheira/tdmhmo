# Dependencies
library(tidyverse)
library(lavaan)

# Import data
redcap <- read_csv("data/results/redcap_subscales.csv")

# Select just the parcels and subscales
redcap_scales <- redcap %>%
  select(contains("factor"), contains("parcel")) %>%
  # Alphabetical variable order
  select(starts_with("gims"), starts_with("isos"), starts_with("mhi"), starts_with("pfq2s"),
         starts_with("sobbs"), starts_with("tis"))
redcap_scales

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

# Latent variable model
tdmhmo_model <- '
  gims =~ a*gims_factor_1 + b*gims_factor_2 + c*gims_factor_3 + d*gims_factor_4 + e*gims_factor_5
  isos =~ f*isos_factor_1 + g*isos_factor_2 + h*isos_factor_3
  sobbs =~ i*sobbs_factor_1 + j*sobbs_factor_2
  tis =~ k*tis_factor_1 + l*tis_factor_2 + m*tis_factor_3 + n*tis_factor_4
  pfq2s =~ o*pfq2s_parcel_1 + p*pfq2s_parcel_2 + q*pfq2s_parcel_3
  mhi =~ r*mhi_parcel_1 + s*mhi_parcel_2
'

# Fit the data to the model using CFA
tdmhmo_fit <- cfa(
  model = tdmhmo_model,
  # Complete data necessary
  data = redcap_scales,
  sample.cov = tdmhmo_cov,
  sample.nobs = 583,
  std.lv = FALSE,
  # Maximum likelihood estimation with robust standard errors
  estimator = "MLR"
)

# Summarize the model
summary(tdmhmo_fit, standardized = TRUE, fit.measures = TRUE)

# SIGNIFICANT INTERCORRELATIONS AMONG LATENT VARIABLES --------------------

# Correlations among the latent variables (LVs)
lv_cor_matrix <- lavInspect(tdmhmo_fit, what = "cor.lv")
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