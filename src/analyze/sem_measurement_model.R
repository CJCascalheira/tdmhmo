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
redcap_sd_vector <- redcap_scales %>%
  gather(key = "measure", value = "score") %>%
  group_by(measure) %>%
  summarize(
    sd = sd(score)
  ) %>%
  select(sd) %>%
  pull()
redcap_sd_vector

# Correlation matrix
redcap_cor_matrix <- cor(redcap_scales)
redcap_cor_matrix

# Create covariance matrix
redcap_cov <- cor2cov(redcap_cor_matrix, redcap_sd_vector)
redcap_cov

# Latent variable model
redcap_model <- '
  gims =~ a*gims_factor_1 + b*gims_factor_2 + c*gims_factor_3 + d*gims_factor_4 + e*gims_factor_5
  isos =~ f*isos_factor_1 + g*isos_factor_2 + h*isos_factor_3
  sobbs =~ i*sobbs_factor_1 + j*sobbs_factor_2
  tis =~ k*tis_factor_1 + l*tis_factor_2 + m*tis_factor_3 + n*tis_factor_4
  pfq2s =~ o*pfq2s_parcel_1 + p*pfq2s_parcel_2 + q*pfq2s_parcel_3
  mhi =~ r*mhi_parcel_1 + s*mhi_parcel_2
'

# Fit the data to the model using CFA
redcap_fit <- cfa(
  model = redcap_model,
  sample.cov = redcap_cov,
  sample.nobs = 583,
  std.lv = FALSE
)
 # Summarize the model
summary(redcap_fit, standardized = TRUE)
