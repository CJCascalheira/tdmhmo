# Dependencies
library(Hmisc)
library(tidyverse)
library(RBtest)
library(psych)

# Import
redcap <- read_csv("data/raw/redcap_cleaned.csv")

# Just the instruments
redcap_scales <- redcap %>%
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

# MISSING DATA? -----------------------------------------------------------

# How many values are missing?
redcap_scales %>%
  gather(key = "scale", value = "score", -record_id) %>%
  filter(is.na(score)) %>%
  nrow()

# Are data missing completely at random (MCAR)?
redcap_scales %>%
  select(-record_id) %>%
  RBtest()

# Replace missing values
for (i in 2:length(redcap_scales)) {
  
  redcap_scales[i] <- ifelse(is.na(redcap_scales[i][[1]]), 
         mean(redcap_scales[i][[1]], na.rm = TRUE), 
         redcap_scales[i][[1]])
}

# Missing values remaining?
redcap_scales %>%
  gather(key = "scale", value = "score", -record_id) %>%
  filter(is.na(score)) %>%
  nrow()

# INTERNAL CONSISTENCY ----------------------------------------------------

# Interpersonal Sexual Objectification Scale 
redcap_scales %>% 
  select(starts_with("isos")) %>%
  alpha()

# Gender Identity Microaggressions Scale
redcap_scales %>%
  select(starts_with("gims")) %>%
  alpha()

# Self-Objectification Beliefs and Behaviors Scale 
redcap_scales %>%
  select(starts_with("sobbs")) %>%
  alpha()

# Transgender Identity Survey 
redcap_scales %>%
  select(starts_with("tis")) %>%
  alpha()

# Personal Feelings Questionnaire-2 
redcap_scales %>%
  select(starts_with("pfq2s")) %>%
  alpha()

# Mental Health Inventory 
redcap_scales %>%
  select(starts_with("mhi")) %>%
  alpha()

# TOTAL SCORES & DESCRIPTIVES ----------------------------------------------

# ISOS - mean
isos <- redcap_scales %>%
  select(record_id, starts_with("isos")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_isos = mean(score)
  ) %>%
  select(-record_id)
isos

# TIS - mean
tis <- redcap_scales %>%
  select(record_id, starts_with("tis")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_tis = mean(score)
  ) %>%
  select(-record_id)
tis

# SOBBS - mean
sobbs <- redcap_scales %>%
  select(record_id, starts_with("sobbs")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_sobbs = mean(score)
  ) %>%
  select(-record_id)
sobbs

# GIMS - sum
gims <- redcap_scales %>%
  select(record_id, starts_with("gims")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_gims = sum(score)
  ) %>%
  select(-record_id)
gims  

# PFQ2S - sum
pfq2s <- redcap_scales %>%
  select(record_id, starts_with("pfq2s")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_pfq2s = sum(score)
  ) %>%
  select(-record_id)
pfq2s

# MHI - sum
mhi <- redcap_scales %>%
  select(record_id, starts_with("mhi")) %>%
  gather(key = "measure", value = "score", -record_id) %>%
  group_by(record_id) %>%
  summarize(
    total_mhi = sum(score)
  ) %>%
  select(-record_id)
mhi  

# Bind columns
scales_total <- redcap_scales %>%
  bind_cols(isos, gims, sobbs, tis, pfq2s, mhi)
scales_total

# Mean, SD, range
scales_total %>%
  select(starts_with("total")) %>%
  gather(key = "measure", value = "score") %>%
  group_by(measure) %>%
  summarise(
    M = mean(score),
    SD = sd(score),
    MIN = min(score),
    MAX = max(score)
  )

# Prepare for the correlation matrix
totals_only <- scales_total %>%
  select(starts_with("total"))

# Bivariate correlations
rcorr(as.matrix(totals_only), type = "pearson")

# Correlation visualization
pairs.panels(as.matrix(totals_only))

# Save correlation matrix
cor_list <- rcorr(as.matrix(totals_only), type = "pearson") %>%
  as.list()

# Convert to table
cor_table <- cor_list$r %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  as_tibble()
cor_table

# EXPORT SCALES -----------------------------------------------------------

# Export correlation table
write_csv(cor_table, path = "data/results/cor_table.csv")

# Export total scales
write_csv(scales_total, path = "data/results/redcap_scales.csv")
