# Dependencies
library(tidyverse)

# Import
redcap <- read_csv("full_data_practice/data/raw/redcap_cleaned.csv")

# Select just the demographic variables
redcap_demo <- redcap %>%
  select(record_id:ethnic)

# CONTEXT -----------------------------------------------------------------

# Age
redcap_demo %>%
  summarize(
    m_age = mean(age),
    sd_age = sd(age),
    min = min(age),
    max = max(age)
  )

# Region
redcap_demo %>%
  count(region) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Income
redcap_demo %>%
  count(income) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Education
redcap_demo %>%
  count(education) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Ethnicity
redcap_demo %>%
  count(ethnic) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# GENDER ------------------------------------------------------------------

# Gender
redcap_demo %>%
  count(gender) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Sex Assigned at Birth
redcap_demo %>%
  count(saab) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Hormones
redcap_demo %>%
  count(hormone) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Top Surgery
redcap_demo %>%
  count(top_sex) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Bottom Surgery
redcap_demo %>%
  count(bottom_sex) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# SEXUAL ORIENTATION ------------------------------------------------------

# Sexual orientation
redcap_demo %>%
  count(sex_orient) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Sexual attraction
redcap_demo %>%
  count(sex_attrac) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))

# Sex in the last year
redcap_demo %>%
  count(sex_last_year) %>%
  mutate(percent = n/nrow(redcap_demo)) %>%
  arrange(desc(n))
