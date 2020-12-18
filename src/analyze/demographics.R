# Dependencies
library(tidyverse)

# Import
redcap <- read_csv("data/raw/redcap_cleaned.csv")

# Select just the demographic variables
redcap_demo <- redcap %>%
  select(record_id:sex_last_year)

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
