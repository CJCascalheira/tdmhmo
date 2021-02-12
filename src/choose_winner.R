# Dependencies
library(tidyverse)
library(rio)

# Import
emails <- read_csv("redcap_emails_full.csv")
full_data <- read_csv("full_data_practice/data/raw/redcap_cleaned.csv")

# Number of gift cards
1500 / 25

# Select the IDs
complete_data <- full_data %>%
  select(record_id)

# Select import variables in email dataset
emails_1 <- emails %>%
  select(record_id, email)

# Filter the emails
emails_2 <- semi_join(emails_1, complete_data, by = "record_id") %>%
  # Add new column to select winners
  mutate(win_id = 1:583)

# Generate 60 random numbers
winners <- sample(1:583, 60, replace = FALSE)

# Filter for the winners
win_emails <- emails_2 %>%
  filter(win_id %in% winners) %>%
  mutate(IRB_number = rep(20796, nrow(.))) %>%
  select(-win_id)

# Save to file
write_csv(win_emails, file = "winning_emails.csv")
