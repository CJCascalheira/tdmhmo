# Dependencies
library(tidyverse)

# Import
redcap <- read_csv("data/raw/redcap_complete.csv")

# Demographic data
redcap_demo <- redcap %>%
  select(record_id:sex_last_year)

# Scale data
redcap_scales <- redcap %>%
  select(-c(age:sex_last_year))

# GENDER ------------------------------------------------------------------

# Gender selection variables
redcap_gender <- redcap_demo %>%
  select(record_id, starts_with("gender__"))

# Gender options for trans community
gender_names <- c("nothing", "agender", "androgynous", "butch", "changes a lot", "dyke", "female", "femme", "FTM", "gender fluid", "gender neutral", "genderless", "genderqueer", "intergender", "male", "man", "MTF", "pangender", "queer", "trans guy", "transfeminine", "transgender", "transman", "transmasculine", "two spirit", "unsure", "woman", "other", "trans girl", "transwoman", "non-binary", "trans woman", "trans man")

# Gender coding
for (i in 2:length(redcap_gender)) {
  
  # Conditional and assignment
  redcap_gender[i] <- if_else(redcap_gender[i] == 1, gender_names[i], "none")
  
}
redcap_gender

# Condense to one column
gender_col <- redcap_gender %>%
  gather(key = "gender__", value = "gender", -record_id) %>%
  filter(gender != "none") %>%
  spread(key = "gender__", value = "gender") %>%
  # Unite all the columns
  unite(gender_p, gender___1:gender___9, sep = "_", remove = TRUE, na.rm = TRUE)
gender_col

# Add named gender variable to demo data frame
redcap_demo_1 <- redcap_demo %>%
  select(-starts_with("gender__")) %>%
  left_join(gender_col, by = "record_id") %>%
  select(record_id:education, gender_p, saab, everything()) %>%
  # Assigned sex at birth
  mutate(saab = if_else(saab == 0, "female", "male"))
redcap_demo_1

# Check unique categories not capture by trans man, trans woman, nonbinary
sum(redcap_demo_1$gender_p == "butch")
sum(redcap_demo_1$gender_p == "dyke")
sum(redcap_demo_1$gender_p == "transfeminine")
sum(redcap_demo_1$gender_p == "transgender")
sum(redcap_demo_1$gender_p == "transmasculine")
sum(redcap_demo_1$gender_p == "two spirit")
sum(redcap_demo_1$gender_p == "unsure")
sum(redcap_demo_1$gender_p == "other")

# Categorize gender identities
redcap_demo_2 <- redcap_demo_1 %>%
  mutate(
    # Trans man
    trans_man = if_else(
      str_detect(gender_p, regex("FTM|(?<!fe)male|(?<!wo)man|trans guy|transman|trans man", ignore_case = TRUE)), 
      "trans_man", NA_character_
      ),
    # Trans woman
    trans_woman = if_else(
      str_detect(gender_p, regex("female|femme|MTF|woman|trans girl|transwoman|trans woman", ignore_case = TRUE)),
      "trans_woman", NA_character_
    ),
    # Nonbinary
    non_binary = if_else(
      str_detect(gender_p, regex("non-binary|other|queer|pangender|intergender|genderqueer|genderless|gender neutral|gender fluid|changes a lot|androgynous|agender", ignore_case = TRUE)),
      "non_binary", NA_character_
    ),
    # Transmasculine
    trans_masc = if_else(
      str_detect(gender_p, regex("transmasc", ignore_case = TRUE)), 
      "trans_masc", NA_character_
    ),
    # Transfeminine
    trans_fem = if_else(
      str_detect(gender_p, regex("transfemin", ignore_case = TRUE)), 
      "trans_fem", NA_character_
    ),
    # Transgender
    transgender = if_else(
      str_detect(gender_p, regex("transgender", ignore_case = TRUE)), 
      "transgender", NA_character_
    ),
    # Unsure / Other
    unsure = if_else(
      str_detect(gender_p, regex("unsure|other", ignore_case = TRUE)), 
      "unsure", NA_character_
    )
  )

# Evidence of cisgender men
cis_men <- redcap_demo_2 %>%
  filter(trans_man == "trans_man" & saab == "male")
cis_men

# Evidence of cisgender women
cis_women <- redcap_demo_2 %>%
  filter(trans_woman == "trans_woman" & saab == "female") %>%
  filter(gender_p %in% c("female", "woman", "woman_female"))
cis_women

# Cisgender people
cisgender <- bind_rows(cis_men, cis_women)
cisgender

# Remove cisgender participants
no_cis <- anti_join(redcap_demo_2, cisgender, by = "record_id") %>%
  # Unite the binary vs. nonbinary columns
  unite(gender, trans_man:unsure, sep = "_", remove = TRUE, na.rm = TRUE) %>%
  # Drop other gender column
  select(-gender_other) %>%
  # Reorder columns
  select(record_id:education, gender, everything())
no_cis

# Does transgender mean trans man or trans woman?
no_cis %>%
  filter(gender == "transgender")

# Recode transgender to trans_man
redcap_demo_3 <- no_cis %>%
  mutate(
    gender = recode(gender, "transgender" = "trans_man")
  ) %>%
  # Drop older column
  select(-gender_p)
redcap_demo_3

# RACE AND ETHNICITY ------------------------------------------------------

# Select ethnicity variables
redcap_ethnic <- redcap_demo_3 %>%
  select(record_id, starts_with("eth_race__"))

# Ethnicity names
ethnic_names <- c("nothing", "American Indian / Alaska Native", "Black / African American / Caribbean American", "East Asian / Asian American", "South Asian / Indian American", "Hispanic / Latina(o) / Latin American / Chicana(o)", "Native Hawaiian / Pacific Islander", "Middle Eastern / Arab American", "Multiethnic / Multiracial", "Biethnic / Biracial", "White / European American", "Other")

# Record integers to characters
for (i in 2:length(redcap_ethnic)) {
  
  # Conditional and assignment
  redcap_ethnic[i] <- if_else(redcap_ethnic[i] == 1, ethnic_names[i], "none")
  
}
redcap_ethnic

# Condense to one column
ethnic_col <- redcap_ethnic %>%
  gather(key = "ethnic__", value = "ethnic", -record_id) %>%
  filter(ethnic != "none") %>%
  spread(key = "ethnic__", value = "ethnic") %>%
  # United columns
  unite(ethnic_p, eth_race___1:eth_race___9, sep = "_", remove = TRUE, na.rm = TRUE)
ethnic_col

# Unreported
ethnic_col_1 <- anti_join(redcap_ethnic, ethnic_col) %>%
  select(record_id) %>%
  # Specify as none
  mutate(ethnic_p = rep("none", 2)) %>%
  # Retain all participants
  bind_rows(ethnic_col)
ethnic_col_1

# Bind ethnicity column
redcap_demo_4 <- redcap_demo_3 %>%
  select(-starts_with("eth_race__")) %>%
  left_join(ethnic_col_1) %>%
  select(record_id:eth_race_other, ethnic_p, everything())
redcap_demo_4

# Other category too small, so keep as other
redcap_demo_4 %>%
  filter(!is.na(eth_race_other) & ethnic_p == "Other")

# Drop other ethnicity column
redcap_demo_5 <- redcap_demo_4 %>%
  select(-eth_race_other)

# SEXUAL ORIENTATION ------------------------------------------------------

unique(redcap_demo_5$sex_orient_other)
