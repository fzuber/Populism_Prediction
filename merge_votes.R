
#library(tidyverse)
#library(haven)

# Load "The PopuList" (Classifications)
populist_list <- read_delim("data/The PopuList 3.0.csv", delim = ";")

# investigating PopuList Dataset to gain knowledge for the Merging Process later
## watch out for dates (check which partys arent populist anymore)
hist(populist_list$populist_end) 
populist_list %>%
  filter(populist_end != "2100")

## check for dublicates
duplicates <- populist_list %>%
  group_by(partyfacts_id) %>%
  filter(n() > 1) %>%
  arrange(partyfacts_id) %>% # Sort so duplicates sit next to each other
  select(party_name, partyfacts_id, populist, populist_start, populist_end)
#View(duplicates)
  
# Load the "Crosswalk"
ess_crosswalk <- read_csv("data/ess-partyfacts-extended-added.csv") %>%
  filter(!is.na(partyfacts_id)) %>%
  select(cntry, ess_id, partyfacts_id) 
glimpse(ess_crosswalk)

# Preparing all 3 dataframes for the merge---
## Prepare ESS Data
ess_ready <- df_voting %>%
  mutate(vote_code = as.numeric(vote_code))
## prepare crosswalk
ess_crosswalk_clean <- ess_crosswalk %>%
  select(cntry, ess_id, partyfacts_id) %>%
  distinct(cntry, ess_id, .keep_all = TRUE)
## clean and dedublicate the PopuList data
populist_clean <- populist_list %>%
  filter(populist == 1) %>% 
  filter(populist_end >= 2019) %>%
  filter(!is.na(partyfacts_id)) %>% 
  distinct(partyfacts_id, .keep_all = TRUE) %>%
  select(partyfacts_id, populist)

# Merging  ---
## Join 1: ESS -> Crosswalk
ess_with_id <- ess_ready %>%
  left_join(ess_crosswalk_clean, by = c("cntry" = "cntry", "vote_code" = "ess_id"))

## Join 2: Linked -> PopuList
ess_classified <- ess_with_id %>%
  left_join(populist_clean, by = "partyfacts_id") %>%
  mutate(
    is_populist_voter = ifelse(populist == 1, 1, 0),
    is_populist_voter = replace_na(is_populist_voter, 0)
  )

# check: table of sum of populist/non-populist votes per country
table(ess_classified$cntry, ess_classified$is_populist_voter)
head(ess_classified)

