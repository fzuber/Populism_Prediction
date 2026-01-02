
# Data engineering script (for ML Project)

## This script...
## - removes irrelevant variables
## - cleans data: removing NA'S; adopting scales
## - Preprocesses ESS data
## - loads the "PopuList" und crosswalk data and uses it to translate voted partys into populist (yes/no) variable

##TODO: na replacement with machine learning approach
##TODO:skalen überprüfen (mit codebook)
##TODO: Kürzel der gewählten partei inkludieren (um zu überprüfen)
##TODO:decide between excluding ppl without voting preference or assign them as non-populist voters


# STEP 1: load packages and the main dataset (ESS) -------------------

library(tidyverse)
library(sjlabelled) 
library(haven)

ess_data <- read.csv("data/ESS11MD_e01_2.csv")
str(ess_data) 

# STEP 2: select relevant variables ------------
df_selected <- ess_data %>%
  select(
    # ID & Weights (meta variables) ---
    idno, cntry, anweight, pspwght,
    
    # Features (Education vars) ---
    eisced,    # Categorical Education
    eduyrs,    # Years of Education
    
    # Features (Economic Insecurity) ---
    hincfel,   # Feeling about income 
    hinctnta,  # household income decile
    uemp12m,   # Unemployment experience
    gincdif,   # views on income inequality
    pdwrk,     # Currently working (Yes/No)
    
    # Features (political trust) ---
    trstplt,   # Trust in politicians
    trstprt,   # Trust in parties
    trstep,    # Trust in EU
    trstun,    # Trust in institutions
    stfdem,    # Satisfaction with democracy
    
    # Features (Nationalism attitudes) ---
    imueclt,   # Cultural threat
    imwbcnt,   # immigration attitudes
    imbgeco,   # immigration attitudes
    feethngr,  # feel part fo same ethic group as most ppl in country
    brncntr,   # Born in country
    
    # additional features/controls ---
    ppltrst,   # trust in ppl
    nwspol,    # Minutes watching news (Media exposure)
    netusoft,  # Internet usage frequency
    rlgdgr,    # Religiosity
    aesfdrk,   # Feeling of safety (Fear of crime)
    # uplconf,   # "Politics is too complicated" 
    
    # Features (Demographics) ---
    gndr,      # Gender
    agea,      # Age
    domicil,   # Urban/Rural
    lrscale,   # Left-Right Scale
    
    # country specific voting variable (for matching via PopuList) ---
    starts_with("prtvt") 
    
  )
#str(df_voting)

# STEP 3: Join with Populist and Crosswalk Data --------------
#         to create populist variable

# A. Create 'vote_code' variable ---
df_voting <- df_selected %>%
  mutate(
    # Squash 30 columns into 1
    # coalesce() takes the first non-missing value it finds in these columns
    vote_code = coalesce(!!!select(., starts_with("prtvt")))
  ) %>%
    select(-starts_with("prtvt"))

# drop people that did not answer (ess codes 66, 77,88,99)
df_voting <- df_voting %>%
  mutate(vote_code = as.numeric(vote_code)) %>%
  filter(!vote_code %in% c(66, 77, 88, 99)) %>%
  filter(!is.na(vote_code)) 

sort(unique(df_voting$vote_code))

# B. Loading and investigating data ---
## Load "The PopuList" 
populist_list <- read_delim("data/The PopuList 3.0.csv", delim = ";")

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

## Load the "Crosswalk"
ess_crosswalk <- read_csv("data/ess-partyfacts-extended-added.csv") %>%
  filter(!is.na(partyfacts_id)) %>%
  select(cntry, ess_id, partyfacts_id) 
glimpse(ess_crosswalk)

# C. Preparing all 3 dataframes for the merge ---
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

# D Merging  ----
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

## check: table of sum of populist/non-populist votes per country
table(ess_classified$cntry, ess_classified$is_populist_voter)
head(ess_classified, 30)

#cleaning up dublicated columns
ess_classified <- ess_classified %>% 
  select(-c(populist, partyfacts_id, vote_code))
head(ess_classified)

# STEP 3: Data cleaning ------------------------
# Function to fix ESS codes (77, 88, 99 -> NA)
clean_ess_codes <- function(x) {
  # For 0-10 scales
  if(is.numeric(x) && max(x, na.rm=TRUE) > 10) {
    x[x %in% c(66, 77, 88, 99)] <- NA
  }
  return(x)
}

df_clean <- ess_classified %>%
  mutate(
    # 1. Apply generic cleaning to 0-10 scales
    across(c(hincfel, gincdif, trstplt, trstprt, trstep, stfdem, 
             imueclt, imwbcnt, imbgeco, lrscale, agea,
             ppltrst, rlgdgr, aesfdrk, uplconf), 
           ~ replace(., . %in% c(66, 77, 88, 99), NA)),
    
    # 2. Clean Categorical Specifics
    eisced = replace(eisced, eisced %in% c(0, 55, 77, 88, 99), NA),
    gndr = replace(gndr, gndr == 9, NA),
    domicil = replace(domicil, domicil %in% c(7, 8, 9), NA),
    brncntr = replace(brncntr, brncntr %in% c(7, 8, 9), NA),
    health = replace(health, health %in% c(7, 8, 9), NA)
  )

# counting remaining NA's
lapply(df_clean, function(x) sum(is.na(x)))
lapply(df_clean, function(x) summary(x))

# step 4: Pre-Processing for ML -------

df_final <- df_clean %>%
  # Convert Binary/Categorical to Factors or 0/1 for ML
  mutate(
    cntry = as.factor(cntry),
    
    # Gender: 0=Male, 1=Female (Standard ML practice)
    gndr_bin = ifelse(gndr == 2, 1, 0), 
    
    # Born in country: 1=Yes, 0=No
    born_native = ifelse(brncntr == 1, 1, 0),
    
    # Domicil: Reverse it so High Number = Urban? 
    # Currently 1=Big City, 5=Farm. Let's keep it but treat as numeric 
    # or factor depending on model. Let's leave as numeric for Trees.
    
    # Income Insecurity: 
    # hincfel is 1 (Comfortable) to 4 (Difficult).
    # Let's make it intuitive: "Financial Stress Score"
    # No change needed, just remember: High Value = High Stress.
  ) %>%
  # Drop original raw categorical columns if you created binary versions
  select(-gndr, -brncntr) %>% 
  # Drop rows with NAs (ML models cannot handle NAs usually)
  drop_na()


# step 5: export --------

write_csv(df_final, "final_data/ESS11_Expanded_ML_Ready.csv")
print(paste("Final dataset has", ncol(df_final), "variables and", nrow(df_final), "respondents."))
