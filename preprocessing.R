
# Data engineering script (for ML Project)

## This script...
## - removes irrelevant variables
## - cleans data: reassigning missing values, adopting scales
## - loads the "PopuList" und crosswalk data and uses it to translate voted parties into populist (yes/no) variable
## - uses the package `wbstats` to add additional variables: gini-idx, unemployment rate and gdp
## - performs NA Imputation via the `missRanger` package


# STEP 1: load packages and the main dataset (ESS) -------------------

library(tidyverse) 
library(sjlabelled)
library(haven)
library(dplyr)
library(wbstats)
library(missRanger) 
library(fastDummies) 

ess_data <- read.csv("data/ESS11MD_e01_2.csv")
str(ess_data) 

# STEP 2: select relevant variables ------------
df_selected <- ess_data %>%
  select(
    # ID, Country & Weights (meta vars) ---
    idno, cntry, anweight, pspwght,
    
    # Features (Education vars) ---
    eisced,    # Categorical Education
    eduyrs,    # Years of Education
    
    # Features (Economic Insecurity) ---
    hincfel,   # Feeling about income 
    hinctnta,  # household income decile
    uemp12m,   # Unemployment experience
    gincdif,   # views on income inequality
    pdwrk,     # Currently working (Yes/No )
    
    # Features (political trust) ---
    trstplt,   # Trust in politicians
    trstprt,   # Trust in parties
    trstep,    # Trust in EU
    trstun,    # Trust in the united nations
    stfdem,    # Satisfaction with democracy
    
    # Features (Nationalism attitudes) ---
    imueclt,   # Cultural life enriched/undermined by immigrants
    imwbcnt,   # immigration attitudes
    imbgeco,   # immigration attitudes
    feethngr,  # feel part fo same ethic group as most ppl in country
    brncntr,   # Born in country
    
    # additional features/controls ---
    ppltrst,   # trust in ppl
    nwspol,    # Minutes watching news 
    netusoft,  # Internet usage frequency
    rlgdgr,    # Religiosity
    aesfdrk,   # Feeling of safety 
    health,    # subjective health
    happy,
    
    # Features (Demographics) ---
    gndr,      # Gender
    agea,      # Age
    domicil,   # Urban/Rural
    lrscale,   # Left-Right Scale
    
    # country specific voting variable (for matching via PopuList) ---
    starts_with("prtvt") 
  )


# STEP 3: add additional country variables ------
# build Gini-Index, GDP and unemployment rate Variable using the wbstats package
my_indicators <- c(
  gdp_capita = "NY.GDP.PCAP.PP.KD",  # GDP per capita 
  gini_index = "SI.POV.GINI",        # Gini Index 
  unemp_rate = "SL.UEM.TOTL.ZS"      # Unemployment Rate
)

# Fetch most resent data 
mmacro_data <- wb_data(
  country = "countries_only", 
  indicator = my_indicators, 
  mrv = 5   
) %>%
  select(iso2c, gdp_capita, gini_index, unemp_rate) %>%
  rename(cntry = iso2c) %>%
  distinct(cntry, .keep_all = TRUE)

# keep only those countries that are actually in the survey
ess_countries <- unique(df_prep$cntry)
macro_relevant <- macro_data %>%
  filter(cntry %in% ess_countries)

colSums(is.na(macro_relevant))

# Check for countries with NAs in the gini column
missing_countries <- macro_relevant %>%
  filter(is.na(gini_index)) %>%
  select(cntry)

print(missing_countries)

# Manually fill missing Gini values 
macro_relevant <- macro_relevant %>%
  mutate(gini_index = case_when(
    cntry == "GB" ~ 35.5,
    cntry == "HU" ~ 27.2,
    cntry == "IS" ~ 23.2,
    cntry == "IL" ~ 37.9,
    cntry == "NI" ~ 26.3,
    
    TRUE ~ gini_index
  ))

#  Merge into ESS data
df_selected <- df_selected %>%
  left_join(macro_relevant, by = "cntry")


# STEP 4: Join with Populist and Crosswalk Data to create 
# the dependent variable: is_populist_voter ----------

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

## watch out for dates (check which parties arent populist anymore)
#hist(populist_list$populist_end) 
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
  select(-c(populist, partyfacts_id))
head(ess_classified)


# STEP 5: Data cleaning ------------------------

# Function to fix ESS codes (77, 88, 99 -> NA)
clean_ess_codes <- function(x) {
  # For 0-10 scales
  if(is.numeric(x) && max(x, na.rm=TRUE) > 10) {
    x[x %in% c(66, 77, 88, 99)] <- NA
  }
  return(x)
}

names(ess_classified)
str(ess_classified)

df_clean <- ess_classified %>%
  mutate(
    # Scale specific cleaning
    
    eisced = replace(eisced, eisced %in% c(0, 55, 77, 88, 99), NA),
    gndr = replace(gndr, gndr == 9, NA),
    agea = replace(agea, agea == 999, NA),

    across(c(uemp12m, gincdif, hincfel, feethngr, brncntr, netusoft,
             aesfdrk, domicil, health),
           ~ replace (., . %in% c(7,8,9), NA)),
    
    across(c(eduyrs, trstplt, trstprt, trstep, trstun, stfdem, imueclt, imbgeco, 
             rlgdgr, ppltrst, happy, hinctnta, lrscale), 
           ~ replace(., . %in% c(77, 88, 99), NA)),
    
    nwspol = replace(nwspol, nwspol %in% c(7777, 8888, 9999), NA)
    
  )

# counting remaining NA's and check for weird numbers in the data
lapply(df_clean, function(x) sum(is.na(x)))
lapply(df_clean, function(x) summary(x))

# STEP 6: NA Imputation ----------

# Setup the dataset for Imputation
df_prep <- df_clean %>%
  # dropping trstep variable
  select(-trstep) %>%
  
  # dropping technical variables that shouldn't be used to predict missingness
  select(-c(idno, anweight, pspwght, vote_code, idno, anweight, pspwght)) %>% 
  
  # Convert Character strings to Factors (Required for Random Forest)
  mutate(
    cntry = as.factor(cntry),
    is_populist_voter = as.factor(is_populist_voter) 
  )

summary(df_prep$hinctnta)

# run the Imputation
df_imputed <- missRanger(
  df_prep, 
  formula = . ~ ., 
  num.trees = 50,  # 50 for speed (sufficient enough for imputation)
  verbose = 1,     
  seed = 100       
)

# check if it worked
sum(is.na(df_imputed))

## STEP 7: last preprocessing steps ------
# transforming country variable --> one-hot encoded dummys
# transforming populist variable back to numeric

df_final_ml <- df_imputed %>%
  dummy_cols(select_columns = "cntry", 
             remove_first_dummy = TRUE, 
             remove_selected_columns = TRUE) %>%
  
  # Convert Target back to Numeric 0/1
  mutate(is_populist_voter = as.numeric(as.character(is_populist_voter)))

str(df_final_ml)
View(df_final_ml)

# STEP 8: export --------
# Reference file including meta data: ID and Country 
# exclude those meta variables in the actual training data set

meta_data <- df_clean %>%
  select(idno, anweight, pspwght, cntry)

df_final_storage <- bind_cols(meta_data, df_final_ml)

df_final_storage <- df_final_storage %>%
  relocate(idno, cntry, is_populist_voter, .before = everything())

write_rds(df_final_storage, "data/ESS11_ReferenceDS_including_meta.rds")

# Final file for ML Analysis:

write_csv(df_final_ml, "final_data/ESS11_ML_Ready.csv")
print(paste("Final dataset has", ncol(df_final_ml), "variables and", nrow(df_final_ml), "respondents."))
print(paste("raw dataset had", ncol(ess_data), "variables and", nrow(ess_data), "respondents."))

# 24667 observations left (lost 46162 - 24667 = around 47% when excluding invalid voters)

